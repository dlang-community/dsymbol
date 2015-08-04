/**
 * This file is part of DCD, a development tool for the D programming language.
 * Copyright (C) 2014 Brian Schott
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

module dsymbol.modulecache;

import containers.dynamicarray;
import containers.hashset;
import containers.ttree;
import containers.unrolledlist;
import dsymbol.conversion;
import dsymbol.conversion.first;
import dsymbol.conversion.third;
import dsymbol.cache_entry;
import dsymbol.scope_;
import dsymbol.semantic;
import dsymbol.symbol;
import dsymbol.string_interning;
import dsymbol.deferred;
import memory.allocators;
import std.algorithm;
import std.experimental.allocator;
import std.experimental.allocator.building_blocks.allocator_list;
import std.experimental.allocator.building_blocks.region;
import std.experimental.allocator.mallocator;
import std.conv;
import std.d.ast;
import std.datetime;
import std.d.lexer;
import std.d.parser;
import std.experimental.logger;
import std.file;
import std.lexer;
import std.path;

alias ASTAllocator = CAllocatorImpl!(AllocatorList!(
	n => Region!Mallocator(1024 * 64)));

/**
 * Returns: true if a file exists at the given path.
 */
bool existanceCheck(A)(A path)
{
	if (path.exists())
		return true;
	warning("Cannot cache modules in ", path, " because it does not exist");
	return false;
}

/**
 * Caches pre-parsed module information.
 */
struct ModuleCache
{
	/// No copying.
	@disable this(this);

	this(IAllocator symbolAllocator)
	{
		this.symbolAllocator = symbolAllocator;
	}

	~this()
	{
		foreach (entry; ModuleCache.cache[])
			typeid(CacheEntry).destroy(entry);
	}

	/**
	 * Adds the given path to the list of directories checked for imports.
	 * Performs duplicate checking, so multiple instances of the same path will
	 * not be present.
	 */
	void addImportPaths(const string[] paths)
	{
		import dsymbol.string_interning : internString;
		import std.path : baseName;
		import std.array : array;

		auto newPaths = paths.filter!(a => existanceCheck(a) && !importPaths[].canFind(a)).map!(
			internString).array;
		importPaths.insert(newPaths);

		foreach (path; newPaths[])
		{
			foreach (fileName; dirEntries(path, "*.{d,di}", SpanMode.depth))
			{
				if (fileName.baseName.startsWith(".#"))
					continue;
				cacheModule(fileName);
			}
		}
	}

	/// TODO: Implement
	void clear()
	{
		info("ModuleCache.clear is not yet implemented.");
	}

	/**
	 * Caches the module at the given location
	 */
	DSymbol* cacheModule(string location)
	{
		import dsymbol.string_interning : internString;
		import std.stdio : File;
		import std.typecons : scoped;

		assert (location !is null);

		istring cachedLocation = internString(location);


		if (!needsReparsing(cachedLocation))
			return getModuleSymbol(cachedLocation);

		recursionGuard.insert(cachedLocation);

		File f = File(cachedLocation);
		immutable fileSize = cast(size_t) f.size;
		if (fileSize == 0)
			return null;

		const(Token)[] tokens;
		auto parseStringCache = StringCache(StringCache.defaultBucketCount);
		{
			ubyte[] source = cast(ubyte[]) Mallocator.instance.allocate(fileSize);
			scope (exit) Mallocator.instance.deallocate(source);
			f.rawRead(source);
			LexerConfig config;
			config.fileName = cachedLocation;

			// The first three bytes are sliced off here if the file starts with a
			// Unicode byte order mark. The lexer/parser don't handle them.
			tokens = getTokensForParser(
				(source.length >= 3 && source[0 .. 3] == "\xef\xbb\xbf"c)
				? source[3 .. $] : source,
				config, &parseStringCache);
		}

		CacheEntry* newEntry = Mallocator.instance.make!CacheEntry();

		auto semanticAllocator = scoped!(ASTAllocator);
		Module m = parseModuleSimple(tokens[], cachedLocation, semanticAllocator);

		assert (symbolAllocator);
		auto first = scoped!FirstPass(m, cachedLocation, symbolAllocator,
			semanticAllocator, false, &this, newEntry);
		first.run();

		thirdPass(first.rootSymbol, first.moduleScope, this);

		typeid(Scope).destroy(first.moduleScope);
		symbolsAllocated += first.symbolsAllocated;

		SysTime access;
		SysTime modification;
		getTimes(cachedLocation, access, modification);

		newEntry.symbol = first.rootSymbol.acSymbol;
		newEntry.modificationTime = modification;
		newEntry.path = cachedLocation;

		CacheEntry* oldEntry = getEntryFor(cachedLocation);
		if (oldEntry !is null)
		{
			// Generate update mapping from the old symbol to the new one
			UpdatePairCollection updatePairs;
			generateUpdatePairs(oldEntry.symbol, newEntry.symbol, updatePairs);

			// Apply updates to all symbols in modules that depend on this one
			cache[].filter!(a => a.dependencies.contains(cachedLocation)).each!(
				upstream => upstream.symbol.updateTypes(updatePairs));

			// Remove the old symbol.
			cache.remove(oldEntry, entry => Mallocator.instance.dispose(entry));
		}

		cache.insert(newEntry);
		recursionGuard.remove(cachedLocation);

		resolveDeferredTypes(cachedLocation);

		return newEntry.symbol;
	}

	/**
	 * Resolves types for deferred symbols
	 */
	void resolveDeferredTypes(istring location)
	{
		UnrolledList!(DeferredSymbol*) temp;
		temp.insert(deferredSymbols[]);
		deferredSymbols.clear();
		foreach (deferred; temp[])
		{
			if (!deferred.imports.empty && !deferred.dependsOn(location))
			{
				deferredSymbols.insert(deferred);
				continue;
			}
			assert(deferred.symbol.type is null);
			if (deferred.symbol.kind == CompletionKind.importSymbol)
			{
				resolveImport(deferred.symbol, deferred.typeLookups, this);
				Mallocator.instance.dispose(deferred);
			}
			else if (!deferred.typeLookups.empty)
			{
				// TODO: Is .front the right thing to do here?
				resolveTypeFromType(deferred.symbol, deferred.typeLookups.front, null,
					this, &deferred.imports);
			}
		}
	}

	/**
	 * Params:
	 *     moduleName = the name of the module in "a/b/c" form
	 * Returns:
	 *     The symbols defined in the given module, or null if the module is
	 *     not cached yet.
	 */
	DSymbol* getModuleSymbol(istring location)
	{
		auto existing = getEntryFor(location);
		return existing is null ? null : existing.symbol;
	}

	/**
	 * Params:
	 *     moduleName = the name of the module being imported, in "a/b/c" style
	 * Returns:
	 *     The absolute path to the file that contains the module, or null if
	 *     not found.
	 */
	istring resolveImportLocation(string moduleName)
	{
		assert(moduleName !is null, "module name is null");
		if (isRooted(moduleName))
			return internString(moduleName);
		string[] alternatives;
		foreach (path; importPaths[])
		{
			string dotDi = buildPath(path, moduleName) ~ ".di";
			string dotD = dotDi[0 .. $ - 1];
			string withoutSuffix = dotDi[0 .. $ - 3];
			if (exists(dotD) && isFile(dotD))
				alternatives = dotD ~ alternatives;
			else if (exists(dotDi) && isFile(dotDi))
				alternatives ~= dotDi;
			else if (exists(withoutSuffix) && isDir(withoutSuffix))
			{
				string packagePath = buildPath(withoutSuffix, "package.di");
				if (exists(packagePath) && isFile(packagePath))
				{
					alternatives ~= packagePath;
					continue;
				}
				if (exists(packagePath[0 .. $ - 1]) && isFile(packagePath[0 .. $ - 1]))
					alternatives ~= packagePath[0 .. $ - 1];
			}
		}
		return alternatives.length > 0 ? internString(alternatives[0]) : istring(null);
	}

	auto getImportPaths()
	{
		return importPaths[];
	}

	auto getAllSymbols()
	{
		return cache[];
	}

	IAllocator symbolAllocator;

	UnrolledList!(DeferredSymbol*) deferredSymbols;

	/// Count of autocomplete symbols that have been allocated
	uint symbolsAllocated;

private:

	CacheEntry* getEntryFor(istring cachedLocation)
	{
		CacheEntry dummy;
		dummy.path = cachedLocation;
		auto r = cache.equalRange(&dummy);
		return r.empty ? null : r.front;
	}

	/**
	 * Params:
	 *     mod = the path to the module
	 * Returns:
	 *     true  if the module needs to be reparsed, false otherwise
	 */
	bool needsReparsing(istring mod)
	{
		if (recursionGuard.contains(mod))
			return false;
		if (!exists(mod))
			return true;
		CacheEntry e;
		e.path = mod;
		auto r = cache.equalRange(&e);
		if (r.empty)
			return true;
		SysTime access;
		SysTime modification;
		getTimes(mod, access, modification);
		return r.front.modificationTime != modification;
	}

	// Mapping of file paths to their cached symbols.
	TTree!(CacheEntry*) cache;

	HashSet!string recursionGuard;

	// Listing of paths to check for imports
	UnrolledList!string importPaths;
}
