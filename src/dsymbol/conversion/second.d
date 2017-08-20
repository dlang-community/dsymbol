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

module dsymbol.conversion.second;

import dsymbol.semantic;
import dsymbol.string_interning;
import dsymbol.symbol;
import dsymbol.scope_;
import dsymbol.builtin.names;
import dsymbol.builtin.symbols;
import dsymbol.type_lookup;
import dsymbol.deferred;
import dsymbol.import_;
import dsymbol.modulecache;
import containers.unrolledlist;
import std.experimental.allocator;
import std.experimental.allocator.mallocator;
import std.experimental.logger;
import dparse.ast;
import dparse.lexer;

void secondPass(SemanticSymbol* currentSymbol, Scope* moduleScope, ref ModuleCache cache)
{
	with (CompletionKind) final switch (currentSymbol.acSymbol.kind)
	{
	case className:
	case interfaceName:
		resolveInheritance(currentSymbol.acSymbol, currentSymbol.typeLookups,
			moduleScope, cache);
		break;
	case withSymbol:
	case variableName:
	case memberVariableName:
	case functionName:
	case aliasName:
		// type may not be null in the case of a renamed import
		if (currentSymbol.acSymbol.type is null)
		{
			resolveType(currentSymbol.acSymbol, currentSymbol.typeLookups,
				moduleScope, cache);
		}
		break;
	case importSymbol:
		if (currentSymbol.acSymbol.type is null)
			resolveImport(currentSymbol.acSymbol, currentSymbol.typeLookups, cache);
		break;
	case structName:
	case unionName:
	case enumName:
	case keyword:
	case enumMember:
	case packageName:
	case moduleName:
	case dummy:
	case templateName:
	case mixinTemplateName:
		break;
	}

	foreach (child; currentSymbol.children)
		secondPass(child, moduleScope, cache);

	// Alias this and mixin templates are resolved after child nodes are
	// resolved so that the correct symbol information will be available.
	with (CompletionKind) switch (currentSymbol.acSymbol.kind)
	{
	case className:
	case interfaceName:
	case structName:
	case unionName:
		resolveAliasThis(currentSymbol.acSymbol, currentSymbol.typeLookups, cache);
		resolveMixinTemplates(currentSymbol.acSymbol, currentSymbol.typeLookups,
			moduleScope, cache);
		break;
	default:
		break;
	}
}

void resolveImport(DSymbol* acSymbol, ref UnrolledList!(TypeLookup*, Mallocator, false) typeLookups,
	ref ModuleCache cache)
in
{
	assert (acSymbol.kind == CompletionKind.importSymbol);
}
body
{
	if (acSymbol.qualifier == SymbolQualifier.selectiveImport)
	{
		assert(acSymbol.symbolFile !is null);
		DSymbol* moduleSymbol = cache.cacheModule(acSymbol.symbolFile);
		if (moduleSymbol is null)
		{
		tryAgain:
			DeferredSymbol* deferred = Mallocator.instance.make!DeferredSymbol(acSymbol);
			deferred.typeLookups.insert(typeLookups[]);
			// Get rid of the old references to the lookups, this new deferred
			// symbol owns them now
			typeLookups.clear();
			cache.deferredSymbols.insert(deferred);
		}
		else
		{
			immutable size_t breadcrumbCount = typeLookups.front.breadcrumbs.length;
			assert(breadcrumbCount <= 2 && breadcrumbCount > 0, "Malformed selective import");

			istring symbolName = typeLookups.front.breadcrumbs.front;
			DSymbol* selected = moduleSymbol.getFirstPartNamed(symbolName);
			if (acSymbol is null)
				goto tryAgain;
			acSymbol.type = selected;
			acSymbol.ownType = false;

			// count of 1 means selective import
			// count of 2 means a renamed selective import
			if (breadcrumbCount == 2)
			{
				acSymbol.kind = CompletionKind.aliasName;
				acSymbol.symbolFile = acSymbol.altFile;
			}
		}
	}
	else
	{
		assert(acSymbol.symbolFile !is null);
		DSymbol* moduleSymbol = cache.cacheModule(acSymbol.symbolFile);
		if (moduleSymbol is null)
		{
			DeferredSymbol* deferred = Mallocator.instance.make!DeferredSymbol(acSymbol);
			cache.deferredSymbols.insert(deferred);
		}
		else
		{
			acSymbol.type = moduleSymbol;
			acSymbol.ownType = false;
		}
	}
}

void resolveTypeFromType(DSymbol* symbol, TypeLookup* lookup, Scope* moduleScope,
	ref ModuleCache cache, UnrolledList!(DSymbol*, Mallocator, false)* imports)
in
{
	if (imports !is null)
		foreach (i; imports.opSlice())
			assert(i.kind == CompletionKind.importSymbol);
}
body
{
	// The left-most suffix
	DSymbol* suffix;
	// The right-most suffix
	DSymbol* lastSuffix;

	// Create symbols for the type suffixes such as array and
	// associative array
	while (!lookup.breadcrumbs.empty)
	{
		auto back = lookup.breadcrumbs.back;
		immutable bool isArr = back == ARRAY_SYMBOL_NAME;
		immutable bool isAssoc = back == ASSOC_ARRAY_SYMBOL_NAME;
		immutable bool isFunction = back == FUNCTION_SYMBOL_NAME;
		immutable bool isPointer = back == POINTER_SYMBOL_NAME;
		if (!isArr && !isAssoc && !isFunction && !isPointer)
			break;
		immutable qualifier = isAssoc ? SymbolQualifier.assocArray :
			(isFunction ? SymbolQualifier.func :
				(isArr ? SymbolQualifier.array : SymbolQualifier.none));
		auto _prevSuffix = suffix;
		suffix = cache.symbolAllocator.make!DSymbol(back, CompletionKind.dummy);
		suffix.qualifier = qualifier;
		suffix.ownType = true;
		suffix.isPointer = isPointer;
		if (_prevSuffix)
			_prevSuffix.type = suffix;
		if (isFunction)
		{
			lookup.breadcrumbs.popBack();
			suffix.callTip = lookup.breadcrumbs.back();
		}
		else if (isArr)
			suffix.addChildren(arraySymbols[], false);
		else if (isAssoc)
			// TODO: resolve the type of the key
			suffix.addChildren(assocArraySymbols[], false);

		if (lastSuffix is null)
			lastSuffix = suffix;
		lookup.breadcrumbs.popBack();
	}

	UnrolledList!(DSymbol*, Mallocator, false) remainingImports;

	DSymbol* currentSymbol;

	void getSymbolFromImports(UnrolledList!(DSymbol*, Mallocator, false)* importList, istring name)
	{
		foreach (im; importList.opSlice())
		{
			assert(im.symbolFile !is null);
			// Try to find a cached version of the module
			DSymbol* moduleSymbol = cache.getModuleSymbol(im.symbolFile);
			// If the module has not been cached yet, store it in the
			// remaining imports list
			if (moduleSymbol is null)
			{
				remainingImports.insert(im);
				continue;
			}
			// Try to get the symbol from the imported module
			currentSymbol = moduleSymbol.getFirstPartNamed(name);
			if (currentSymbol is null)
				continue;
		}
	}

	// Follow all the names and try to resolve them
	size_t i = 0;
	foreach (part; lookup.breadcrumbs[])
	{
		if (i == 0)
		{
			if (moduleScope is null)
				getSymbolFromImports(imports, part);
			else
			{
				auto symbols = moduleScope.getSymbolsByNameAndCursor(part, symbol.location);
				if (symbols.length > 0)
					currentSymbol = symbols[0];
				else
					return;
			}
		}
		else
		{
			if (currentSymbol.kind == CompletionKind.aliasName)
				currentSymbol = currentSymbol.type;
			if (currentSymbol is null)
				return;
			if (currentSymbol.kind == CompletionKind.moduleName && currentSymbol.type !is null)
				currentSymbol = currentSymbol.type;
			if (currentSymbol is null)
				return;
			if (currentSymbol.kind == CompletionKind.importSymbol)
				currentSymbol = currentSymbol.type;
			if (currentSymbol is null)
				return;
			currentSymbol = currentSymbol.getFirstPartNamed(part);
		}
		++i;
		if (currentSymbol is null)
			return;
	}

	if (lastSuffix !is null)
	{
		assert(suffix !is null);
		suffix.type = currentSymbol;
		suffix.ownType = false;
		symbol.type = lastSuffix;
		symbol.ownType = true;
		if (currentSymbol is null && !remainingImports.empty)
		{
//			info("Deferring type resolution for ", symbol.name);
			auto deferred = Mallocator.instance.make!DeferredSymbol(suffix);
			// TODO: The scope has ownership of the import information
			deferred.imports.insert(remainingImports[]);
			deferred.typeLookups.insert(lookup);
			cache.deferredSymbols.insert(deferred);
		}
	}
	else if (currentSymbol !is null)
	{
		symbol.type = currentSymbol;
		symbol.ownType = false;
	}
	else if (!remainingImports.empty)
	{
		auto deferred = Mallocator.instance.make!DeferredSymbol(symbol);
//		info("Deferring type resolution for ", symbol.name);
		// TODO: The scope has ownership of the import information
		deferred.imports.insert(remainingImports[]);
		deferred.typeLookups.insert(lookup);
		cache.deferredSymbols.insert(deferred);
	}
}

unittest
{
	import dsymbol.tests : generateAutocompleteTrees;
	import std.stdio : writeln;

	ModuleCache cache = ModuleCache(theAllocator);
	{
		auto pair = generateAutocompleteTrees(q{uint[][] i;}, cache);
		auto sym = pair.symbol.getFirstPartNamed(internString("i"));
		assert(sym.type.name is ARRAY_SYMBOL_NAME);
		assert(sym.type.type.name is ARRAY_SYMBOL_NAME);
		assert(sym.type.type.type.name is internString("uint"));
	}

	{
		auto pair = generateAutocompleteTrees(q{byte[int][] k;}, cache);
		auto sym = pair.symbol.getFirstPartNamed(internString("k"));
		assert(sym.type.name is ARRAY_SYMBOL_NAME);
		assert(sym.type.type.name is ASSOC_ARRAY_SYMBOL_NAME);
		assert(sym.type.type.type);
		assert(sym.type.type.type.name is internString("byte"));
	}

	{
		auto pair = generateAutocompleteTrees(q{double* j;}, cache);
		auto sym = pair.symbol.getFirstPartNamed(internString("j"));
		assert(sym.type.isPointer);
		assert(sym.type.type.name is internString("double"));
	}

	{
		auto pair = generateAutocompleteTrees(q{const(float)*[] k;}, cache);
		auto sym = pair.symbol.getFirstPartNamed(internString("k"));
		assert(sym.type.name is ARRAY_SYMBOL_NAME);
		assert(sym.type.type.isPointer);
		assert(sym.type.type.type.name is internString("float"));
	}

	{
		auto pair = generateAutocompleteTrees(q{int[dstring[]] aa;}, cache);
		auto sym = pair.symbol.getFirstPartNamed(internString("aa"));
		assert(sym.type.name is ASSOC_ARRAY_SYMBOL_NAME);
		assert(sym.type.type.name is internString("int"));
		// FIXME: check the key type
	}
}

private:

void resolveInheritance(DSymbol* symbol, ref UnrolledList!(TypeLookup*, Mallocator, false) typeLookups,
	Scope* moduleScope, ref ModuleCache cache)
{
	import std.algorithm : filter;

	outer: foreach (TypeLookup* lookup; typeLookups[])
	{
		if (lookup.kind != TypeLookupKind.inherit)
			continue;
		DSymbol* baseClass;
		assert(lookup.breadcrumbs.length > 0);

		// TODO: Delayed type lookup
		auto symbolScope = moduleScope.getScopeByCursor(symbol.location);
		auto symbols = moduleScope.getSymbolsByNameAndCursor(lookup.breadcrumbs.front,
			symbol.location);
		if (symbols.length == 0)
			continue;

		baseClass = symbols[0];
		lookup.breadcrumbs.popFront();
		foreach (part; lookup.breadcrumbs[])
		{
			symbols = baseClass.getPartsByName(part);
			if (symbols.length == 0)
				continue outer;
			baseClass = symbols[0];
		}

		static bool shouldSkipFromBase(const DSymbol* d) pure nothrow @nogc
		{
			if (d.name.ptr == CONSTRUCTOR_SYMBOL_NAME.ptr)
				return false;
			if (d.name.ptr == DESTRUCTOR_SYMBOL_NAME.ptr)
				return false;
			if (d.name.ptr == UNITTEST_SYMBOL_NAME.ptr)
				return false;
			if (d.name.ptr == THIS_SYMBOL_NAME.ptr)
				return false;
			if (d.kind == CompletionKind.keyword)
				return false;
			return true;
		}

		// TODO: This will not work with symbol replacement and cache invalidation
		foreach (_; baseClass.opSlice().filter!shouldSkipFromBase())
		{
			symbol.addChild(_, false);
			symbolScope.addSymbol(_, false);
		}
		if (baseClass.kind == CompletionKind.className)
		{
			auto s = cache.symbolAllocator.make!DSymbol(SUPER_SYMBOL_NAME,
				CompletionKind.variableName, baseClass);
			symbolScope.addSymbol(s, true);
		}
	}
}

void resolveAliasThis(DSymbol* symbol,
	ref UnrolledList!(TypeLookup*, Mallocator, false) typeLookups, ref ModuleCache cache)
{
	import std.algorithm : filter;

	foreach (aliasThis; typeLookups[].filter!(a => a.kind == TypeLookupKind.aliasThis))
	{
		assert(aliasThis.breadcrumbs.length > 0);
		auto parts = symbol.getPartsByName(aliasThis.breadcrumbs.front);
		if (parts.length == 0 || parts[0].type is null)
			continue;
		DSymbol* s = cache.symbolAllocator.make!DSymbol(IMPORT_SYMBOL_NAME,
			CompletionKind.importSymbol, parts[0].type);
		symbol.addChild(s, true);
	}
}

void resolveMixinTemplates(DSymbol* symbol,
	ref UnrolledList!(TypeLookup*, Mallocator, false) typeLookups, Scope* moduleScope, ref ModuleCache cache)
{
	import std.algorithm : filter;

	foreach (mix; typeLookups[].filter!(a => a.kind == TypeLookupKind.mixinTemplate))
	{
		assert(mix.breadcrumbs.length > 0);
		auto symbols = moduleScope.getSymbolsByNameAndCursor(mix.breadcrumbs.front,
			symbol.location);
		if (symbols.length == 0)
			continue;
		auto currentSymbol = symbols[0];
		mix.breadcrumbs.popFront();
		foreach (m; mix.breadcrumbs[])
		{
			auto s = currentSymbol.getPartsByName(m);
			if (s.length == 0)
			{
				currentSymbol = null;
				break;
			}
			else
				currentSymbol = s[0];
		}
		if (currentSymbol !is null)
		{
			auto i = cache.symbolAllocator.make!DSymbol(IMPORT_SYMBOL_NAME,
				CompletionKind.importSymbol, currentSymbol);
			i.ownType = false;
			symbol.addChild(i, true);
		}
	}
}

void resolveType(DSymbol* symbol, ref UnrolledList!(TypeLookup*, Mallocator, false) typeLookups,
	Scope* moduleScope, ref ModuleCache cache)
{
	if (typeLookups.length == 0)
		return;
	assert(typeLookups.length == 1);
	auto lookup = typeLookups.front;
	if (lookup.kind == TypeLookupKind.varOrFunType)
		resolveTypeFromType(symbol, lookup, moduleScope, cache, null);
	else if (lookup.kind == TypeLookupKind.initializer)
		resolveTypeFromInitializer(symbol, lookup, moduleScope, cache);
	else
		assert(false, "How did this happen?");
}


void resolveTypeFromInitializer(DSymbol* symbol, TypeLookup* lookup,
	Scope* moduleScope, ref ModuleCache cache)
{
	if (lookup.breadcrumbs.length == 0)
		return;
	DSymbol* currentSymbol = null;
	size_t i = 0;

	auto crumbs = lookup.breadcrumbs[];
	foreach (crumb; crumbs)
	{
		if (i == 0)
		{
			currentSymbol = moduleScope.getFirstSymbolByNameAndCursor(
				symbolNameToTypeName(crumb), symbol.location);

			// solves auto arrays
			if (crumb == ARRAY_SYMBOL_NAME)
			{
				auto nestedArr = crumbs.save();
				auto a = nestedArr.front();

				DSymbol* suffix;
				DSymbol* lastSuffix;

				// process the flags set in ArrayInitializer visit
				while (true)
				{
					lastSuffix = cache.symbolAllocator.make!(DSymbol)(a, CompletionKind.dummy, lastSuffix);
					lastSuffix.qualifier = SymbolQualifier.array;

					if (suffix is null)
						suffix = lastSuffix;

					nestedArr.popFront();
					if (nestedArr.empty())
						break;
					a = nestedArr.front();
					if (a != ARRAY_SYMBOL_NAME)
						break;
				}

				// last crumb should be the element type
				DSymbol* elemType;
				if (!nestedArr.empty)
				{
					suffix.addChildren(arraySymbols[], false);
					elemType = moduleScope.getFirstSymbolByNameAndCursor(
						symbolNameToTypeName(a), symbol.location);
				}

				// put the elem type to the back of the *arr* chain
				if (suffix !is null && elemType)
				{
					suffix.type = elemType;
					suffix.ownType = false;
					symbol.type = lastSuffix;
					symbol.ownType = true;
				}
			}
			if (currentSymbol is null)
				return;
		}
		else if (crumb == ARRAY_SYMBOL_NAME)
		{
			typeSwap(currentSymbol);
			if (currentSymbol is null)
				return;

			// Index expressions can be an array index or an AA index
			if (currentSymbol.qualifier == SymbolQualifier.array
					|| currentSymbol.qualifier == SymbolQualifier.assocArray
					|| currentSymbol.kind == CompletionKind.aliasName)
			{
				if (currentSymbol.type !is null)
					currentSymbol = currentSymbol.type;
				else
					return;
			}
			else
			{
				auto opIndex = currentSymbol.getFirstPartNamed(internString("opIndex"));
				if (opIndex !is null)
					currentSymbol = opIndex.type;
				else
					return;
			}
		}
		else if (crumb == "foreach")
		{
			typeSwap(currentSymbol);
			if (currentSymbol is null)
				return;
			if (currentSymbol.qualifier == SymbolQualifier.array
					|| currentSymbol.qualifier == SymbolQualifier.assocArray)
			{
				currentSymbol = currentSymbol.type;
				break;
			}
			auto front = currentSymbol.getFirstPartNamed(internString("front"));
			if (front !is null)
			{
				currentSymbol = front.type;
				break;
			}
			auto opApply = currentSymbol.getFirstPartNamed(internString("opApply"));
			if (opApply !is null)
			{
				currentSymbol = opApply.type;
				break;
			}
		}
		else
		{
			typeSwap(currentSymbol);
			if (currentSymbol is null )
				return;
			currentSymbol = currentSymbol.getFirstPartNamed(crumb);
		}
		++i;
		if (currentSymbol is null)
			return;
	}
	typeSwap(currentSymbol);
	symbol.type = currentSymbol;
	symbol.ownType = false;
}

void typeSwap(ref DSymbol* currentSymbol)
{
	while (currentSymbol !is null && currentSymbol.type !is currentSymbol
			&& (currentSymbol.kind == CompletionKind.variableName
			|| currentSymbol.kind == CompletionKind.importSymbol
			|| currentSymbol.kind == CompletionKind.withSymbol
			|| currentSymbol.kind == CompletionKind.aliasName))
		currentSymbol = currentSymbol.type;
}
