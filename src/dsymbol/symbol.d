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

module dsymbol.symbol;

import std.experimental.allocator;
import std.experimental.allocator.mallocator : Mallocator;
import std.array;

import containers.ttree;
import containers.unrolledlist;
import containers.slist;
import containers.hashset;
import dparse.lexer;
import std.bitmanip;

import dsymbol.builtin.names;
import dsymbol.string_interning;
public import dsymbol.string_interning : istring;

import std.range : isOutputRange;

/**
 * Identifies the kind of the item in an identifier completion list
 */
enum CompletionKind : char
{
	/// Invalid completion kind. This is used internally and will never
	/// be returned in a completion response.
	dummy = '?',

	/// Import symbol. This is used internally and will never
	/// be returned in a completion response.
	importSymbol = '*',

	/// With symbol. This is used internally and will never
	/// be returned in a completion response.
	withSymbol = 'w',

	/// class names
	className = 'c',

	/// interface names
	interfaceName = 'i',

	/// structure names
	structName = 's',

	/// union name
	unionName = 'u',

	/// variable name
	variableName = 'v',

	/// member variable
	memberVariableName = 'm',

	/// keyword, built-in version, scope statement
	keyword = 'k',

	/// function or method
	functionName = 'f',

	/// enum name
	enumName = 'g',

	/// enum member
	enumMember = 'e',

	/// package name
	packageName = 'P',

	/// module name
	moduleName = 'M',

	/// alias name
	aliasName = 'l',

	/// template name
	templateName = 't',

	/// mixin template name
	mixinTemplateName = 'T'
}

/**
 * Returns: true if `kind` is something that can be returned to the client
 */
bool isPublicCompletionKind(CompletionKind kind) pure nothrow @safe @nogc
{
	return kind != CompletionKind.dummy && kind != CompletionKind.importSymbol
		&& kind != CompletionKind.withSymbol;
}


/**
 * Any special information about a variable declaration symbol.
 */
enum SymbolQualifier : ubyte
{
	/// None
	none,
	/// The symbol is an array
	array,
	/// The symbol is a associative array
	assocArray,
	/// The symbol is a function or delegate pointer
	func,
	/// Selective import
	selectiveImport,
}

/**
 * Autocompletion symbol
 */
struct DSymbol
{
public:

	/**
	 * Copying is disabled.
	 */
	@disable this();

	/// ditto
	@disable this(this);

	/// ditto
	this(istring name) /+nothrow+/ /+@safe+/
	{
		this.name = name;
	}

	/**
	 * Params:
	 *     name = the symbol's name
	 *     kind = the symbol's completion kind
	 */
	this(string name, CompletionKind kind) /+nothrow+/ /+@safe+/ /+@nogc+/
	{
		this.name = name is null ? istring(name) : internString(name);
		this.kind = kind;
	}

	/// ditto
	this(istring name, CompletionKind kind) /+nothrow+/ /+@safe+/ /+@nogc+/
	{
		this.name = name;
		this.kind = kind;
	}

	/**
	 * Params:
	 *     name = the symbol's name
	 *     kind = the symbol's completion kind
	 *     resolvedType = the resolved type of the symbol
	 */
	this(string name, CompletionKind kind, DSymbol* type)
	{
		this.name = name is null ? istring(name) : internString(name);
		this.kind = kind;
		this.type = type;
	}

	/// ditto
	this(istring name, CompletionKind kind, DSymbol* type)
	{
		this.name = name;
		this.kind = kind;
		this.type = type;
	}

	~this()
	{
		foreach (ref part; parts[])
		{
			if (part.owned)
			{
				assert(part.ptr !is null);
				typeid(DSymbol).destroy(part.ptr);
			}
			else
				part = null;
		}
		if (ownType)
			typeid(DSymbol).destroy(type);
	}

	int opCmp(ref const DSymbol other) const pure nothrow @trusted
	{
		// Compare the pointers because the strings have been interned.
		// Identical strings MUST have the same address
		int r = name.ptr > other.name.ptr;
		if (name.ptr < other.name.ptr)
			r = -1;
		return r;
	}

	bool opEquals(ref const DSymbol other) const pure nothrow @trusted
	{
		return other.name.ptr == this.name.ptr;
	}

	size_t toHash() const pure nothrow @trusted
	{
		return (cast(size_t) name.ptr) * 27_644_437;
	}

	/**
	 * Gets all parts whose name matches the given string.
	 */
	inout(DSymbol)*[] getPartsByName(istring name) inout
	{
		auto app = appender!(DSymbol*[])();
		HashSet!size_t visited;
		getParts(name, app, visited);
		return cast(typeof(return)) app.data;
	}

	inout(DSymbol)* getFirstPartNamed(this This)(istring name) inout
	{
		auto app = appender!(DSymbol*[])();
		HashSet!size_t visited;
		getParts(name, app, visited);
		return app.data.length > 0 ? cast(typeof(return)) app.data[0] : null;
	}

	/**
	 * Gets all parts and imported parts. Filters based on the part's name if
	 * the `name` argument is not null. Stores results in `app`.
	 */
	void getParts(OR)(istring name, ref OR app, ref HashSet!size_t visited,
			bool onlyOne = false) inout
		if (isOutputRange!(OR, DSymbol*))
	{
		import std.algorithm.iteration : filter;

		if (visited.contains(cast(size_t) &this))
			return;
		visited.insert(cast(size_t) &this);

		DSymbol p = DSymbol(IMPORT_SYMBOL_NAME);
		if (qualifier == SymbolQualifier.selectiveImport && type !is null
				&& (name is null ? true : type.name.ptr == name.ptr))
		{
			app.put(cast(DSymbol*) type);
			if (onlyOne)
				return;
		}
		else
		{
			if (name == "")
			{
				foreach (part; parts[].filter!(a => a.name != IMPORT_SYMBOL_NAME))
				{
					app.put(cast(DSymbol*) part);
					if (onlyOne)
						return;
				}
				foreach (im; parts.equalRange(SymbolOwnership(&p)))
					if (im.type !is null && !im.skipOver)
						im.type.getParts(name, app, visited, onlyOne);
			}
			else
			{
				DSymbol s = DSymbol(name);
				foreach (part; parts.equalRange(SymbolOwnership(&s)))
				{
					app.put(cast(DSymbol*) part);
					if (onlyOne)
						return;
				}
				if (name.ptr == CONSTRUCTOR_SYMBOL_NAME.ptr
						|| name.ptr == DESTRUCTOR_SYMBOL_NAME.ptr
						|| name.ptr == UNITTEST_SYMBOL_NAME.ptr
						|| name.ptr == THIS_SYMBOL_NAME.ptr)
					return;	// these symbols should not be imported
				foreach (im; parts.equalRange(SymbolOwnership(&p)))
					if (im.type !is null && !im.skipOver)
						im.type.getParts(name, app, visited, onlyOne);
			}
		}
	}

	/**
	 * Returns: a range over this symbol's parts and publicly visible imports
	 */
	inout(DSymbol)*[] opSlice(this This)() inout
	{
		auto app = appender!(DSymbol*[])();
		HashSet!size_t visited;
		getParts!(typeof(app))(internString(null), app, visited);
		return cast(typeof(return)) app.data;
	}

	void addChild(DSymbol* symbol, bool owns)
	{
		assert(symbol !is null);
		parts.insert(SymbolOwnership(symbol, owns));
	}

	void addChildren(R)(R symbols, bool owns)
	{
		foreach (symbol; symbols)
		{
			assert(symbol !is null);
			parts.insert(SymbolOwnership(symbol, owns));
		}
	}

	void addChildren(DSymbol*[] symbols, bool owns)
	{
		foreach (symbol; symbols)
		{
			assert(symbol !is null);
			parts.insert(SymbolOwnership(symbol, owns));
		}
	}

	/**
	 * Updates the type field based on the mappings contained in the given
	 * collection.
	 */
	void updateTypes(ref UpdatePairCollection collection)
	{
		auto r = collection.equalRange(UpdatePair(type, null));
		if (!r.empty)
			type = r.front.newSymbol;
		foreach (part; parts[])
			part.updateTypes(collection);
	}

	/**
	 * Symbols that compose this symbol, such as enum members, class variables,
	 * methods, parameters, etc.
	 */
	private TTree!(SymbolOwnership, Mallocator, true, "a < b", false) parts;

	/**
	 * DSymbol's name
	 */
	istring name;

	/**
	 * Calltip to display if this is a function
	 */
	istring callTip;

	/**
	 * Used for storing information for selective renamed imports
	 */
	alias altFile = callTip;

	/**
	 * Module containing the symbol.
	 */
	istring symbolFile;

	/**
	 * Documentation for the symbol.
	 */
	istring doc;

	/**
	 * The symbol that represents the type.
	 */
	// TODO: assert that the type is not a function
	DSymbol* type;

	/**
	 * Names of function arguments
	 */
	UnrolledList!(istring) argNames;

	private uint _location;

	/**
	 * DSymbol location
	 */
	size_t location() const pure nothrow @nogc @property
	{
		return _location;
	}

	void location(size_t location) pure nothrow @nogc @property
	{
		// If the symbol was declared in a file, assert that it has a location
		// in that file. Built-in symbols don't need a location.
		assert(symbolFile is null || location < uint.max);
		_location = cast(uint) location;
	}

	/**
	 * The kind of symbol
	 */
	CompletionKind kind;

	/**
	 * DSymbol qualifier
	 */
	SymbolQualifier qualifier;

	/**
	 * If true, this symbol owns its type and will free it on destruction
	 */
	// dfmt off
	mixin(bitfields!(bool, "ownType", 1,
		bool, "skipOver", 1,
		bool, "isPointer", 1,
		ubyte, "", 5));
	// dfmt on

}

struct UpdatePair
{
	int opCmp(ref const UpdatePair other) const
	{
		immutable size_t otherOld = cast(size_t) other.oldSymbol;
		immutable size_t thisOld = cast(size_t) this.oldSymbol;
		if (otherOld < thisOld)
			return -1;
		if (otherOld > thisOld)
			return 1;
		return 0;
	}

	DSymbol* oldSymbol;
	DSymbol* newSymbol;
}

alias UpdatePairCollection = TTree!(UpdatePair, Mallocator, false, "a < b", false);

void generateUpdatePairs(DSymbol* oldSymbol, DSymbol* newSymbol, ref UpdatePairCollection results)
{
	results.insert(UpdatePair(oldSymbol, newSymbol));
	foreach (part; oldSymbol.parts[])
	{
		auto temp = DSymbol(oldSymbol.name);
		auto r = newSymbol.parts.equalRange(SymbolOwnership(&temp));
		if (r.empty)
			continue;
		generateUpdatePairs(part, r.front, results);
	}
}

struct SymbolOwnership
{
	int opCmp(ref const SymbolOwnership other) const
	{
		return this.ptr.opCmp(*other.ptr);
	}

	DSymbol* ptr;
	bool owned;
	alias ptr this;
}
