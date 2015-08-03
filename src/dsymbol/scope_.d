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

module dsymbol.scope_;

import dsymbol.symbol;
import dsymbol.import_;
import dsymbol.builtin.names;
import containers.ttree;
import containers.unrolledlist;

/**
 * Contains symbols and supports lookup of symbols by cursor position.
 */
struct Scope
{
	@disable this(this);
	@disable this();

	/**
	 * Params:
	 *     begin = the beginning byte index
	 *     end = the ending byte index
	 */
	this (size_t begin, size_t end)
	{
		this.startLocation = begin;
		this.endLocation = end;
	}

	~this()
	{
		foreach (child; children[])
			typeid(Scope).destroy(child);
		foreach (info; importInformation[])
			typeid(ImportInformation).destroy(info);
	}

	/**
	 * Params:
	 *     cursorPosition = the cursor position in bytes
	 * Returns:
	 *     the innermost scope that contains the given cursor position
	 */
	Scope* getScopeByCursor(size_t cursorPosition) const pure @nogc
	{
		if (cursorPosition < startLocation) return null;
		if (cursorPosition > endLocation) return null;
		foreach (child; children[])
		{
			auto childScope = child.getScopeByCursor(cursorPosition);
			if (childScope !is null)
				return childScope;
		}
		return cast(typeof(return)) &this;
	}

	/**
	 * Params:
	 *     cursorPosition = the cursor position in bytes
	 * Returns:
	 *     all symbols in the scope containing the cursor position, as well as
	 *     the symbols in parent scopes of that scope.
	 */
	DSymbol*[] getSymbolsInCursorScope(size_t cursorPosition)
	{
		import std.array : array;
		import std.algorithm.iteration : map;

		auto s = getScopeByCursor(cursorPosition);
		if (s is null)
			return [];
		UnrolledList!(DSymbol*) symbols;
		Scope* sc = s;
		while (sc !is null)
		{
			foreach (item; sc._symbols[])
			{
				if (item.ptr.type !is null && (item.ptr.kind == CompletionKind.importSymbol
					|| item.ptr.kind == CompletionKind.withSymbol))
				{
					foreach (i; item.ptr.type.opSlice())
						symbols.insert(i);
				}
				else
					symbols.insert(item.ptr);
			}
			sc = sc.parent;
		}
		return array(_symbols[].map!(a => a.ptr));
	}

	/**
	 * Params:
	 *     name = the symbol name to search for
	 * Returns:
	 *     all symbols in this scope or parent scopes with the given name
	 */
	DSymbol*[] getSymbolsByName(istring name)
	{
		import std.array : array, appender;
		import std.algorithm.iteration : map;

		DSymbol s = DSymbol(name);
		auto er = _symbols.equalRange(SymbolOwnership(&s));
		if (!er.empty)
			return array(er.map!(a => a.ptr));

		// Check symbols from "with" statement
		DSymbol ir2 = DSymbol(WITH_SYMBOL_NAME);
		auto r2 = _symbols.equalRange(SymbolOwnership(&ir2));
		if (!r2.empty)
		{
			auto app = appender!(DSymbol*[])();
			foreach (e; r2)
			{
				if (e.type is null)
					continue;
				foreach (withSymbol; e.type.getPartsByName(s.name))
					app.put(withSymbol);
			}
			if (app.data.length > 0)
				return app.data;
		}

		// Check imported symbols
		DSymbol ir = DSymbol(IMPORT_SYMBOL_NAME);
		auto r = _symbols.equalRange(SymbolOwnership(&ir));
		if (!r.empty)
		{
			auto app = appender!(DSymbol*[])();
			foreach (e; r)
				foreach (importedSymbol; e.type.getPartsByName(s.name))
					app.put(importedSymbol);
			if (app.data.length > 0)
				return app.data;
		}
		if (parent is null)
			return [];
		return parent.getSymbolsByName(name);
	}

	/**
	 * Params:
	 *     name = the symbol name to search for
	 *     cursorPosition = the cursor position in bytes
	 * Returns:
	 *     all symbols with the given name in the scope containing the cursor
	 *     and its parent scopes
	 */
	DSymbol*[] getSymbolsByNameAndCursor(istring name, size_t cursorPosition) const
	{
		auto s = getScopeByCursor(cursorPosition);
		if (s is null)
			return [];
		return s.getSymbolsByName(name);
	}

	/**
	 * Returns an array of symbols that are present at global scope
	 */
	DSymbol*[] getSymbolsAtGlobalScope(istring name)
	{
		if (parent !is null)
			return parent.getSymbolsAtGlobalScope(name);
		return getSymbolsByName(name);
	}

	/// Imports contained in this scope
	UnrolledList!(ImportInformation*) importInformation;

	/// The scope that contains this one
	Scope* parent;

	/// Child scopes
	UnrolledList!(Scope*, false) children;

	/// Start location of this scope in bytes
	size_t startLocation;

	/// End location of this scope in bytes
	size_t endLocation;

	auto symbols() @property
	{
		return _symbols[];
	}

	void addSymbol(DSymbol* symbol, bool owns)
	{
		_symbols.insert(SymbolOwnership(symbol, owns));
	}

private:
	/// Symbols contained in this scope
	TTree!(SymbolOwnership, true, "a < b", false) _symbols;
}
