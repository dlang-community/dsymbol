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

module dsymbol.conversion;

import dsymbol.cache_entry;
import dsymbol.conversion.first;
import dsymbol.conversion.second;
import dsymbol.modulecache;
import dsymbol.scope_;
import dsymbol.string_interning;
import dsymbol.symbol;
import dsymbol.semantic;
import dparse.ast;
import dparse.lexer;
import dparse.parser;
import dparse.rollback_allocator;
import stdx.allocator;
import std.functional;
import std.typecons;

/**
 * Used by autocompletion.
 */
ScopeSymbolPair generateAutocompleteTrees(const(Token)[] tokens,
	IAllocator symbolAllocator, RollbackAllocator* parseAllocator,
	size_t cursorPosition, ref ModuleCache cache)
{
	Module m = parseModuleForAutocomplete(tokens, internString("stdin"),
		parseAllocator, cursorPosition);

	auto first = scoped!FirstPass(m, internString("stdin"), symbolAllocator,
		symbolAllocator, true, &cache);
	first.run();

	secondPass(first.rootSymbol, first.moduleScope, cache);
	auto r = first.rootSymbol.acSymbol;
	typeid(SemanticSymbol).destroy(first.rootSymbol);
	return ScopeSymbolPair(r, first.moduleScope);
}

struct ScopeSymbolPair
{
	void destroy()
	{
		typeid(DSymbol).destroy(symbol);
		typeid(Scope).destroy(scope_);
	}

	DSymbol* symbol;
	Scope* scope_;
}

/**
 * Used by import symbol caching.
 *
 * Params:
 *     tokens = the tokens that compose the file
 *     fileName = the name of the file being parsed
 *     parseAllocator = the allocator to use for the AST
 * Returns: the parsed module
 */
Module parseModuleSimple(const(Token)[] tokens, string fileName, RollbackAllocator* parseAllocator)
{
	assert (parseAllocator !is null);
	auto parser = scoped!SimpleParser();
	parser.fileName = fileName;
	parser.tokens = tokens;
	parser.messageDg = toDelegate(&doesNothing);
	parser.allocator = parseAllocator;
	return parser.parseModule();
}

private:

Module parseModuleForAutocomplete(const(Token)[] tokens, string fileName,
	RollbackAllocator* parseAllocator, size_t cursorPosition)
{
	auto parser = scoped!AutocompleteParser();
	parser.fileName = fileName;
	parser.tokens = tokens;
	parser.messageDg = toDelegate(&doesNothing);
	parser.allocator = parseAllocator;
	parser.cursorPosition = cursorPosition;
	return parser.parseModule();
}

class AutocompleteParser : Parser
{
	override BlockStatement parseBlockStatement()
	{
		if (current.index > cursorPosition)
		{
			BlockStatement bs = allocator.make!(BlockStatement);
			bs.startLocation = current.index;
			skipBraces();
			bs.endLocation = tokens[index - 1].index;
			return bs;
		}
		immutable start = current.index;
		auto b = setBookmark();
		skipBraces();
		if (tokens[index - 1].index < cursorPosition)
		{
			abandonBookmark(b);
			BlockStatement bs = allocator.make!BlockStatement();
			bs.startLocation = start;
			bs.endLocation = tokens[index - 1].index;
			return bs;
		}
		else
		{
			goToBookmark(b);
			return super.parseBlockStatement();
		}
	}

private:
	size_t cursorPosition;
}

class SimpleParser : Parser
{
	override Unittest parseUnittest()
	{
		expect(tok!"unittest");
		if (currentIs(tok!"{"))
			skipBraces();
		return null;
	}

	override FunctionBody parseFunctionBody()
	{
		bool needDo;
		// no impl
		if (currentIs(tok!";"))
			advance();
		// no contracts
		else if (currentIs(tok!"{"))
			skipBraces();
		// skip contracts
		else while (true)
		{
			if (currentIs(tok!"in"))
			{
				if (moreTokens)
					advance();
				if (currentIs(tok!"{"))
				{
					skipBraces();
					needDo = true;
				}
				if (currentIs(tok!"("))
					skipParens();
			}
			else if (currentIs(tok!"out"))
			{
				if (moreTokens)
					advance();
				if (currentIs(tok!"("))
					skipParens();
				if (currentIs(tok!"{"))
				{
					skipBraces();
					needDo = true;
				}
				if (currentIs(tok!"("))
					skipParens();
			}
			else break;
		}
		if (needDo && !currentIs(tok!"{"))
			advance();
		// body
		if (currentIs(tok!"{"))
			skipBraces();
		return allocator.make!FunctionBody();
	}
}

void doesNothing(string, size_t, size_t, string, bool) {}
