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

module dsymbol.semantic;

import dsymbol.symbol;
import dparse.ast;
import dparse.lexer;
import containers.unrolledlist;
import dsymbol.type_lookup;
import dsymbol.allocator : dispose, Mallocator;

enum ResolutionFlags : ubyte
{
	inheritance = 0b0000_0001,
	type = 0b0000_0010,
	mixinTemplates = 0b0000_0100,
}

/**
 * Intermediate form between DSymbol and the AST classes. Stores enough
 * information to resolve things like base classes and alias this.
 */
struct SemanticSymbol
{
public:

	/// Disable default construction.
	@disable this();
	/// Disable copy construction
	@disable this(this);

	/**
	 * Params:
	 *    name = the name
	 */
	this(DSymbol* acSymbol)
	{
		this.acSymbol = acSymbol;
	}

	~this()
	{
		import stdx.allocator : dispose;

		foreach (child; children[])
			typeid(SemanticSymbol).destroy(child);
		foreach (lookup; typeLookups[])
			Mallocator.instance.dispose(lookup);
	}

	/**
	 * Adds a child to the children field and updates the acSymbol's parts field
	 */
	void addChild(SemanticSymbol* child, bool owns)
	{
		children.insert(child);
		acSymbol.addChild(child.acSymbol, owns);
	}

	/// Information used to do type resolution, inheritance, mixins, and alias this
	UnrolledList!(TypeLookup*, Mallocator, false) typeLookups;

	/// Child symbols
	UnrolledList!(SemanticSymbol*, Mallocator, false) children;

	/// Autocompletion symbol
	DSymbol* acSymbol;

	/// Parent symbol
	SemanticSymbol* parent;

	/// Protection level for this symobol
	deprecated("Use acSymbol.protection instead") ref inout(IdType) protection() @property inout
	{
		return acSymbol.protection;
	}
}

/**
 * Type of the _argptr variable
 */
Type argptrType;

/**
 * Type of _arguments
 */
Type argumentsType;

static this()
{
	import dsymbol.string_interning : internString;
	import stdx.allocator : make;

	// TODO: Replace these with DSymbols

	// _argptr has type void*
	argptrType = make!Type(Mallocator.instance);
	argptrType.type2 = make!Type2(Mallocator.instance);
	argptrType.type2.builtinType = tok!"void";
	TypeSuffix argptrTypeSuffix = make!TypeSuffix(Mallocator.instance);
	argptrTypeSuffix.star = Token(tok!"*");
	argptrType.typeSuffixes = cast(TypeSuffix[]) Mallocator.instance.allocate(TypeSuffix.sizeof);
	argptrType.typeSuffixes[0] = argptrTypeSuffix;

	// _arguments has type TypeInfo[]
	argumentsType = make!Type(Mallocator.instance);
	argumentsType.type2 = make!Type2(Mallocator.instance);
	argumentsType.type2.typeIdentifierPart = make!TypeIdentifierPart(Mallocator.instance);
	IdentifierOrTemplateInstance i = make!IdentifierOrTemplateInstance(Mallocator.instance);
	i.identifier.text = internString("TypeInfo");
	i.identifier.type = tok!"identifier";
	argumentsType.type2.typeIdentifierPart.identifierOrTemplateInstance = i;
	TypeSuffix argumentsTypeSuffix = make!TypeSuffix(Mallocator.instance);
	argumentsTypeSuffix.array = true;
	argumentsType.typeSuffixes = cast(TypeSuffix[]) Mallocator.instance.allocate(TypeSuffix.sizeof);
	argumentsType.typeSuffixes[0] = argumentsTypeSuffix;
}

static ~this()
{
	dispose(Mallocator.instance, argumentsType.typeSuffixes[0]);
	dispose(Mallocator.instance, argumentsType.type2.typeIdentifierPart.identifierOrTemplateInstance);
	dispose(Mallocator.instance, argumentsType.type2.typeIdentifierPart);
	dispose(Mallocator.instance, argumentsType.type2);
	dispose(Mallocator.instance, argptrType.typeSuffixes[0]);
	dispose(Mallocator.instance, argptrType.type2);

	Mallocator.instance.deallocate(argumentsType.typeSuffixes);
	Mallocator.instance.deallocate(argptrType.typeSuffixes);

	dispose(Mallocator.instance, argumentsType);
	dispose(Mallocator.instance, argptrType);
}
