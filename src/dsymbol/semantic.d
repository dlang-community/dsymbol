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
import dsymbol.makex : makeX, disposeX, AllocatorX;
import stdx.allocator.mallocator : Mallocator;

@safe:

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
	this(DSymbol* acSymbol) nothrow
	{
		this.acSymbol = acSymbol;
	}

	~this() @trusted
	{
		import stdx.allocator.mallocator : Mallocator;
		import stdx.allocator : dispose;

		foreach (child; children[])
			typeid(SemanticSymbol).destroy(child);
		foreach (lookup; typeLookups[])
			Mallocator.instance.disposeX(lookup);
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
	UnrolledList!(TypeLookup*, AllocatorX, false) typeLookups;

	/// Child symbols
	UnrolledList!(SemanticSymbol*, AllocatorX, false) children;

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

static this() @trusted
{
	import dsymbol.string_interning : internString;
	import stdx.allocator : make;
	import stdx.allocator.mallocator : Mallocator;

	// TODO: Replace these with DSymbols

	// _argptr has type void*
	argptrType = makeX!Type(Mallocator.instance);
	argptrType.type2 = makeX!Type2(Mallocator.instance);
	argptrType.type2.builtinType = tok!"void";
	TypeSuffix argptrTypeSuffix = makeX!TypeSuffix(Mallocator.instance);
	argptrTypeSuffix.star = Token(tok!"*");
	argptrType.typeSuffixes = cast(TypeSuffix[]) Mallocator.instance.allocate(TypeSuffix.sizeof);
	argptrType.typeSuffixes[0] = argptrTypeSuffix;

	// _arguments has type TypeInfo[]
	argumentsType = makeX!Type(Mallocator.instance);
	argumentsType.type2 = makeX!Type2(Mallocator.instance);
	argumentsType.type2.typeIdentifierPart = makeX!TypeIdentifierPart(Mallocator.instance);
	IdentifierOrTemplateInstance i = makeX!IdentifierOrTemplateInstance(Mallocator.instance);
	i.identifier.text = internString("TypeInfo");
	i.identifier.type = tok!"identifier";
	argumentsType.type2.typeIdentifierPart.identifierOrTemplateInstance = i;
	TypeSuffix argumentsTypeSuffix = makeX!TypeSuffix(Mallocator.instance);
	argumentsTypeSuffix.array = true;
	argumentsType.typeSuffixes = cast(TypeSuffix[]) Mallocator.instance.allocate(TypeSuffix.sizeof);
	argumentsType.typeSuffixes[0] = argumentsTypeSuffix;
}

static ~this() @trusted
{
	import stdx.allocator.mallocator : Mallocator;

	disposeX(Mallocator.instance, argumentsType.typeSuffixes[0]);
	disposeX(Mallocator.instance, argumentsType.type2.typeIdentifierPart.identifierOrTemplateInstance);
	disposeX(Mallocator.instance, argumentsType.type2.typeIdentifierPart);
	disposeX(Mallocator.instance, argumentsType.type2);
	disposeX(Mallocator.instance, argptrType.typeSuffixes[0]);
	disposeX(Mallocator.instance, argptrType.type2);

	Mallocator.instance.deallocate(argumentsType.typeSuffixes);
	Mallocator.instance.deallocate(argptrType.typeSuffixes);

	disposeX(Mallocator.instance, argumentsType);
	disposeX(Mallocator.instance, argptrType);
}
