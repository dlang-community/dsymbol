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

module dsymbol.conversion.first;

import containers.unrolledlist;
import dparse.ast;
import dparse.formatter;
import dparse.lexer;
import dsymbol.builtin.names;
import dsymbol.builtin.symbols;
import dsymbol.cache_entry;
import dsymbol.import_;
import dsymbol.modulecache;
import dsymbol.scope_;
import dsymbol.semantic;
import dsymbol.string_interning;
import dsymbol.symbol;
import dsymbol.type_lookup;
import dsymbol.makex;
import std.algorithm.iteration : map;
import stdx.allocator;
import stdx.allocator.mallocator;
import std.experimental.logger;
import std.typecons;

@safe:

/**
 * First Pass handles the following:
 * $(UL
 *     $(LI symbol name)
 *     $(LI symbol location)
 *     $(LI alias this locations)
 *     $(LI base class names)
 *     $(LI protection level)
 *     $(LI symbol kind)
 *     $(LI function call tip)
 *     $(LI symbol file path)
 * )
 */
@safe final class FirstPass : ASTVisitor
{
	/**
	 * Params:
	 *     mod = the module to visit
	 *     symbolFile = path to the file being converted
	 *     symbolAllocator = allocator used for the auto-complete symbols
	 *     semanticAllocator = allocator used for semantic symbols
	 *     includeParameterSymbols = include parameter symbols as children of
	 *         function decalarations and constructors
	 */
	this(const Module mod, istring symbolFile, IAllocator symbolAllocator,
		IAllocator semanticAllocator, bool includeParameterSymbols,
		ModuleCache* cache, CacheEntry* entry = null) nothrow
	in
	{
		assert(mod);
		assert(symbolAllocator);
		assert(semanticAllocator);
		assert(cache);
	}
	do
	{
		this.mod = mod;
		this.symbolFile = symbolFile;
		this.symbolAllocator = symbolAllocator;
		this.semanticAllocator = semanticAllocator;
		this.includeParameterSymbols = includeParameterSymbols;
		this.entry = entry;
		this.cache = cache;
	}

	/**
	 * Runs the against the AST and produces symbols.
	 */
	void run()
	{
		visit(mod);
	}

	override void visit(const Unittest u)
	{
		// Create a dummy symbol because we don't want unit test symbols leaking
		// into the symbol they're declared in.
		pushSymbol(UNITTEST_SYMBOL_NAME,
			CompletionKind.dummy, istring(null));
		scope(exit) popSymbol();
        () @trusted { u.accept(this); } ();
	}

	override void visit(const Constructor con)
	{
		visitConstructor(con.location, con.parameters, con.templateParameters, con.functionBody, con.comment);
	}

	override void visit(const SharedStaticConstructor con)
	{
		visitConstructor(con.location, null, null, con.functionBody, con.comment);
	}

	override void visit(const StaticConstructor con)
	{
		visitConstructor(con.location, null, null, con.functionBody, con.comment);
	}

	override void visit(const Destructor des)
	{
		visitDestructor(des.index, des.functionBody, des.comment);
	}

	override void visit(const SharedStaticDestructor des)
	{
		visitDestructor(des.location, des.functionBody, des.comment);
	}

	override void visit(const StaticDestructor des)
	{
		visitDestructor(des.location, des.functionBody, des.comment);
	}

	override void visit(const FunctionDeclaration dec)
	{
		assert(dec);
		pushSymbol(dec.name.text, CompletionKind.functionName, symbolFile,
				dec.name.index, dec.returnType);
		scope (exit) popSymbol();
		currentSymbol.acSymbol.protection = protection.current;
		currentSymbol.acSymbol.doc = makeDocumentation(dec.comment);

		istring lastComment = this.lastComment;
		this.lastComment = istring.init;
		scope(exit) this.lastComment = lastComment;

		if (dec.functionBody !is null)
		{
			pushFunctionScope(dec.functionBody, semanticAllocator,
					dec.name.index + dec.name.text.length);
			scope (exit) popScope();
			processParameters(currentSymbol, dec.returnType,
					currentSymbol.acSymbol.name, dec.parameters, dec.templateParameters);
            () @trusted { dec.functionBody.accept(this); } ();
		}
		else
		{
			immutable ips = includeParameterSymbols;
			includeParameterSymbols = false;
			processParameters(currentSymbol, dec.returnType,
					currentSymbol.acSymbol.name, dec.parameters, dec.templateParameters);
			includeParameterSymbols = ips;
		}
	}

	override void visit(const FunctionLiteralExpression exp)
	{
		assert(exp);

		auto fbody = exp.specifiedFunctionBody;
		if (fbody is null)
			return;
		auto block = fbody.blockStatement;
		if (block is null)
			return;

		pushSymbol(FUNCTION_LITERAL_SYMBOL_NAME, CompletionKind.dummy, symbolFile,
			block.startLocation, null);
		scope(exit) popSymbol();

		pushScope(block.startLocation, block.endLocation);
		scope (exit) popScope();
		processParameters(currentSymbol, exp.returnType,
				FUNCTION_LITERAL_SYMBOL_NAME, exp.parameters, null);
        () @trusted { block.accept(this); } ();
	}

	override void visit(const ClassDeclaration dec)
	{
		visitAggregateDeclaration(dec, CompletionKind.className);
	}

	override void visit(const TemplateDeclaration dec)
	{
		visitAggregateDeclaration(dec, CompletionKind.templateName);
	}

	override void visit(const InterfaceDeclaration dec)
	{
		visitAggregateDeclaration(dec, CompletionKind.interfaceName);
	}

	override void visit(const UnionDeclaration dec)
	{
		visitAggregateDeclaration(dec, CompletionKind.unionName);
	}

	override void visit(const StructDeclaration dec)
	{
		visitAggregateDeclaration(dec, CompletionKind.structName);
	}

	override void visit(const NewAnonClassExpression nace)
	{
		// its base classes would be added as "inherit" breadcrumbs in the current symbol
		skipBaseClassesOfNewAnon = true;
        () @trusted { nace.accept(this); } ();
		skipBaseClassesOfNewAnon = false;
	}

	override void visit(const BaseClass bc)
	{
		if (skipBaseClassesOfNewAnon)
			return;
		if (bc.type2.typeIdentifierPart is null ||
			bc.type2.typeIdentifierPart.identifierOrTemplateInstance is null)
			return;
		auto lookup = () @trusted { return Mallocator.instance.makeX!TypeLookup(TypeLookupKind.inherit); } ();
		writeIotcTo(bc.type2.typeIdentifierPart, lookup.breadcrumbs);
		currentSymbol.typeLookups.insert(lookup);

		// create an alias to the BaseClass to allow completions
		// of the form : `instance.BaseClass.`, which is
		// mostly used to bypass the most derived overrides.
		const idt = lookup.breadcrumbs.back;
		if (!idt.length)
			return;
		SemanticSymbol* symbol = allocateSemanticSymbol(idt,
			CompletionKind.aliasName, symbolFile, currentScope.endLocation);
		Type t = () @trusted { return Mallocator.instance.makeX!Type; } ();
        () @trusted { t.type2 = cast() bc.type2; } ();
		addTypeToLookups(symbol.typeLookups, t);
		symbol.parent = currentSymbol;
		currentSymbol.addChild(symbol, true);
		symbol.acSymbol.protection = protection.current;
	}

	override void visit(const VariableDeclaration dec)
	{
		assert (currentSymbol);
		foreach (declarator; dec.declarators)
		{
			SemanticSymbol* symbol = allocateSemanticSymbol(
				declarator.name.text, CompletionKind.variableName,
				symbolFile, declarator.name.index);
			if (dec.type !is null)
				addTypeToLookups(symbol.typeLookups, dec.type);
			symbol.parent = currentSymbol;
			symbol.acSymbol.protection = protection.current;
			symbol.acSymbol.doc = makeDocumentation(declarator.comment);
			currentSymbol.addChild(symbol, true);
			currentScope.addSymbol(symbol.acSymbol, false);

			if (currentSymbol.acSymbol.kind == CompletionKind.structName
				|| currentSymbol.acSymbol.kind == CompletionKind.unionName)
			{
                () @trusted {
                    structFieldNames.insert(symbol.acSymbol.name);
                    // TODO: remove this cast. See the note on structFieldTypes
                    structFieldTypes.insert(cast() dec.type);
                } ();
			}
		}
		if (dec.autoDeclaration !is null)
		{
			foreach (part; dec.autoDeclaration.parts)
			{
				SemanticSymbol* symbol = allocateSemanticSymbol(
					part.identifier.text, CompletionKind.variableName,
					symbolFile, part.identifier.index);
				symbol.parent = currentSymbol;
				populateInitializer(symbol, part.initializer);
				symbol.acSymbol.protection = protection.current;
				symbol.acSymbol.doc = makeDocumentation(dec.comment);
				currentSymbol.addChild(symbol, true);
				currentScope.addSymbol(symbol.acSymbol, false);

				if (currentSymbol.acSymbol.kind == CompletionKind.structName
					|| currentSymbol.acSymbol.kind == CompletionKind.unionName)
				{
                    () @trusted {
                        structFieldNames.insert(symbol.acSymbol.name);
                        // TODO: remove this cast. See the note on structFieldTypes
                        structFieldTypes.insert(null);
                    } ();
				}
			}
		}
	}

	override void visit(const AliasDeclaration aliasDeclaration)
	{
		if (aliasDeclaration.initializers.length == 0)
		{
			foreach (name; aliasDeclaration.declaratorIdentifierList.identifiers)
			{
				SemanticSymbol* symbol = allocateSemanticSymbol(
					name.text, CompletionKind.aliasName, symbolFile, name.index);
				if (aliasDeclaration.type !is null)
					addTypeToLookups(symbol.typeLookups, aliasDeclaration.type);
				symbol.parent = currentSymbol;
				currentSymbol.addChild(symbol, true);
				currentScope.addSymbol(symbol.acSymbol, false);
				symbol.acSymbol.protection = protection.current;
				symbol.acSymbol.doc = makeDocumentation(aliasDeclaration.comment);
			}
		}
		else
		{
			foreach (initializer; aliasDeclaration.initializers)
			{
				SemanticSymbol* symbol = allocateSemanticSymbol(
					initializer.name.text, CompletionKind.aliasName,
					symbolFile, initializer.name.index);
				if (initializer.type !is null)
					addTypeToLookups(symbol.typeLookups, initializer.type);
				symbol.parent = currentSymbol;
				currentSymbol.addChild(symbol, true);
				currentScope.addSymbol(symbol.acSymbol, false);
				symbol.acSymbol.protection = protection.current;
				symbol.acSymbol.doc = makeDocumentation(aliasDeclaration.comment);
			}
		}
	}

	override void visit(const AliasThisDeclaration dec)
	{
		const k = currentSymbol.acSymbol.kind;
		if (k != CompletionKind.structName && k != CompletionKind.className &&
			k != CompletionKind.unionName && k != CompletionKind.mixinTemplateName)
		{
			return;
		}
        auto _ = () @trusted { return Mallocator.instance.makeX!TypeLookup(internString(dec.identifier.text), TypeLookupKind.aliasThis); } ();
		currentSymbol.typeLookups.insert(_);
	}

	override void visit(const Declaration dec)
	{
        auto ad = () @trusted { return dec.attributeDeclaration; } ();
		if (ad !is null
			&& isProtection(ad.attribute.attribute.type))
		{
			protection.addScope(ad.attribute.attribute.type);
			return;
		}
		IdType p;
		foreach (const Attribute attr; dec.attributes)
		{
			if (isProtection(attr.attribute.type))
				p = attr.attribute.type;
		}
		if (p != tok!"")
		{
			protection.beginLocal(p);
			if (dec.declarations.length > 0)
			{
				protection.beginScope();
                () @trusted { dec.accept(this); } ();
				protection.endScope();
			}
			else
                () @trusted { dec.accept(this); } ();
			protection.endLocal();
		}
		else
            () @trusted { dec.accept(this); } ();
	}

	override void visit(const Module mod)
	{
		rootSymbol = allocateSemanticSymbol(null, CompletionKind.moduleName,
			symbolFile);
		currentSymbol = rootSymbol;
		moduleScope = () @trusted { return semanticAllocator.makeX!Scope(0, uint.max); } ();
		currentScope = moduleScope;
		auto objectLocation = cache.resolveImportLocation("object");
		if (objectLocation is null)
			warning("Could not locate object.d or object.di");
		else
		{
			auto objectImport = allocateSemanticSymbol(IMPORT_SYMBOL_NAME,
				CompletionKind.importSymbol, objectLocation);
			objectImport.acSymbol.skipOver = true;
			currentSymbol.addChild(objectImport, true);
			currentScope.addSymbol(objectImport.acSymbol, false);
		}
        () @trusted {
            foreach (s; builtinSymbols[])
                currentScope.addSymbol(s, false);
        } ();
        () @trusted { mod.accept(this); } ();
	}

	override void visit(const EnumDeclaration dec)
	{
		assert (currentSymbol);
		SemanticSymbol* symbol = allocateSemanticSymbol(dec.name.text,
			CompletionKind.enumName, symbolFile, dec.name.index);
		if (dec.type !is null)
			addTypeToLookups(symbol.typeLookups, dec.type);
		symbol.acSymbol.addChildren(enumSymbols[], false);
		symbol.parent = currentSymbol;
		currentSymbol.addChild(symbol, true);
		currentScope.addSymbol(symbol.acSymbol, false);
		symbol.acSymbol.doc = makeDocumentation(dec.comment);

		istring lastComment = this.lastComment;
		this.lastComment = istring.init;
		scope(exit) this.lastComment = lastComment;

		currentSymbol = symbol;

		if (dec.enumBody !is null)
		{
			pushScope(dec.enumBody.startLocation, dec.enumBody.endLocation);
            () @trusted { dec.enumBody.accept(this); } ();
			popScope();
		}

		currentSymbol = currentSymbol.parent;
	}

	mixin visitEnumMember!EnumMember;
	mixin visitEnumMember!AnonymousEnumMember;

	override void visit(const ModuleDeclaration moduleDeclaration)
	{
		const parts = moduleDeclaration.moduleName.identifiers;
		rootSymbol.acSymbol.name = internString(parts.length ? parts[$ - 1].text : null);
	}

	override void visit(const StructBody structBody) @trusted
	{
		import std.algorithm : move;

		pushScope(structBody.startLocation, structBody.endLocation);
		scope (exit) popScope();
		protection.beginScope();
		scope (exit) protection.endScope();

		auto savedStructFieldNames = move(structFieldNames);
		auto savedStructFieldTypes = move(structFieldTypes);
		scope(exit) structFieldNames = move(savedStructFieldNames);
		scope(exit) structFieldTypes = move(savedStructFieldTypes);

		DSymbol* thisSymbol = makeX!DSymbol(symbolAllocator, THIS_SYMBOL_NAME,
			CompletionKind.variableName, currentSymbol.acSymbol);
		thisSymbol.location = currentScope.startLocation;
		thisSymbol.symbolFile = symbolFile;
		thisSymbol.type = currentSymbol.acSymbol;
		thisSymbol.ownType = false;
		currentScope.addSymbol(thisSymbol, false);

		foreach (dec; structBody.declarations)
			visit(dec);

		// If no constructor is found, generate one
		if ((currentSymbol.acSymbol.kind == CompletionKind.structName
				|| currentSymbol.acSymbol.kind == CompletionKind.unionName)
				&& currentSymbol.acSymbol.getFirstPartNamed(CONSTRUCTOR_SYMBOL_NAME) is null)
			createConstructor();
	}

	override void visit(const ImportDeclaration importDeclaration)
	{
		import std.algorithm : filter, map;
		import std.path : buildPath;
		import std.typecons : Tuple;

		foreach (single; importDeclaration.singleImports.filter!(
			a => a !is null && a.identifierChain !is null))
		{
			immutable importPath = convertChainToImportPath(single.identifierChain);
			istring modulePath = cache.resolveImportLocation(importPath);
			if (modulePath is null)
			{
				warning("Could not resolve location of module '", importPath.data, "'");
				continue;
			}
			SemanticSymbol* importSymbol = allocateSemanticSymbol(IMPORT_SYMBOL_NAME,
				CompletionKind.importSymbol, modulePath);
			importSymbol.acSymbol.skipOver = protection.currentForImport != tok!"public";
			if (single.rename == tok!"")
			{
				size_t i = 0;
				DSymbol* currentImportSymbol;
				foreach (p; single.identifierChain.identifiers.map!(a => a.text))
				{
					immutable bool first = i == 0;
					immutable bool last = i + 1 >= single.identifierChain.identifiers.length;
					immutable CompletionKind kind = last ? CompletionKind.moduleName
						: CompletionKind.packageName;
					istring ip = internString(p);
					if (first)
					{
						auto s = currentScope.getSymbolsByName(ip);
						if (s.length == 0)
						{
                            () @trusted { currentImportSymbol = symbolAllocator.makeX!DSymbol(ip, kind); } ();
							currentScope.addSymbol(currentImportSymbol, true);
							if (last)
							{
								currentImportSymbol.symbolFile = modulePath;
								currentImportSymbol.type = importSymbol.acSymbol;
								currentImportSymbol.ownType = false;
							}
						}
						else
							currentImportSymbol = s[0];
					}
					else
					{
						auto s = currentImportSymbol.getPartsByName(ip);
						if (s.length == 0)
						{
							auto sym = () @trusted { return symbolAllocator.makeX!DSymbol(ip, kind); } ();
							currentImportSymbol.addChild(sym, true);
							currentImportSymbol = sym;
							if (last)
							{
								currentImportSymbol.symbolFile = modulePath;
								currentImportSymbol.type = importSymbol.acSymbol;
								currentImportSymbol.ownType = false;
							}
						}
						else
							currentImportSymbol = s[0];
					}
					i++;
				}
				currentSymbol.addChild(importSymbol, true);
				currentScope.addSymbol(importSymbol.acSymbol, false);
			}
			else
			{
				SemanticSymbol* renameSymbol = allocateSemanticSymbol(
					internString(single.rename.text), CompletionKind.aliasName,
					modulePath);
				renameSymbol.acSymbol.skipOver = protection.currentForImport != tok!"public";
				renameSymbol.acSymbol.type = importSymbol.acSymbol;
				renameSymbol.acSymbol.ownType = true;
				renameSymbol.addChild(importSymbol, true);
				currentSymbol.addChild(renameSymbol, true);
				currentScope.addSymbol(renameSymbol.acSymbol, false);
			}
			if (entry !is null)
                () @trusted { entry.dependencies.insert(modulePath); } ();
		}
		if (importDeclaration.importBindings is null) return;
		if (importDeclaration.importBindings.singleImport.identifierChain is null) return;

		immutable chain = convertChainToImportPath(importDeclaration.importBindings.singleImport.identifierChain);
		istring modulePath = cache.resolveImportLocation(chain);
		if (modulePath is null)
		{
			warning("Could not resolve location of module '", chain, "'");
			return;
		}

		foreach (bind; importDeclaration.importBindings.importBinds)
		{
			TypeLookup* lookup = () @trusted { return Mallocator.instance.makeX!TypeLookup(TypeLookupKind.selectiveImport); } ();

			immutable bool isRenamed = bind.right != tok!"";

			// The second phase must change this `importSymbol` kind to
			// `aliasName` for symbol lookup to work.
			SemanticSymbol* importSymbol = allocateSemanticSymbol(
				isRenamed ? bind.left.text : IMPORT_SYMBOL_NAME,
				CompletionKind.importSymbol, modulePath);

			if (isRenamed)
			{
                () @trusted { lookup.breadcrumbs.insert(internString(bind.right.text)); } ();
				importSymbol.acSymbol.location = bind.left.index;
				importSymbol.acSymbol.altFile = symbolFile;
			}
            () @trusted { lookup.breadcrumbs.insert(internString(bind.left.text)); } ();

			importSymbol.acSymbol.qualifier = SymbolQualifier.selectiveImport;
			importSymbol.typeLookups.insert(lookup);
			importSymbol.acSymbol.skipOver = protection.currentForImport != tok!"public";
			currentSymbol.addChild(importSymbol, true);
			currentScope.addSymbol(importSymbol.acSymbol, false);
		}

		if (entry !is null)
            () @trusted { entry.dependencies.insert(modulePath); } ();
	}

	// Create scope for block statements
	override void visit(const BlockStatement blockStatement)
	{
		if (blockStatement.declarationsAndStatements !is null)
		{
			pushScope(blockStatement.startLocation, blockStatement.endLocation);
			scope(exit) popScope();
            () @trusted { visit (blockStatement.declarationsAndStatements); } ();
		}
	}

	// Create attribute/protection scope for conditional compilation declaration
	// blocks.
	override void visit(const ConditionalDeclaration conditionalDecl)
	{
		if (conditionalDecl.compileCondition !is null)
            () @trusted { visit(conditionalDecl.compileCondition); } ();

		if (conditionalDecl.trueDeclarations.length)
		{
			protection.beginScope();
			scope (exit) protection.endScope();

			foreach (decl; conditionalDecl.trueDeclarations)
				if (decl !is null)
					visit (decl);
		}

		if (conditionalDecl.falseDeclarations.length)
		{
			protection.beginScope();
			scope (exit) protection.endScope();

			foreach (decl; conditionalDecl.falseDeclarations)
				if (decl !is null)
					visit (decl);
		}
	}

	override void visit(const TemplateMixinExpression tme)
	{
		// TODO: support typeof here
		if (tme.mixinTemplateName.symbol is null)
			return;
		const Symbol sym = tme.mixinTemplateName.symbol;
		auto lookup = () @trusted { return Mallocator.instance.makeX!TypeLookup(TypeLookupKind.mixinTemplate); } ();

		writeIotcTo(tme.mixinTemplateName.symbol.identifierOrTemplateChain,
			lookup.breadcrumbs);

		if (currentSymbol.acSymbol.kind != CompletionKind.functionName)
			currentSymbol.typeLookups.insert(lookup);

		/* If the mixin is named then do like if `mixin F f;` would be `mixin F; alias f = F;`
		which's been empirically verified to produce the right completions for `f.`,
		*/
		if (tme.identifier != tok!"" && sym.identifierOrTemplateChain &&
			sym.identifierOrTemplateChain.identifiersOrTemplateInstances.length)
		{
			SemanticSymbol* symbol = allocateSemanticSymbol(tme.identifier.text,
				CompletionKind.aliasName, symbolFile, tme.identifier.index);
			Type tp = () @trusted { return Mallocator.instance.makeX!Type; } ();
			tp.type2 = () @trusted { return Mallocator.instance.makeX!Type2; } ();
			TypeIdentifierPart root;
			TypeIdentifierPart current;
			foreach(ioti; sym.identifierOrTemplateChain.identifiersOrTemplateInstances)
			{
				TypeIdentifierPart old = current;
				current = () @trusted { return Mallocator.instance.makeX!TypeIdentifierPart; } ();
				if (old)
				{
					old.typeIdentifierPart = current;
				}
				else
				{
					root = current;
				}
                () @trusted { current.identifierOrTemplateInstance = cast() ioti; } ();
			}
			tp.type2.typeIdentifierPart = root;
			addTypeToLookups(symbol.typeLookups, tp);
			symbol.parent = currentSymbol;
			currentSymbol.addChild(symbol, true);
			currentScope.addSymbol(symbol.acSymbol, false);
			symbol.acSymbol.protection = protection.current;
		}
	}

	override void visit(const ForeachStatement feStatement)
	{
		if (feStatement.declarationOrStatement !is null
			&& feStatement.declarationOrStatement.statement !is null
			&& feStatement.declarationOrStatement.statement.statementNoCaseNoDefault !is null
			&& feStatement.declarationOrStatement.statement.statementNoCaseNoDefault.blockStatement !is null)
		{
			const BlockStatement bs =
				feStatement.declarationOrStatement.statement.statementNoCaseNoDefault.blockStatement;
			pushScope(feStatement.startIndex, bs.endLocation);
			scope(exit) popScope();
			feExpression = feStatement.low.items[$ - 1];
            () @trusted { feStatement.accept(this); } ();
			feExpression = null;
		}
		else
		{
			const ubyte o1 = foreachTypeIndexOfInterest;
			const ubyte o2 = foreachTypeIndex;
            () @trusted { feStatement.accept(this); } ();
			foreachTypeIndexOfInterest = o1;
			foreachTypeIndex = o2;
		}
	}

	override void visit(const ForeachTypeList feTypeList)
	{
		foreachTypeIndex = 0;
		foreachTypeIndexOfInterest = cast(ubyte)(feTypeList.items.length - 1);
        () @trusted { feTypeList.accept(this); } ();
	}

	override void visit(const ForeachType feType)
	{
		if (foreachTypeIndex++ == foreachTypeIndexOfInterest)
		{
			SemanticSymbol* symbol = allocateSemanticSymbol(feType.identifier.text,
				CompletionKind.variableName, symbolFile, feType.identifier.index);
			if (feType.type !is null)
				addTypeToLookups(symbol.typeLookups, feType.type);
			symbol.parent = currentSymbol;
			currentSymbol.addChild(symbol, true);
			currentScope.addSymbol(symbol.acSymbol, true);
			if (symbol.typeLookups.empty && feExpression !is null)
				populateInitializer(symbol, feExpression, true);
		}
	}

	override void visit(const IfStatement ifs)
	{
		if (ifs.identifier != tok!"" && ifs.thenStatement)
		{
			pushScope(ifs.thenStatement.startLocation, ifs.thenStatement.endLocation);
			scope(exit) popScope();

			SemanticSymbol* symbol = allocateSemanticSymbol(ifs.identifier.text,
				CompletionKind.variableName, symbolFile, ifs.identifier.index);
			if (ifs.type !is null)
				addTypeToLookups(symbol.typeLookups, ifs.type);
			symbol.parent = currentSymbol;
			currentSymbol.addChild(symbol, true);
			currentScope.addSymbol(symbol.acSymbol, true);
			if (symbol.typeLookups.empty && ifs.expression !is null)
				populateInitializer(symbol, ifs.expression, false);
		}
        () @trusted { ifs.accept(this); } ();
	}

	override void visit(const WithStatement withStatement)
	{
		if (withStatement.expression !is null
			&& withStatement.declarationOrStatement !is null)
		{
			pushScope(withStatement.declarationOrStatement.startLocation,
				withStatement.declarationOrStatement.endLocation);
			scope(exit) popScope();

			pushSymbol(WITH_SYMBOL_NAME, CompletionKind.withSymbol, symbolFile,
				currentScope.startLocation, null);
			scope(exit) popSymbol();

			populateInitializer(currentSymbol, withStatement.expression, false);
            () @trusted { withStatement.accept(this); } ();

		}
		else
            () @trusted { withStatement.accept(this); } ();
	}

	override void visit(const ArgumentList list)
	{
		scope visitor = new ArgumentListVisitor(this);
        () @trusted { visitor.visit(list); } ();
	}

	alias visit = ASTVisitor.visit;

	/// Module scope
	Scope* moduleScope;

	/// The module
	SemanticSymbol* rootSymbol;

	/// Allocator used for symbol allocation
	IAllocator symbolAllocator;

	/// Number of symbols allocated
	uint symbolsAllocated;

private:

	void createConstructor()
	{
		import std.array : appender;
		import std.range : zip;

		auto app = appender!string();
		app.put("this(");
		bool first = true;
		foreach (field; zip(structFieldTypes[], structFieldNames[]))
		{
			if (first)
				first = false;
			else
				app.put(", ");
			if (field[0] is null)
				app.put("auto ");
			else
			{
				app.formatNode(field[0]);
				app.put(" ");
			}
			app.put(field[1].data);
		}
		app.put(")");
		SemanticSymbol* symbol = allocateSemanticSymbol(CONSTRUCTOR_SYMBOL_NAME,
			CompletionKind.functionName, symbolFile, currentSymbol.acSymbol.location);
		symbol.acSymbol.callTip = istring(app.data);
		currentSymbol.addChild(symbol, true);
	}

	void pushScope(size_t startLocation, size_t endLocation)
	{
		assert (startLocation < uint.max);
		assert (endLocation < uint.max || endLocation == size_t.max);
		Scope* s = () @trusted { return semanticAllocator.makeX!Scope(cast(uint) startLocation, cast(uint) endLocation); } ();
		s.parent = currentScope;
		currentScope.children.insert(s);
		currentScope = s;
	}

	void popScope()
	{
		currentScope = currentScope.parent;
	}

	void pushFunctionScope(const FunctionBody functionBody,
		IAllocator semanticAllocator, size_t scopeBegin)
	{
		Scope* s = () @trusted { return semanticAllocator.makeX!Scope(cast(uint) scopeBegin, cast(uint) functionBody.endLocation); } ();
		s.parent = currentScope;
		currentScope.children.insert(s);
		currentScope = s;
	}

	void pushSymbol(string name, CompletionKind kind, istring symbolFile,
		size_t location = 0, const Type type = null)
	{
		SemanticSymbol* symbol = allocateSemanticSymbol(name, kind, symbolFile,
			location);
		if (type !is null)
			addTypeToLookups(symbol.typeLookups, type);
		symbol.parent = currentSymbol;
		currentSymbol.addChild(symbol, true);
		currentScope.addSymbol(symbol.acSymbol, false);
		currentSymbol = symbol;
	}

	void popSymbol()
	{
		currentSymbol = currentSymbol.parent;
	}

	template visitEnumMember(T)
	{
		override void visit(const T member)
		{
			pushSymbol(member.name.text, CompletionKind.enumMember, symbolFile,
				member.name.index, member.type);
			scope(exit) popSymbol();
			currentSymbol.acSymbol.doc = makeDocumentation(member.comment);
		}
	}

	void visitAggregateDeclaration(AggType)(AggType dec, CompletionKind kind)
	{
		if ((kind == CompletionKind.unionName || kind == CompletionKind.structName) &&
			dec.name == tok!"")
		{
            () @trusted { dec.accept(this); } ();
			return;
		}
		pushSymbol(dec.name.text, kind, symbolFile, dec.name.index);
		scope(exit) popSymbol();

		if (kind == CompletionKind.className)
			currentSymbol.acSymbol.addChildren(classSymbols[], false);
		else
			currentSymbol.acSymbol.addChildren(aggregateSymbols[], false);
		currentSymbol.acSymbol.protection = protection.current;
		currentSymbol.acSymbol.doc = makeDocumentation(dec.comment);

		istring lastComment = this.lastComment;
		this.lastComment = istring.init;
		scope(exit) this.lastComment = lastComment;

		immutable size_t scopeBegin = dec.name.index + dec.name.text.length;
		static if (is (AggType == const(TemplateDeclaration)))
			immutable size_t scopeEnd = dec.endLocation;
		else
			immutable size_t scopeEnd = dec.structBody is null ? scopeBegin : dec.structBody.endLocation;
		pushScope(scopeBegin, scopeEnd);
		scope(exit) popScope();
		protection.beginScope();
		scope (exit) protection.endScope();
		processTemplateParameters(currentSymbol, dec.templateParameters);
        () @trusted { dec.accept(this); } ();
	}

	void visitConstructor(size_t location, const Parameters parameters,
		const TemplateParameters templateParameters,
		const FunctionBody functionBody, string doc)
	{
		SemanticSymbol* symbol = allocateSemanticSymbol(CONSTRUCTOR_SYMBOL_NAME,
			CompletionKind.functionName, symbolFile, location);
		symbol.parent = currentSymbol;
		currentSymbol.addChild(symbol, true);
		processParameters(symbol, null, THIS_SYMBOL_NAME, parameters, templateParameters);
		symbol.acSymbol.protection = protection.current;
		symbol.acSymbol.doc = makeDocumentation(doc);

		istring lastComment = this.lastComment;
		this.lastComment = istring.init;
		scope(exit) this.lastComment = lastComment;

		if (functionBody !is null)
		{
			pushFunctionScope(functionBody, semanticAllocator,
				location + 4); // 4 == "this".length
			scope(exit) popScope();
			currentSymbol = symbol;
            () @trusted { functionBody.accept(this); } ();
			currentSymbol = currentSymbol.parent;
		}
	}

	void visitDestructor(size_t location, const FunctionBody functionBody, string doc)
	{
		SemanticSymbol* symbol = allocateSemanticSymbol(DESTRUCTOR_SYMBOL_NAME,
			CompletionKind.functionName, symbolFile, location);
		symbol.parent = currentSymbol;
		currentSymbol.addChild(symbol, true);
		symbol.acSymbol.callTip = internString("~this()");
		symbol.acSymbol.protection = protection.current;
		symbol.acSymbol.doc = makeDocumentation(doc);

		istring lastComment = this.lastComment;
		this.lastComment = istring.init;
		scope(exit) this.lastComment = lastComment;

		if (functionBody !is null)
		{
			pushFunctionScope(functionBody, semanticAllocator, location + 4); // 4 == "this".length
			scope(exit) popScope();
			currentSymbol = symbol;
            () @trusted { functionBody.accept(this); } ();
			currentSymbol = currentSymbol.parent;
		}
	}

	void processParameters(SemanticSymbol* symbol, const Type returnType,
		string functionName, const Parameters parameters,
		const TemplateParameters templateParameters)
	{
		processTemplateParameters(symbol, templateParameters);
		if (includeParameterSymbols && parameters !is null)
		{
			foreach (const Parameter p; parameters.parameters)
			{
				SemanticSymbol* parameter = allocateSemanticSymbol(
					p.name.text, CompletionKind.variableName, symbolFile,
					p.name.index);
				if (p.type !is null)
					addTypeToLookups(parameter.typeLookups, p.type);
				parameter.parent = currentSymbol;
                () @trusted { currentSymbol.acSymbol.argNames.insert(parameter.acSymbol.name); } ();
				currentSymbol.addChild(parameter, true);
				currentScope.addSymbol(parameter.acSymbol, false);
			}
			if (parameters.hasVarargs)
			{
				SemanticSymbol* argptr = allocateSemanticSymbol(ARGPTR_SYMBOL_NAME,
					CompletionKind.variableName, istring(null), size_t.max);
				addTypeToLookups(argptr.typeLookups, argptrType);
				argptr.parent = currentSymbol;
				currentSymbol.addChild(argptr, true);
				currentScope.addSymbol(argptr.acSymbol, false);

				SemanticSymbol* arguments = allocateSemanticSymbol(
					ARGUMENTS_SYMBOL_NAME, CompletionKind.variableName,
					istring(null), size_t.max);
				addTypeToLookups(arguments.typeLookups, argumentsType);
				arguments.parent = currentSymbol;
				currentSymbol.addChild(arguments, true);
				currentScope.addSymbol(arguments.acSymbol, false);
			}
		}
		symbol.acSymbol.callTip = formatCallTip(returnType, functionName,
			parameters, templateParameters);
	}

	void processTemplateParameters(SemanticSymbol* symbol, const TemplateParameters templateParameters)
	{
		if (includeParameterSymbols && templateParameters !is null
				&& templateParameters.templateParameterList !is null)
		{
			foreach (const TemplateParameter p; templateParameters.templateParameterList.items)
			{
				string name;
				CompletionKind kind;
				size_t index;
				Rebindable!(const(Type)) type;
				if (p.templateAliasParameter !is null)
				{
					name = p.templateAliasParameter.identifier.text;
					kind = CompletionKind.aliasName;
					index = p.templateAliasParameter.identifier.index;
				}
				else if (p.templateTypeParameter !is null)
				{
					name = p.templateTypeParameter.identifier.text;
					kind = CompletionKind.aliasName;
					index = p.templateTypeParameter.identifier.index;
					// even if templates are not solved we can get the completions
					// for the type the template parameter implicitly converts to,
					// which is often useful for aggregate types.
					if (p.templateTypeParameter.colonType)
						type = p.templateTypeParameter.colonType;
					// otherwise just provide standard type properties
					else
						kind = CompletionKind.typeTmpParam;
				}
				else if (p.templateValueParameter !is null)
				{
					name = p.templateValueParameter.identifier.text;
					kind = CompletionKind.variableName;
					index = p.templateValueParameter.identifier.index;
					type = p.templateValueParameter.type;
				}
				else if (p.templateTupleParameter !is null)
				{
					name = p.templateTupleParameter.identifier.text;
					kind = CompletionKind.variadicTmpParam;
					index = p.templateTupleParameter.identifier.index;
				}
				else
					continue;
				SemanticSymbol* templateParameter = allocateSemanticSymbol(name,
					kind, symbolFile, index);
				if (type !is null)
					addTypeToLookups(templateParameter.typeLookups, type);

				if (p.templateTupleParameter !is null)
				{
					TypeLookup* tl = () @trusted { return Mallocator.instance.makeX!TypeLookup(istring(name), TypeLookupKind.varOrFunType); } ();
					templateParameter.typeLookups.insert(tl);
				}
				else if (p.templateTypeParameter && kind == CompletionKind.typeTmpParam)
				{
					TypeLookup* tl = () @trusted { return Mallocator.instance.makeX!TypeLookup(istring(name), TypeLookupKind.varOrFunType); } ();
					templateParameter.typeLookups.insert(tl);
				}

				templateParameter.parent = symbol;
				symbol.addChild(templateParameter, true);
				if (currentScope)
					currentScope.addSymbol(templateParameter.acSymbol, false);
			}
		}
	}

	istring formatCallTip(const Type returnType, string name,
		const Parameters parameters, const TemplateParameters templateParameters)
	{
		import std.array : appender;

		auto app = appender!string();
		if (returnType !is null)
		{
			app.formatNode(returnType);
			app.put(' ');
		}
		app.put(name);
		if (templateParameters !is null)
			app.formatNode(templateParameters);
		if (parameters is null)
			app.put("()");
		else
			app.formatNode(parameters);
		return istring(app.data);
	}

	void populateInitializer(T)(SemanticSymbol* symbol, const T initializer,
		bool appendForeach = false) @trusted
	{
		auto lookup = Mallocator.instance.makeX!TypeLookup(TypeLookupKind.initializer);
		scope visitor = new InitializerVisitor(lookup, appendForeach, this);
		symbol.typeLookups.insert(lookup);
		visitor.visit(initializer);
	}

	SemanticSymbol* allocateSemanticSymbol(string name, CompletionKind kind,
		istring symbolFile, size_t location = 0)
	in
	{
		assert (symbolAllocator !is null);
	}
	do
	{
		DSymbol* acSymbol = () @trusted { return makeX!DSymbol(symbolAllocator, istring(name), kind); } ();
		acSymbol.location = location;
		acSymbol.symbolFile = symbolFile;
		symbolsAllocated++;
		return () @trusted { return semanticAllocator.makeX!SemanticSymbol(acSymbol); } ();
	}

	void addTypeToLookups(ref UnrolledList!(TypeLookup*, Mallocator, false) lookups,
		const Type type, TypeLookup* l = null)
	{
		auto lookup = l !is null ? l : () @trusted { return Mallocator.instance.makeX!TypeLookup(TypeLookupKind.varOrFunType); } ();
		auto t2 = type.type2;
		if (t2.type !is null)
			addTypeToLookups(lookups, t2.type, lookup);
		else if (t2.superOrThis is tok!"this")
			() @trusted { lookup.breadcrumbs.insert(internString("this")); } ();
		else if (t2.superOrThis is tok!"super")
			() @trusted { lookup.breadcrumbs.insert(internString("super")); } ();
		else if (t2.builtinType !is tok!"")
			() @trusted { lookup.breadcrumbs.insert(getBuiltinTypeName(t2.builtinType)); } ();
		else if (t2.typeIdentifierPart !is null)
			writeIotcTo(t2.typeIdentifierPart, lookup.breadcrumbs);
		else
		{
			// TODO: Add support for typeof expressions
			// TODO: Add support for __vector
//			warning("typeof() and __vector are not yet supported");
		}

		foreach (suffix; type.typeSuffixes)
		{
			if (suffix.star != tok!"")
				continue;
			else if (suffix.type)
				() @trusted { lookup.breadcrumbs.insert(ASSOC_ARRAY_SYMBOL_NAME); } ();
			else if (suffix.array)
				() @trusted { lookup.breadcrumbs.insert(ARRAY_SYMBOL_NAME); } ();
			else if (suffix.star != tok!"")
				() @trusted { lookup.breadcrumbs.insert(POINTER_SYMBOL_NAME); } ();
			else if (suffix.delegateOrFunction != tok!"")
			{
				import std.array : appender;
				auto app = appender!string();
				formatNode(app, type);
				istring callTip = istring(app.data);
				// Insert the call tip and THEN the "function" string because
				// the breadcrumbs are processed in reverse order
				() @trusted { lookup.breadcrumbs.insert(callTip); } ();
				() @trusted { lookup.breadcrumbs.insert(FUNCTION_SYMBOL_NAME); } ();
			}
		}
		if (l is null)
			lookups.insert(lookup);
	}

	DocString makeDocumentation(string documentation)
	{
		if (documentation.isDitto)
			return DocString(lastComment, true);
		else
		{
			lastComment = internString(documentation);
			return DocString(lastComment, false);
		}
	}

	/// Current protection type
	ProtectionStack protection;

	/// Current scope
	Scope* currentScope;

	/// Current symbol
	SemanticSymbol* currentSymbol;

	/// Path to the file being converted
	istring symbolFile;

	/// Field types used for generating struct constructors if no constructor
	/// was defined
	// TODO: This should be `const Type`, but Rebindable and opEquals don't play
	// well together
	UnrolledList!(Type) structFieldTypes;

	/// Field names for struct constructor generation
	UnrolledList!(istring) structFieldNames;

	/// Last comment for ditto-ing
	istring lastComment;

	const Module mod;

	IAllocator semanticAllocator;

	Rebindable!(const ExpressionNode) feExpression;

	CacheEntry* entry;

	ModuleCache* cache;

	bool includeParameterSymbols;
	bool skipBaseClassesOfNewAnon;

	ubyte foreachTypeIndexOfInterest;
	ubyte foreachTypeIndex;
}

struct ProtectionStack
{
	invariant
	{
		import std.algorithm.iteration : filter, joiner, map;
		import std.conv:to;
		import std.range : walkLength;

		assert(stack.length == stack[].filter!(a => isProtection(a)
				|| a == tok!":" || a == tok!"{").walkLength(), to!string(stack[].map!(a => str(a)).joiner(", ")));
	}

	IdType currentForImport() const
	{
		return stack.empty ? tok!"default" : current();
	}

	IdType current() const
	{
		import std.algorithm.iteration : filter;
		import std.range : choose, only;

		IdType retVal;
		foreach (t; choose(stack.empty, only(tok!"public"), stack[]).filter!(
				a => a != tok!"{" && a != tok!":"))
			retVal = cast(IdType) t;
		return retVal;
	}

	void beginScope()
	{
		stack.insertBack(tok!"{");
	}

	void endScope()
	{
		import std.algorithm.iteration : joiner;
		import std.conv : to;
		import std.range : walkLength;

		while (!stack.empty && stack.back == tok!":")
		{
			assert(stack.length >= 2);
			() @trusted { stack.popBack(); } ();
			() @trusted { stack.popBack(); } ();
		}
		assert(stack.length == stack[].walkLength());
		assert(!stack.empty && stack.back == tok!"{", to!string(stack[].map!(a => str(a)).joiner(", ")));
		() @trusted { stack.popBack(); } ();
	}

	void beginLocal(const IdType t)
	{
		assert (t != tok!"", "DERP!");
		stack.insertBack(t);
	}

	void endLocal()
	{
		import std.algorithm.iteration : joiner;
		import std.conv : to;

		assert(!stack.empty && stack.back != tok!":" && stack.back != tok!"{",
				to!string(stack[].map!(a => str(a)).joiner(", ")));
		() @trusted { stack.popBack(); } ();
	}

	void addScope(const IdType t)
	{
		assert(t != tok!"", "DERP!");
		assert(isProtection(t));
		if (!stack.empty && stack.back == tok!":")
		{
			assert(stack.length >= 2);
			() @trusted { stack.popBack(); } ();
			assert(isProtection(stack.back));
			() @trusted { stack.popBack(); } ();
		}
		() @trusted { stack.insertBack(t); } ();
		() @trusted { stack.insertBack(tok!":"); } ();
	}

private:

	UnrolledList!IdType stack;
}

void formatNode(A, T)(ref A appender, const T node)
{
	if (node is null)
		return;
    () @trusted {
        scope f = new Formatter!(A*)(&appender);
        f.format(node);
    } ();
}

private:

bool isDitto(scope const(char)[] comment)
{
	import std.uni : icmp;

	return comment.length == 5 && icmp(comment, "ditto") == 0;
}

void writeIotcTo(T)(const TypeIdentifierPart tip, ref T output) nothrow
{
	if (!tip.identifierOrTemplateInstance)
		return;
	if (tip.identifierOrTemplateInstance.identifier != tok!"")
        () @trusted { output.insert(internString(tip.identifierOrTemplateInstance.identifier.text)); } ();
	else
        () @trusted { output.insert(internString(tip.identifierOrTemplateInstance.templateInstance.identifier.text)); } ();

	// the indexer of a TypeIdentifierPart means either that there's
	// a static array dimension or that a type is selected in a type list.
	// we can only handle the first case since dsymbol does not process templates yet.
	if (tip.indexer)
        () @trusted { output.insert(ARRAY_SYMBOL_NAME); } ();

	if (tip.typeIdentifierPart)
		writeIotcTo(tip.typeIdentifierPart, output);
}

auto byIdentifier(const IdentifierOrTemplateChain iotc) nothrow
{
	import std.algorithm : map;

	return iotc.identifiersOrTemplateInstances.map!(a => a.identifier == tok!""
		? a.templateInstance.identifier.text
		: a.identifier.text);
}

void writeIotcTo(T)(const IdentifierOrTemplateChain iotc, ref T output) nothrow
{
	import std.algorithm : each;

    () @trusted { byIdentifier(iotc).each!(a => output.insert(internString(a))); } ();
}

static istring convertChainToImportPath(const IdentifierChain ic)
{
	import std.path : dirSeparator;
	import std.array : appender;
	auto app = appender!string();
	foreach (i, ident; ic.identifiers)
	{
		app.put(ident.text);
		if (i + 1 < ic.identifiers.length)
			app.put(dirSeparator);
	}
	return istring(app.data);
}

class InitializerVisitor : ASTVisitor
{
	this (TypeLookup* lookup, bool appendForeach, FirstPass fp)
	{
		this.lookup = lookup;
		this.appendForeach = appendForeach;
		this.fp = fp;
	}

	alias visit = ASTVisitor.visit;

	override void visit(const FunctionLiteralExpression exp)
	{
		fp.visit(exp);
	}

	override void visit(const IdentifierOrTemplateInstance ioti)
	{
		if (on && ioti.identifier != tok!"")
			() @trusted { lookup.breadcrumbs.insert(internString(ioti.identifier.text)); } ();
		else if (on && ioti.templateInstance.identifier != tok!"")
			() @trusted { lookup.breadcrumbs.insert(internString(ioti.templateInstance.identifier.text)); } ();
		() @trusted { ioti.accept(this); } ();
	}

	override void visit(const PrimaryExpression primary)
	{
		// Add identifiers without processing. Convert literals to strings with
		// the prefix '*' so that that the second pass can tell the difference
		// between "int.abc" and "10.abc".
		if (on && primary.basicType != tok!"")
			() @trusted { lookup.breadcrumbs.insert(internString(str(primary.basicType.type))); } ();
		if (on) switch (primary.primary.type)
		{
		case tok!"identifier":
			() @trusted { lookup.breadcrumbs.insert(internString(primary.primary.text)); } ();
			break;
		case tok!"doubleLiteral":
			() @trusted { lookup.breadcrumbs.insert(DOUBLE_LITERAL_SYMBOL_NAME); } ();
			break;
		case tok!"floatLiteral":
			() @trusted { lookup.breadcrumbs.insert(FLOAT_LITERAL_SYMBOL_NAME); } ();
			break;
		case tok!"idoubleLiteral":
			() @trusted { lookup.breadcrumbs.insert(IDOUBLE_LITERAL_SYMBOL_NAME); } ();
			break;
		case tok!"ifloatLiteral":
			() @trusted { lookup.breadcrumbs.insert(IFLOAT_LITERAL_SYMBOL_NAME); } ();
			break;
		case tok!"intLiteral":
			() @trusted { lookup.breadcrumbs.insert(INT_LITERAL_SYMBOL_NAME); } ();
			break;
		case tok!"longLiteral":
			() @trusted { lookup.breadcrumbs.insert(LONG_LITERAL_SYMBOL_NAME); } ();
			break;
		case tok!"realLiteral":
			() @trusted { lookup.breadcrumbs.insert(REAL_LITERAL_SYMBOL_NAME); } ();
			break;
		case tok!"irealLiteral":
			() @trusted { lookup.breadcrumbs.insert(IREAL_LITERAL_SYMBOL_NAME); } ();
			break;
		case tok!"uintLiteral":
			() @trusted { lookup.breadcrumbs.insert(UINT_LITERAL_SYMBOL_NAME); } ();
			break;
		case tok!"ulongLiteral":
			() @trusted { lookup.breadcrumbs.insert(ULONG_LITERAL_SYMBOL_NAME); } ();
			break;
		case tok!"characterLiteral":
			() @trusted { lookup.breadcrumbs.insert(CHAR_LITERAL_SYMBOL_NAME); } ();
			break;
		case tok!"dstringLiteral":
			() @trusted { lookup.breadcrumbs.insert(DSTRING_LITERAL_SYMBOL_NAME); } ();
			break;
		case tok!"stringLiteral":
			() @trusted { lookup.breadcrumbs.insert(STRING_LITERAL_SYMBOL_NAME); } ();
			break;
		case tok!"wstringLiteral":
			() @trusted { lookup.breadcrumbs.insert(WSTRING_LITERAL_SYMBOL_NAME); } ();
			break;
		case tok!"false":
		case tok!"true":
			() @trusted { lookup.breadcrumbs.insert(BOOL_VALUE_SYMBOL_NAME); } ();
			break;
		default:
			break;
		}
		() @trusted { primary.accept(this); } ();
	}

	override void visit(const IndexExpression expr)
	{
		() @trusted { expr.unaryExpression.accept(this); } ();
		foreach (index; expr.indexes)
			if (index.high is null)
				() @trusted { lookup.breadcrumbs.insert(ARRAY_SYMBOL_NAME); } ();
	}

	override void visit(const Initializer initializer)
	{
		on = true;
		() @trusted { initializer.accept(this); } ();
		on = false;
	}

	override void visit(const ArrayInitializer ai)
	{
		// If the array has any elements, assume all elements have the
		// same type as the first element.
		if (ai.arrayMemberInitializations)
		{
			if (ai.arrayMemberInitializations.length)
				() @trusted { ai.arrayMemberInitializations[0].accept(this); } ();
			else
				() @trusted { lookup.breadcrumbs.insert(VOID_SYMBOL_NAME); } ();

		}
		() @trusted { lookup.breadcrumbs.insert(ARRAY_LITERAL_SYMBOL_NAME); } ();
	}

	override void visit(const ArrayLiteral al)
	{
		// ditto
		if (al.argumentList)
		{
			if (al.argumentList.items.length)
				() @trusted { al.argumentList.items[0].accept(this); } ();
			else
				() @trusted { lookup.breadcrumbs.insert(VOID_SYMBOL_NAME); } ();
		}
		() @trusted { lookup.breadcrumbs.insert(ARRAY_LITERAL_SYMBOL_NAME); } ();
	}

	// Skip it
	override void visit(const NewAnonClassExpression) {}

	override void visit(const NewExpression ne)
	{
		if (ne.newAnonClassExpression)
			() @trusted { lowerNewAnonToNew((cast() ne)); } ();
		() @trusted { ne.accept(this); } ();
	}

	private void lowerNewAnonToNew(NewExpression ne)
	{
		import std.format : format;

		// here we follow DMDFE naming style
        const idt = () @trusted {
            __gshared size_t anonIndex;
            return istring("__anonclass%d".format(++anonIndex));
        } ();

		// the goal is to replace it so we null the field
		NewAnonClassExpression nace = ne.newAnonClassExpression;
		ne.newAnonClassExpression = null;

		// Lower the AnonClass do to a standard ClassDeclaration and visit it.
		ClassDeclaration cd = () @trusted { return theAllocator.makeX!(ClassDeclaration); } ();
		cd.name = Token(tok!"identifier", idt, 1, 1, nace.structBody.startLocation - idt.length);
		cd.baseClassList = nace.baseClassList;
		cd.structBody = nace.structBody;
		fp.visit(cd);

		// Change the NewAnonClassExpression to a standard NewExpression using
		// the ClassDeclaration created in previous step
		ne.type = () @trusted { return theAllocator.makeX!(Type); } ();
		ne.type.type2 = () @trusted { return theAllocator.makeX!(Type2); } ();
		ne.type.type2.typeIdentifierPart = () @trusted { return theAllocator.makeX!(TypeIdentifierPart); } ();
		ne.type.type2.typeIdentifierPart.identifierOrTemplateInstance = () @trusted { return theAllocator.makeX!(IdentifierOrTemplateInstance); } ();
		ne.type.type2.typeIdentifierPart.identifierOrTemplateInstance.identifier = cd.name;
		ne.arguments = nace.constructorArguments;
	}

	override void visit(const ArgumentList list)
	{
		scope visitor = new ArgumentListVisitor(fp);
        () @trusted { visitor.visit(list); } ();
	}

	override void visit(const Expression expression)
	{
		on = true;
        () @trusted { expression.accept(this); } ();
		if (appendForeach)
            () @trusted { lookup.breadcrumbs.insert(internString("foreach")); } ();
		on = false;
	}

	override void visit(const ExpressionNode expression)
	{
		on = true;
        () @trusted { expression.accept(this); } ();
		if (appendForeach)
            () @trusted { lookup.breadcrumbs.insert(internString("foreach")); } ();
		on = false;
	}

	TypeLookup* lookup;
	bool on = false;
	const bool appendForeach;
	FirstPass fp;
}

class ArgumentListVisitor : ASTVisitor
{
	this(FirstPass fp)
	{
		assert(fp);
		this.fp = fp;
	}

	alias visit = ASTVisitor.visit;

	override void visit(const FunctionLiteralExpression exp)
	{
		fp.visit(exp);
	}

	override void visit(const NewAnonClassExpression exp)
	{
		fp.visit(exp);
	}

private:
	FirstPass fp;
}
