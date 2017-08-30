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
import std.algorithm.iteration : map;
import std.experimental.allocator;
import std.experimental.allocator.mallocator;
import std.experimental.logger;
import std.typecons;

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
final class FirstPass : ASTVisitor
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
		ModuleCache* cache, CacheEntry* entry = null)
	in
	{
		assert(mod);
		assert(symbolAllocator);
		assert(semanticAllocator);
		assert(cache);
	}
	body
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
		u.accept(this);
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
		currentSymbol.protection = protection.current;
		currentSymbol.acSymbol.doc = internString(dec.comment);

		if (dec.functionBody !is null)
		{
			pushFunctionScope(dec.functionBody, semanticAllocator,
					dec.name.index + dec.name.text.length);
			scope (exit) popScope();
			processParameters(currentSymbol, dec.returnType,
					currentSymbol.acSymbol.name, dec.parameters, dec.templateParameters);
			dec.functionBody.accept(this);
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

	override void visit(const BaseClass bc)
	{
		if (bc.type2.symbol is null || bc.type2.symbol.identifierOrTemplateChain is null)
			return;
		auto lookup = Mallocator.instance.make!TypeLookup(TypeLookupKind.inherit);
		writeIotcTo(bc.type2.symbol.identifierOrTemplateChain,
			lookup.breadcrumbs);
		currentSymbol.typeLookups.insert(lookup);
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
			symbol.protection = protection.current;
			symbol.acSymbol.doc = internString(declarator.comment);
			currentSymbol.addChild(symbol, true);
			currentScope.addSymbol(symbol.acSymbol, false);

			if (currentSymbol.acSymbol.kind == CompletionKind.structName)
			{
				structFieldNames.insert(symbol.acSymbol.name);
				// TODO: remove this cast. See the note on structFieldTypes
				structFieldTypes.insert(cast() dec.type);
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
				symbol.protection = protection.current;
				symbol.acSymbol.doc = internString(dec.comment);
				currentSymbol.addChild(symbol, true);
				currentScope.addSymbol(symbol.acSymbol, false);

				if (currentSymbol.acSymbol.kind == CompletionKind.structName)
				{
					structFieldNames.insert(symbol.acSymbol.name);
					// TODO: remove this cast. See the note on structFieldTypes
					structFieldTypes.insert(null);
				}
			}
		}
	}

	override void visit(const AliasDeclaration aliasDeclaration)
	{
		if (aliasDeclaration.initializers.length == 0)
		{
			foreach (name; aliasDeclaration.identifierList.identifiers)
			{
				SemanticSymbol* symbol = allocateSemanticSymbol(
					name.text, CompletionKind.aliasName, symbolFile, name.index);
				if (aliasDeclaration.type !is null)
					addTypeToLookups(symbol.typeLookups, aliasDeclaration.type);
				symbol.parent = currentSymbol;
				currentSymbol.addChild(symbol, true);
				currentScope.addSymbol(symbol.acSymbol, false);
				symbol.protection = protection.current;
				symbol.acSymbol.doc = internString(aliasDeclaration.comment);
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
				symbol.protection = protection.current;
				symbol.acSymbol.doc = internString(aliasDeclaration.comment);
			}
		}
	}

	override void visit(const AliasThisDeclaration dec)
	{
		currentSymbol.typeLookups.insert(Mallocator.instance.make!TypeLookup(
			internString(dec.identifier.text), TypeLookupKind.aliasThis));
	}

	override void visit(const Declaration dec)
	{
		if (dec.attributeDeclaration !is null
			&& isProtection(dec.attributeDeclaration.attribute.attribute.type))
		{
			protection.addScope(dec.attributeDeclaration.attribute.attribute.type);
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
				dec.accept(this);
				protection.endScope();
			}
			else
				dec.accept(this);
			protection.endLocal();
		}
		else
			dec.accept(this);
	}

	override void visit(const Module mod)
	{
		rootSymbol = allocateSemanticSymbol(null, CompletionKind.moduleName,
			symbolFile);
		currentSymbol = rootSymbol;
		moduleScope = semanticAllocator.make!Scope(0, uint.max);
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
		foreach (s; builtinSymbols[])
			currentScope.addSymbol(s, false);
		mod.accept(this);
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
		symbol.acSymbol.doc = internString(dec.comment);
		currentSymbol = symbol;

		if (dec.enumBody !is null)
		{
			pushScope(dec.enumBody.startLocation, dec.enumBody.endLocation);
			dec.enumBody.accept(this);
			popScope();
		}

		currentSymbol = currentSymbol.parent;
	}

	mixin visitEnumMember!EnumMember;
	mixin visitEnumMember!AnonymousEnumMember;

	override void visit(const ModuleDeclaration moduleDeclaration)
	{
		rootSymbol.acSymbol.name = internString(moduleDeclaration.moduleName.identifiers[$ - 1].text);
	}

	override void visit(const StructBody structBody)
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

		DSymbol* thisSymbol = make!DSymbol(symbolAllocator, THIS_SYMBOL_NAME,
			CompletionKind.variableName, currentSymbol.acSymbol);
		thisSymbol.location = currentScope.startLocation;
		thisSymbol.symbolFile = symbolFile;
		thisSymbol.type = currentSymbol.acSymbol;
		thisSymbol.ownType = false;
		currentScope.addSymbol(thisSymbol, false);

		foreach (dec; structBody.declarations)
			visit(dec);

		// If no constructor is found, generate one
		if (currentSymbol.acSymbol.kind == CompletionKind.structName
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
				warning("Could not resolve location of module '", importPath, "'");
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
							currentImportSymbol = symbolAllocator.make!DSymbol(ip, kind);
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
							auto sym = symbolAllocator.make!DSymbol(ip, kind);
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
				renameSymbol.acSymbol.skipOver = protection.current != tok!"public";
				renameSymbol.acSymbol.type = importSymbol.acSymbol;
				renameSymbol.acSymbol.ownType = true;
				renameSymbol.addChild(importSymbol, true);
				currentSymbol.addChild(renameSymbol, true);
				currentScope.addSymbol(renameSymbol.acSymbol, false);
			}
			if (entry !is null)
				entry.dependencies.insert(modulePath);
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
			TypeLookup* lookup = Mallocator.instance.make!TypeLookup(
				TypeLookupKind.selectiveImport);

			immutable bool isRenamed = bind.right != tok!"";

			// The second phase must change this `importSymbol` kind to
			// `aliasName` for symbol lookup to work.
			SemanticSymbol* importSymbol = allocateSemanticSymbol(
				isRenamed ? bind.left.text : IMPORT_SYMBOL_NAME,
				CompletionKind.importSymbol, modulePath);

			if (isRenamed)
			{
				lookup.breadcrumbs.insert(internString(bind.right.text));
				importSymbol.acSymbol.location = bind.left.index;
				importSymbol.acSymbol.altFile = symbolFile;
			}
			lookup.breadcrumbs.insert(internString(bind.left.text));

			importSymbol.acSymbol.qualifier = SymbolQualifier.selectiveImport;
			importSymbol.typeLookups.insert(lookup);
			importSymbol.acSymbol.skipOver = protection.current != tok!"public";
			currentSymbol.addChild(importSymbol, true);
			currentScope.addSymbol(importSymbol.acSymbol, false);
		}

		if (entry !is null)
			entry.dependencies.insert(modulePath);
	}

	// Create scope for block statements
	override void visit(const BlockStatement blockStatement)
	{
		if (blockStatement.declarationsAndStatements !is null)
		{
			pushScope(blockStatement.startLocation, blockStatement.endLocation);
			scope(exit) popScope();
			visit (blockStatement.declarationsAndStatements);
		}
	}

	override void visit(const TemplateMixinExpression tme)
	{
		// TODO: support typeof here
		if (tme.mixinTemplateName.symbol is null)
			return;
		auto lookup = Mallocator.instance.make!TypeLookup(TypeLookupKind.mixinTemplate);
		writeIotcTo(tme.mixinTemplateName.symbol.identifierOrTemplateChain,
			lookup.breadcrumbs);

		if (currentSymbol.acSymbol.kind != CompletionKind.functionName)
			currentSymbol.typeLookups.insert(lookup);
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
			feStatement.accept(this);
			feExpression = null;
		}
		else
		{
			const ubyte o1 = foreachTypeIndexOfInterest;
			const ubyte o2 = foreachTypeIndex;
			feStatement.accept(this);
			foreachTypeIndexOfInterest = o1;
			foreachTypeIndex = o2;
		}
	}

	override void visit(const ForeachTypeList feTypeList)
	{
		foreachTypeIndex = 0;
		foreachTypeIndexOfInterest = cast(ubyte)(feTypeList.items.length - 1);
		feTypeList.accept(this);
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

	override void visit(const WithStatement withStatement)
	{
		if (withStatement.expression !is null
			&& withStatement.statementNoCaseNoDefault !is null)
		{
			pushScope(withStatement.statementNoCaseNoDefault.startLocation,
				withStatement.statementNoCaseNoDefault.endLocation);
			scope(exit) popScope();

			pushSymbol(WITH_SYMBOL_NAME, CompletionKind.withSymbol, symbolFile,
				currentScope.startLocation, null);
			scope(exit) popSymbol();

			populateInitializer(currentSymbol, withStatement.expression, false);
			withStatement.accept(this);

		}
		else
			withStatement.accept(this);
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

		auto app = appender!(char[])();
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
			app.put(field[1]);
		}
		app.put(")");
		SemanticSymbol* symbol = allocateSemanticSymbol(CONSTRUCTOR_SYMBOL_NAME,
			CompletionKind.functionName, symbolFile, currentSymbol.acSymbol.location);
		symbol.acSymbol.callTip = internString(cast(string) app.data);
		currentSymbol.addChild(symbol, true);
	}

	void pushScope(size_t startLocation, size_t endLocation)
	{
		assert (startLocation < uint.max);
		assert (endLocation < uint.max || endLocation == ulong.max);
		Scope* s = semanticAllocator.make!Scope(cast(uint) startLocation, cast(uint) endLocation);
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
		import std.algorithm : max;

		immutable scopeEnd = max(
			functionBody.inStatement is null ? 0 : functionBody.inStatement.blockStatement.endLocation,
			functionBody.outStatement is null ? 0 : functionBody.outStatement.blockStatement.endLocation,
			functionBody.blockStatement is null ? 0 : functionBody.blockStatement.endLocation,
			functionBody.bodyStatement is null ? 0 : functionBody.bodyStatement.blockStatement.endLocation);
		Scope* s = semanticAllocator.make!Scope(cast(uint) scopeBegin, cast(uint) scopeEnd);
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
			currentSymbol.acSymbol.doc = internString(member.comment);
		}
	}

	void visitAggregateDeclaration(AggType)(AggType dec, CompletionKind kind)
	{
		if (kind == CompletionKind.unionName && dec.name == tok!"")
		{
			dec.accept(this);
			return;
		}
		pushSymbol(dec.name.text, kind, symbolFile, dec.name.index);
		scope(exit) popSymbol();

		if (kind == CompletionKind.className)
			currentSymbol.acSymbol.addChildren(classSymbols[], false);
		else
			currentSymbol.acSymbol.addChildren(aggregateSymbols[], false);
		currentSymbol.protection = protection.current;
		currentSymbol.acSymbol.doc = internString(dec.comment);

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
		dec.accept(this);
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
		symbol.protection = protection.current;
		symbol.acSymbol.doc = internString(doc);
		if (functionBody !is null)
		{
			pushFunctionScope(functionBody, semanticAllocator,
				location + 4); // 4 == "this".length
			scope(exit) popScope();
			currentSymbol = symbol;
			functionBody.accept(this);
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
		symbol.protection = protection.current;
		symbol.acSymbol.doc = internString(doc);
		if (functionBody !is null)
		{
			pushFunctionScope(functionBody, semanticAllocator, location + 4); // 4 == "this".length
			scope(exit) popScope();
			currentSymbol = symbol;
			functionBody.accept(this);
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
				currentSymbol.acSymbol.argNames.insert(parameter.acSymbol.name);
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
				}
				else if (p.templateValueParameter !is null)
				{
					name = p.templateValueParameter.identifier.text;
					kind = CompletionKind.variableName;
					index = p.templateValueParameter.identifier.index;
					type = p.templateValueParameter.type;
				}
				else
					continue;
				SemanticSymbol* templateParameter = allocateSemanticSymbol(name,
					kind, symbolFile, index);
				if (type !is null)
					addTypeToLookups(templateParameter.typeLookups, type);
				templateParameter.parent = symbol;
				symbol.addChild(templateParameter, true);
			}
		}
	}

	istring formatCallTip(const Type returnType, string name,
		const Parameters parameters, const TemplateParameters templateParameters)
	{
		import std.array : appender;

		auto app = appender!(char[])();
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
		return internString(cast(string) app.data);
	}

	void populateInitializer(T)(SemanticSymbol* symbol, const T initializer,
		bool appendForeach = false)
	{
		auto lookup = Mallocator.instance.make!TypeLookup(TypeLookupKind.initializer);
		auto visitor = scoped!InitializerVisitor(lookup, appendForeach);
		symbol.typeLookups.insert(lookup);
		visitor.visit(initializer);
	}

	SemanticSymbol* allocateSemanticSymbol(string name, CompletionKind kind,
		istring symbolFile, size_t location = 0)
	in
	{
		assert (symbolAllocator !is null);
	}
	body
	{
		DSymbol* acSymbol = make!DSymbol(symbolAllocator, name, kind);
		acSymbol.location = location;
		acSymbol.symbolFile = symbolFile;
		symbolsAllocated++;
		return semanticAllocator.make!SemanticSymbol(acSymbol);
	}

	void addTypeToLookups(ref UnrolledList!(TypeLookup*, Mallocator, false) lookups,
		const Type type, TypeLookup* l = null)
	{
		auto lookup = l !is null ? l : Mallocator.instance.make!TypeLookup(
			TypeLookupKind.varOrFunType);
		auto t2 = type.type2;
		if (t2.type !is null)
			addTypeToLookups(lookups, t2.type, lookup);
		else if (t2.superOrThis is tok!"this")
			lookup.breadcrumbs.insert(internString("this"));
		else if (t2.superOrThis is tok!"super")
			lookup.breadcrumbs.insert(internString("super"));
		else if (t2.builtinType !is tok!"")
			lookup.breadcrumbs.insert(getBuiltinTypeName(t2.builtinType));
		else if (t2.symbol !is null)
			writeIotcTo(t2.symbol.identifierOrTemplateChain, lookup.breadcrumbs);
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
				lookup.breadcrumbs.insert(ASSOC_ARRAY_SYMBOL_NAME);
			else if (suffix.array)
				lookup.breadcrumbs.insert(ARRAY_SYMBOL_NAME);
			else if (suffix.star != tok!"")
				lookup.breadcrumbs.insert(POINTER_SYMBOL_NAME);
			else if (suffix.delegateOrFunction != tok!"")
			{
				import std.array : appender;
				auto app = appender!(char[])();
				formatNode(app, type);
				istring callTip = internString(cast(string) app.data);
				// Insert the call tip and THEN the "function" string because
				// the breadcrumbs are processed in reverse order
				lookup.breadcrumbs.insert(callTip);
				lookup.breadcrumbs.insert(FUNCTION_SYMBOL_NAME);
			}
		}
		if (l is null)
			lookups.insert(lookup);
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

	const Module mod;

	IAllocator semanticAllocator;

	Rebindable!(const ExpressionNode) feExpression;

	CacheEntry* entry;

	ModuleCache* cache;

	bool includeParameterSymbols;

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
			stack.popBack();
			stack.popBack();
		}
		assert(stack.length == stack[].walkLength());
		assert(!stack.empty && stack.back == tok!"{", to!string(stack[].map!(a => str(a)).joiner(", ")));
		stack.popBack();
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
		stack.popBack();
	}

	void addScope(const IdType t)
	{
		assert(t != tok!"", "DERP!");
		assert(isProtection(t));
		if (!stack.empty && stack.back == tok!":")
		{
			assert(stack.length >= 2);
			stack.popBack();
			assert(isProtection(stack.back));
			stack.popBack();
		}
		stack.insertBack(t);
		stack.insertBack(tok!":");
	}

private:

	UnrolledList!IdType stack;
}

void formatNode(A, T)(ref A appender, const T node)
{
	if (node is null)
		return;
	auto f = scoped!(Formatter!(A*))(&appender);
	f.format(node);
}

private:

auto byIdentifier(const IdentifierOrTemplateChain iotc)
{
	import std.algorithm : map;

	return iotc.identifiersOrTemplateInstances.map!(a => a.identifier == tok!""
		? a.templateInstance.identifier.text
		: a.identifier.text);
}

void writeIotcTo(T)(const IdentifierOrTemplateChain iotc, ref T output)
{
	import std.algorithm : each;

	byIdentifier(iotc).each!(a => output.insert(internString(a)));
}

static istring convertChainToImportPath(const IdentifierChain ic)
{
	import std.path : dirSeparator;
	import std.array : appender;
	auto app = appender!(char[])();
	foreach (i, ident; ic.identifiers)
	{
		app.put(ident.text);
		if (i + 1 < ic.identifiers.length)
			app.put(dirSeparator);
	}
	return internString(cast(string) app.data);
}

class InitializerVisitor : ASTVisitor
{
	this (TypeLookup* lookup, bool appendForeach = false)
	{
		this.lookup = lookup;
		this.appendForeach = appendForeach;
	}

	alias visit = ASTVisitor.visit;

	override void visit(const IdentifierOrTemplateInstance ioti)
	{
		if (on && ioti.identifier != tok!"")
			lookup.breadcrumbs.insert(internString(ioti.identifier.text));
		else if (on && ioti.templateInstance.identifier != tok!"")
			lookup.breadcrumbs.insert(internString(ioti.templateInstance.identifier.text));
		ioti.accept(this);
	}

	override void visit(const PrimaryExpression primary)
	{
		// Add identifiers without processing. Convert literals to strings with
		// the prefix '*' so that that the second pass can tell the difference
		// between "int.abc" and "10.abc".
		if (on && primary.basicType != tok!"")
			lookup.breadcrumbs.insert(internString(str(primary.basicType.type)));
		if (on) switch (primary.primary.type)
		{
		case tok!"identifier":
			lookup.breadcrumbs.insert(internString(primary.primary.text));
			break;
		case tok!"doubleLiteral":
			lookup.breadcrumbs.insert(DOUBLE_LITERAL_SYMBOL_NAME);
			break;
		case tok!"floatLiteral":
			lookup.breadcrumbs.insert(FLOAT_LITERAL_SYMBOL_NAME);
			break;
		case tok!"idoubleLiteral":
			lookup.breadcrumbs.insert(IDOUBLE_LITERAL_SYMBOL_NAME);
			break;
		case tok!"ifloatLiteral":
			lookup.breadcrumbs.insert(IFLOAT_LITERAL_SYMBOL_NAME);
			break;
		case tok!"intLiteral":
			lookup.breadcrumbs.insert(INT_LITERAL_SYMBOL_NAME);
			break;
		case tok!"longLiteral":
			lookup.breadcrumbs.insert(LONG_LITERAL_SYMBOL_NAME);
			break;
		case tok!"realLiteral":
			lookup.breadcrumbs.insert(REAL_LITERAL_SYMBOL_NAME);
			break;
		case tok!"irealLiteral":
			lookup.breadcrumbs.insert(IREAL_LITERAL_SYMBOL_NAME);
			break;
		case tok!"uintLiteral":
			lookup.breadcrumbs.insert(UINT_LITERAL_SYMBOL_NAME);
			break;
		case tok!"ulongLiteral":
			lookup.breadcrumbs.insert(ULONG_LITERAL_SYMBOL_NAME);
			break;
		case tok!"characterLiteral":
			lookup.breadcrumbs.insert(CHAR_LITERAL_SYMBOL_NAME);
			break;
		case tok!"dstringLiteral":
			lookup.breadcrumbs.insert(DSTRING_LITERAL_SYMBOL_NAME);
			break;
		case tok!"stringLiteral":
			lookup.breadcrumbs.insert(STRING_LITERAL_SYMBOL_NAME);
			break;
		case tok!"wstringLiteral":
			lookup.breadcrumbs.insert(WSTRING_LITERAL_SYMBOL_NAME);
			break;
		case tok!"false":
		case tok!"true":
			lookup.breadcrumbs.insert(BOOL_VALUE_SYMBOL_NAME);
			break;
		default:
			break;
		}
		primary.accept(this);
	}

	override void visit(const IndexExpression index)
	{
		lookup.breadcrumbs.insert(ARRAY_SYMBOL_NAME);
		index.accept(this);
	}

	override void visit(const Initializer initializer)
	{
		on = true;
		initializer.accept(this);
		on = false;
	}

	override void visit(const ArrayInitializer ai)
	{
		lookup.breadcrumbs.insert(ARRAY_SYMBOL_NAME);
		ai.accept(this);
	}

	// Skip these
	override void visit(const ArgumentList) {}
	override void visit(const NewAnonClassExpression) {}

	override void visit(const Expression expression)
	{
		on = true;
		expression.accept(this);
		if (appendForeach)
			lookup.breadcrumbs.insert(internString("foreach"));
		on = false;
	}

	override void visit(const ExpressionNode expression)
	{
		on = true;
		expression.accept(this);
		if (appendForeach)
			lookup.breadcrumbs.insert(internString("foreach"));
		on = false;
	}

	TypeLookup* lookup;
	bool on = false;
	const bool appendForeach;
}
