module dsymbol.tests;

import stdx.allocator;
import dparse.ast, dparse.parser, dparse.lexer, dparse.rollback_allocator;
import dsymbol.cache_entry, dsymbol.modulecache, dsymbol.symbol;
import dsymbol.conversion, dsymbol.conversion.first, dsymbol.conversion.second;
import dsymbol.semantic, dsymbol.string_interning, dsymbol.builtin.names;
import std.file, std.path, std.format;
import std.stdio : writeln, stdout;
import std.typecons : scoped;

/**
 * Parses `source`, caches its symbols and compares the the cache content
 * with the `results`.
 *
 * Params:
 *      source = The source code to test.
 *      results = An array of string array. Each slot represents the variable name
 *      followed by the type strings.
 */
version (unittest):
void expectSymbolsAndTypes(const string source, const string[][] results,
	string file = __FILE_FULL_PATH__, size_t line = __LINE__)
{
	import core.exception : AssertError;
	import std.exception : enforce;

	ModuleCache mcache = ModuleCache(theAllocator);
	auto pair = generateAutocompleteTrees(source, mcache);
	scope(exit) pair.destroy();

	size_t i;
	foreach(ss; (*pair.symbol)[])
	{
		if (ss.type)
		{
			enforce!AssertError(i <= results.length, "not enough results", file, line);
			enforce!AssertError(results[i].length > 1,
				"at least one type must be present in a result row", file, line);
			enforce!AssertError(ss.name == results[i][0],
				"expected variableName: `%s` but got `%s`".format(results[i][0], ss.name),
				file, line);

			auto t = cast() ss.type;
			foreach (immutable j; 1..results[i].length)
			{
				enforce!AssertError(t != null, "null symbol", file, line);
				enforce!AssertError(t.name == results[i][j],
					"expected typeName: `%s` but got `%s`".format(results[i][j], t.name),
					file, line);
				if (t.type is t && t.name.length && t.name[0] != '*')
					break;
				t = t.type;
			}
			i++;
		}
	}
}

@system unittest
{
	writeln("Running type deduction tests...");
	q{bool b; int i;}.expectSymbolsAndTypes([["b", "bool"],["i", "int"]]);
	q{auto b = false;}.expectSymbolsAndTypes([["b", "bool"]]);
	q{auto b = true;}.expectSymbolsAndTypes([["b", "bool"]]);
	q{auto b = [0];}.expectSymbolsAndTypes([["b", "*arr-literal*", "int"]]);
	q{auto b = [[0]];}.expectSymbolsAndTypes([["b", "*arr-literal*", "*arr-literal*", "int"]]);
	q{auto b = [[[0]]];}.expectSymbolsAndTypes([["b", "*arr-literal*", "*arr-literal*", "*arr-literal*", "int"]]);
	q{auto b = [];}.expectSymbolsAndTypes([["b", "*arr-literal*", "void"]]);
	q{auto b = [[]];}.expectSymbolsAndTypes([["b", "*arr-literal*", "*arr-literal*", "void"]]);
	//q{int* b;}.expectSymbolsAndTypes([["b", "*", "int"]]);
	//q{int*[] b;}.expectSymbolsAndTypes([["b", "*arr*", "*", "int"]]);

	q{auto b = new class {int i;};}.expectSymbolsAndTypes([["b", "__anonclass1"]]);

	// got a crash before but solving is not yet working ("foo" instead of  "__anonclass1");
	q{class Bar{} auto foo(){return new class Bar{};} auto b = foo();}.expectSymbolsAndTypes([["b", "foo"]]);
}

// this one used to crash, see #125
unittest
{
	ModuleCache cache = ModuleCache(theAllocator);
	auto source = q{ auto a = true ? [42] : []; };
	auto pair = generateAutocompleteTrees(source, cache);
}

// https://github.com/dlang-community/D-Scanner/issues/749
unittest
{
	ModuleCache cache = ModuleCache(theAllocator);
	auto source = q{ void test() { foo(new class A {});}  };
	auto pair = generateAutocompleteTrees(source, cache);
}

// https://github.com/dlang-community/D-Scanner/issues/738
unittest
{
	ModuleCache cache = ModuleCache(theAllocator);
	auto source = q{ void b() { c = } alias b this;  };
	auto pair = generateAutocompleteTrees(source, cache);
}

unittest
{
	ModuleCache cache = ModuleCache(theAllocator);

	writeln("Running struct constructor tests...");
	auto source = q{ struct A {int a; struct B {bool b;} int c;} };
	auto pair = generateAutocompleteTrees(source, cache);
	auto A = pair.symbol.getFirstPartNamed(internString("A"));
	auto B = A.getFirstPartNamed(internString("B"));
	auto ACtor = A.getFirstPartNamed(CONSTRUCTOR_SYMBOL_NAME);
	auto BCtor = B.getFirstPartNamed(CONSTRUCTOR_SYMBOL_NAME);
	assert(ACtor.callTip == "this(int a, int c)");
	assert(BCtor.callTip == "this(bool b)");
}

unittest
{
	ModuleCache cache = ModuleCache(theAllocator);

	writeln("Running union constructor tests...");
	auto source = q{ union A {int a; bool b;} };
	auto pair = generateAutocompleteTrees(source, cache);
	auto A = pair.symbol.getFirstPartNamed(internString("A"));
	auto ACtor = A.getFirstPartNamed(CONSTRUCTOR_SYMBOL_NAME);
	assert(ACtor.callTip == "this(int a, bool b)");
}

unittest
{
	ModuleCache cache = ModuleCache(theAllocator);
	writeln("Running non-importable symbols tests...");
	auto source = q{
		class A { this(int a){} }
		class B : A {}
		class C { A f; alias f this; }
	};
	auto pair = generateAutocompleteTrees(source, cache);
	auto A = pair.symbol.getFirstPartNamed(internString("A"));
	auto B = pair.symbol.getFirstPartNamed(internString("B"));
	auto C = pair.symbol.getFirstPartNamed(internString("C"));
	assert(A.getFirstPartNamed(CONSTRUCTOR_SYMBOL_NAME) !is null);
	assert(B.getFirstPartNamed(CONSTRUCTOR_SYMBOL_NAME) is null);
	assert(C.getFirstPartNamed(CONSTRUCTOR_SYMBOL_NAME) is null);
}

unittest
{
	ModuleCache cache = ModuleCache(theAllocator);

	writeln("Running alias this tests...");
	auto source = q{ struct A {int f;} struct B { A a; alias a this; void fun() { auto var = f; };} };
	auto pair = generateAutocompleteTrees(source, cache);
	auto A = pair.symbol.getFirstPartNamed(internString("A"));
	auto B = pair.symbol.getFirstPartNamed(internString("B"));
	auto Af = A.getFirstPartNamed(internString("f"));
	auto fun = B.getFirstPartNamed(internString("fun"));
	auto var = fun.getFirstPartNamed(internString("var"));
	assert(Af is pair.scope_.getFirstSymbolByNameAndCursor(internString("f"), var.location));
}

unittest
{
	ModuleCache cache = ModuleCache(theAllocator);

	writeln("Running anon struct tests...");
	auto source = q{ struct A { struct {int a;}} };
	auto pair = generateAutocompleteTrees(source, cache);
	auto A = pair.symbol.getFirstPartNamed(internString("A"));
	assert(A);
	auto Aa = A.getFirstPartNamed(internString("a"));
	assert(Aa);
}

unittest
{
	ModuleCache cache = ModuleCache(theAllocator);

	writeln("Running the deduction from index expr tests...");
	{
		auto source = q{struct S{} S[] s; auto b = s[i];};
		auto pair = generateAutocompleteTrees(source, cache);
		DSymbol* S = pair.symbol.getFirstPartNamed(internString("S"));
		DSymbol* b = pair.symbol.getFirstPartNamed(internString("b"));
		assert(S);
		assert(b.type is S);
	}
	{
		auto source = q{struct S{} S[1] s; auto b = s[i];};
		auto pair = generateAutocompleteTrees(source, cache);
		DSymbol* S = pair.symbol.getFirstPartNamed(internString("S"));
		DSymbol* b = pair.symbol.getFirstPartNamed(internString("b"));
		assert(S);
		assert(b.type is S);
	}
	{
		auto source = q{struct S{} S[][] s; auto b = s[0];};
		auto pair = generateAutocompleteTrees(source, cache);
		DSymbol* S = pair.symbol.getFirstPartNamed(internString("S"));
		DSymbol* b = pair.symbol.getFirstPartNamed(internString("b"));
		assert(S);
		assert(b.type.type is S);
	}
	{
		auto source = q{struct S{} S[][][] s; auto b = s[0][0];};
		auto pair = generateAutocompleteTrees(source, cache);
		DSymbol* S = pair.symbol.getFirstPartNamed(internString("S"));
		DSymbol* b = pair.symbol.getFirstPartNamed(internString("b"));
		assert(S);
		assert(b.type.name == ARRAY_SYMBOL_NAME);
		assert(b.type.type is S);
	}
	{
		auto source = q{struct S{} S s; auto b = [s][0];};
		auto pair = generateAutocompleteTrees(source, cache);
		DSymbol* S = pair.symbol.getFirstPartNamed(internString("S"));
		DSymbol* b = pair.symbol.getFirstPartNamed(internString("b"));
		assert(S);
		assert(b.type is S);
	}
}

unittest
{
	ModuleCache cache = ModuleCache(theAllocator);

	writeln("Running `super` tests...");
	auto source = q{ class A {} class B : A {} };
	auto pair = generateAutocompleteTrees(source, cache);
	assert(pair.symbol);
	auto A = pair.symbol.getFirstPartNamed(internString("A"));
	auto B = pair.symbol.getFirstPartNamed(internString("B"));
	auto scopeA = (pair.scope_.getScopeByCursor(A.location + A.name.length));
	auto scopeB = (pair.scope_.getScopeByCursor(B.location + B.name.length));
	assert(scopeA !is scopeB);

	assert(!scopeA.getSymbolsByName(SUPER_SYMBOL_NAME).length);
	assert(scopeB.getSymbolsByName(SUPER_SYMBOL_NAME)[0].type is A);
}

unittest
{
	ModuleCache cache = ModuleCache(theAllocator);

	writeln("Running the \"access chain with inherited type\" tests...");
	auto source = q{ class A {} class B : A {} };
	auto pair = generateAutocompleteTrees(source, cache);
	assert(pair.symbol);
	auto A = pair.symbol.getFirstPartNamed(internString("A"));
	assert(A);
	auto B = pair.symbol.getFirstPartNamed(internString("B"));
	assert(B);
	auto AfromB = B.getFirstPartNamed(internString("A"));
	assert(AfromB.kind == CompletionKind.aliasName);
	assert(AfromB.type is A);
}

unittest
{
	ModuleCache cache = ModuleCache(theAllocator);

	writeln("Running template type parameters tests...");
	{
		auto source = q{ struct Foo(T : int){} struct Bar(T : Foo){} };
		auto pair = generateAutocompleteTrees(source, "", 0, cache);
		DSymbol* T1 = pair.symbol.getFirstPartNamed(internString("Foo"));
		DSymbol* T2 = T1.getFirstPartNamed(internString("T"));
		assert(T2.type.name == "int");
		DSymbol* T3 = pair.symbol.getFirstPartNamed(internString("Bar"));
		DSymbol* T4 = T3.getFirstPartNamed(internString("T"));
		assert(T4.type);
		assert(T4.type == T1);
	}
	{
		auto source = q{ struct Foo(T){ }};
		auto pair = generateAutocompleteTrees(source, "", 0, cache);
		DSymbol* T1 = pair.symbol.getFirstPartNamed(internString("Foo"));
		assert(T1);
		DSymbol* T2 = T1.getFirstPartNamed(internString("T"));
		assert(T2);
		assert(T2.kind == CompletionKind.typeTmpParam);
	}
}

unittest
{
	ModuleCache cache = ModuleCache(theAllocator);

	writeln("Running template variadic parameters tests...");
	auto source = q{ struct Foo(T...){ }};
	auto pair = generateAutocompleteTrees(source, "", 0, cache);
	DSymbol* T1 = pair.symbol.getFirstPartNamed(internString("Foo"));
	assert(T1);
	DSymbol* T2 = T1.getFirstPartNamed(internString("T"));
	assert(T2);
	assert(T2.kind == CompletionKind.variadicTmpParam);
}

// this is for testing that internString data is always on the same address
// since we use this special property for modulecache recursion guard
unittest
{
	istring a = internString("foo_bar_baz".idup);
	istring b = internString("foo_bar_baz".idup);
	assert(a.data.ptr == b.data.ptr);
}

static StringCache stringCache = void;
static this()
{
	stringCache = StringCache(StringCache.defaultBucketCount);
}

const(Token)[] lex(string source)
{
	return lex(source, null);
}

const(Token)[] lex(string source, string filename)
{
	import dparse.lexer : getTokensForParser;
	import std.string : representation;
	LexerConfig config;
	config.fileName = filename;
	return getTokensForParser(source.dup.representation, config, &stringCache);
}

unittest
{
	auto tokens = lex(q{int a = 9;});
	foreach(i, t;
		cast(IdType[]) [tok!"int", tok!"identifier", tok!"=", tok!"intLiteral", tok!";"])
	{
		assert(tokens[i] == t);
	}
	assert(tokens[1].text == "a", tokens[1].text);
	assert(tokens[3].text == "9", tokens[3].text);
}

string randomDFilename()
{
	import std.uuid : randomUUID;
	return "dsymbol_" ~ randomUUID().toString() ~ ".d";
}

ScopeSymbolPair generateAutocompleteTrees(string source, ref ModuleCache cache)
{
	return generateAutocompleteTrees(source, randomDFilename, cache);
}

ScopeSymbolPair generateAutocompleteTrees(string source, string filename, ref ModuleCache cache)
{
	auto tokens = lex(source);
	RollbackAllocator rba;
	Module m = parseModule(tokens, filename, &rba);

	auto first = scoped!FirstPass(m, internString(filename),
			theAllocator, theAllocator, true, &cache);
	first.run();

	secondPass(first.rootSymbol, first.moduleScope, cache);
	auto r = first.rootSymbol.acSymbol;
	typeid(SemanticSymbol).destroy(first.rootSymbol);
	return ScopeSymbolPair(r, first.moduleScope);
}

ScopeSymbolPair generateAutocompleteTrees(string source, size_t cursorPosition, ref ModuleCache cache)
{
	return generateAutocompleteTrees(source, null, cache);
}

ScopeSymbolPair generateAutocompleteTrees(string source, string filename, size_t cursorPosition, ref ModuleCache cache)
{
	auto tokens = lex(source);
	RollbackAllocator rba;
	return dsymbol.conversion.generateAutocompleteTrees(
		tokens, theAllocator, &rba, cursorPosition, cache);
}
