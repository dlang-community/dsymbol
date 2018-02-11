module dsymbol.tests;

import std.experimental.allocator;
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
    q{auto b = [0];}.expectSymbolsAndTypes([["b", "*arr*", "int"]]);
    q{auto b = [[0]];}.expectSymbolsAndTypes([["b", "*arr*", "*arr*", "int"]]);
    q{auto b = [[[0]]];}.expectSymbolsAndTypes([["b", "*arr*", "*arr*", "*arr*", "int"]]);
    //q{int* b;}.expectSymbolsAndTypes([["b", "*", "int"]]);
    //q{int*[] b;}.expectSymbolsAndTypes([["b", "*arr*", "*", "int"]]);
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
