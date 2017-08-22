module dsymbol.tests;

import std.experimental.allocator;
import dparse.ast, dparse.parser, dparse.lexer, dparse.rollback_allocator;
import dsymbol.cache_entry, dsymbol.modulecache, dsymbol.symbol;
import dsymbol.conversion, dsymbol.conversion.first, dsymbol.conversion.second;
import dsymbol.semantic, dsymbol.string_interning;
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

    static immutable rName = "dsymbol-test-154251542-56564105-78944416-98523170-02452336.d";
    auto fName = tempDir ~ dirSeparator ~ rName;
    fName.write(source);
    scope(exit)
        remove(fName);

    ModuleCache mcache = ModuleCache(theAllocator);
    mcache.cacheModule(fName);

    size_t i;
    foreach (const(CacheEntry)* s; mcache.getAllSymbols)
        foreach(ss; (*s.symbol)[])
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
    import std.meta : AliasSeq;

    auto tokens = lex(q{int a = 9;});
    foreach(i, t;
        AliasSeq!(tok!"int", tok!"identifier", tok!"=", tok!"intLiteral", tok!";"))
    {
        assert(tokens[i] == t);
    }
    assert(tokens[1].text == "a", tokens[1].text);
    assert(tokens[3].text == "9", tokens[3].text);
}

string randomDFilename()
{
    import std.uuid : randomUUID;
    import std.string : translate;
    return "dsymbol_" ~ randomUUID().toString().translate(['-': '_']) ~ ".d";
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
