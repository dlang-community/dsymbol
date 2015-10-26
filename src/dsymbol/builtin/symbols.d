module dsymbol.builtin.symbols;

import dsymbol.symbol;
import dsymbol.builtin.names;
import dsymbol.string_interning;
import containers.ttree;
import std.experimental.allocator;
import std.experimental.allocator.mallocator;
import std.d.lexer;

/**
 * Symbols for the built in types
 */
TTree!(DSymbol*, Mallocator, true, "a < b", false) builtinSymbols;

/**
 * Array properties
 */
TTree!(DSymbol*, Mallocator, true, "a < b", false) arraySymbols;

/**
 * Associative array properties
 */
TTree!(DSymbol*, Mallocator, true, "a < b", false) assocArraySymbols;

/**
 * Struct, enum, union, class, and interface properties
 */
TTree!(DSymbol*, Mallocator, true, "a < b", false) aggregateSymbols;

/**
 * Class properties
 */
TTree!(DSymbol*, Mallocator, true, "a < b", false) classSymbols;

/**
 * Enum properties
 */
TTree!(DSymbol*, Mallocator, true, "a < b", false) enumSymbols;

static this()
{
	auto bool_ = make!DSymbol(Mallocator.instance, builtinTypeNames[13], CompletionKind.keyword);
	auto int_ = make!DSymbol(Mallocator.instance, builtinTypeNames[0], CompletionKind.keyword);
	auto long_ = make!DSymbol(Mallocator.instance, builtinTypeNames[8], CompletionKind.keyword);
	auto byte_ = make!DSymbol(Mallocator.instance, builtinTypeNames[19], CompletionKind.keyword);
	auto char_ = make!DSymbol(Mallocator.instance, builtinTypeNames[10], CompletionKind.keyword);
	auto dchar_ = make!DSymbol(Mallocator.instance, builtinTypeNames[12], CompletionKind.keyword);
	auto short_ = make!DSymbol(Mallocator.instance, builtinTypeNames[6], CompletionKind.keyword);
	auto ubyte_ = make!DSymbol(Mallocator.instance, builtinTypeNames[20], CompletionKind.keyword);
	auto uint_ = make!DSymbol(Mallocator.instance, builtinTypeNames[1], CompletionKind.keyword);
	auto ulong_ = make!DSymbol(Mallocator.instance, builtinTypeNames[9], CompletionKind.keyword);
	auto ushort_ = make!DSymbol(Mallocator.instance, builtinTypeNames[7], CompletionKind.keyword);
	auto wchar_ = make!DSymbol(Mallocator.instance, builtinTypeNames[11], CompletionKind.keyword);

	auto alignof_ = make!DSymbol(Mallocator.instance, internString("alignof"), CompletionKind.keyword);
	auto mangleof_ = make!DSymbol(Mallocator.instance, internString("mangleof"), CompletionKind.keyword);
	auto sizeof_ = make!DSymbol(Mallocator.instance, internString("sizeof"), CompletionKind.keyword);
	auto stringof_ = make!DSymbol(Mallocator.instance, internString("stringof"), CompletionKind.keyword);
	auto init = make!DSymbol(Mallocator.instance, internString("init"), CompletionKind.keyword);
	auto min = make!DSymbol(Mallocator.instance, internString("min"), CompletionKind.keyword);
	auto max = make!DSymbol(Mallocator.instance, internString("max"), CompletionKind.keyword);
	auto dup = make!DSymbol(Mallocator.instance, internString("dup"), CompletionKind.keyword);
	auto length = make!DSymbol(Mallocator.instance, internString("length"), CompletionKind.keyword, ulong_);
	auto tupleof = make!DSymbol(Mallocator.instance, internString("tupleof"), CompletionKind.variableName);

	arraySymbols.insert(alignof_);
	arraySymbols.insert(dup);
	arraySymbols.insert(make!DSymbol(Mallocator.instance, internString("idup"), CompletionKind.keyword));
	arraySymbols.insert(init);
	arraySymbols.insert(length);
	arraySymbols.insert(mangleof_);
	arraySymbols.insert(make!DSymbol(Mallocator.instance, internString("ptr"), CompletionKind.keyword));
	arraySymbols.insert(make!DSymbol(Mallocator.instance, internString("reverse"), CompletionKind.keyword));
	arraySymbols.insert(sizeof_);
	arraySymbols.insert(make!DSymbol(Mallocator.instance, internString("sort"), CompletionKind.keyword));
	arraySymbols.insert(stringof_);

	assocArraySymbols.insert(alignof_);
	assocArraySymbols.insert(make!DSymbol(Mallocator.instance, internString("byKey"), CompletionKind.keyword));
	assocArraySymbols.insert(make!DSymbol(Mallocator.instance, internString("byValue"), CompletionKind.keyword));
	assocArraySymbols.insert(dup);
	assocArraySymbols.insert(make!DSymbol(Mallocator.instance, internString("get"), CompletionKind.keyword));
	assocArraySymbols.insert(make!DSymbol(Mallocator.instance, internString("init"), CompletionKind.keyword));
	assocArraySymbols.insert(make!DSymbol(Mallocator.instance, internString("keys"), CompletionKind.keyword));
	assocArraySymbols.insert(length);
	assocArraySymbols.insert(mangleof_);
	assocArraySymbols.insert(make!DSymbol(Mallocator.instance, internString("rehash"), CompletionKind.keyword));
	assocArraySymbols.insert(sizeof_);
	assocArraySymbols.insert(stringof_);
	assocArraySymbols.insert(init);
	assocArraySymbols.insert(make!DSymbol(Mallocator.instance, internString("values"), CompletionKind.keyword));

	DSymbol*[11] integralTypeArray;
	integralTypeArray[0] = bool_;
	integralTypeArray[1] = int_;
	integralTypeArray[2] = long_;
	integralTypeArray[3] = byte_;
	integralTypeArray[4] = char_;
	integralTypeArray[4] = dchar_;
	integralTypeArray[5] = short_;
	integralTypeArray[6] = ubyte_;
	integralTypeArray[7] = uint_;
	integralTypeArray[8] = ulong_;
	integralTypeArray[9] = ushort_;
	integralTypeArray[10] = wchar_;

	foreach (s; integralTypeArray)
	{
		s.addChild(make!DSymbol(Mallocator.instance, internString("init"), CompletionKind.keyword, s), true);
		s.addChild(make!DSymbol(Mallocator.instance, internString("min"), CompletionKind.keyword, s), true);
		s.addChild(make!DSymbol(Mallocator.instance, internString("max"), CompletionKind.keyword, s), true);
		s.addChild(alignof_, false);
		s.addChild(sizeof_, false);
		s.addChild(stringof_, false);
		s.addChild(mangleof_, false);
	}

	auto cdouble_ = make!DSymbol(Mallocator.instance, builtinTypeNames[21], CompletionKind.keyword);
	auto cent_ = make!DSymbol(Mallocator.instance, builtinTypeNames[15], CompletionKind.keyword);
	auto cfloat_ = make!DSymbol(Mallocator.instance, builtinTypeNames[22], CompletionKind.keyword);
	auto creal_ = make!DSymbol(Mallocator.instance, builtinTypeNames[23], CompletionKind.keyword);
	auto double_ = make!DSymbol(Mallocator.instance, builtinTypeNames[2], CompletionKind.keyword);
	auto float_ = make!DSymbol(Mallocator.instance, builtinTypeNames[4], CompletionKind.keyword);
	auto idouble_ = make!DSymbol(Mallocator.instance, builtinTypeNames[3], CompletionKind.keyword);
	auto ifloat_ = make!DSymbol(Mallocator.instance, builtinTypeNames[5], CompletionKind.keyword);
	auto ireal_ = make!DSymbol(Mallocator.instance, builtinTypeNames[18], CompletionKind.keyword);
	auto real_ = make!DSymbol(Mallocator.instance, builtinTypeNames[17], CompletionKind.keyword);
	auto ucent_ = make!DSymbol(Mallocator.instance, builtinTypeNames[16], CompletionKind.keyword);

	DSymbol*[11] floatTypeArray;
	floatTypeArray[0] = cdouble_;
	floatTypeArray[1] = cent_;
	floatTypeArray[2] = cfloat_;
	floatTypeArray[3] = creal_;
	floatTypeArray[4] = double_;
	floatTypeArray[5] = float_;
	floatTypeArray[6] = idouble_;
	floatTypeArray[7] = ifloat_;
	floatTypeArray[8] = ireal_;
	floatTypeArray[9] = real_;
	floatTypeArray[10] = ucent_;

	foreach (s; floatTypeArray)
	{
		s.addChild(alignof_, false);
		s.addChild(make!DSymbol(Mallocator.instance, internString("dig"), CompletionKind.keyword, s), true);
		s.addChild(make!DSymbol(Mallocator.instance, internString("epsilon"), CompletionKind.keyword, s), true);
		s.addChild(make!DSymbol(Mallocator.instance, internString("infinity"), CompletionKind.keyword, s), true);
		s.addChild(make!DSymbol(Mallocator.instance, internString("init"), CompletionKind.keyword, s), true);
		s.addChild(mangleof_, false);
		s.addChild(make!DSymbol(Mallocator.instance, internString("mant_dig"), CompletionKind.keyword, int_), true);
		s.addChild(make!DSymbol(Mallocator.instance, internString("max"), CompletionKind.keyword, s), true);
		s.addChild(make!DSymbol(Mallocator.instance, internString("max_10_exp"), CompletionKind.keyword, int_), true);
		s.addChild(make!DSymbol(Mallocator.instance, internString("max_exp"), CompletionKind.keyword, int_), true);
		s.addChild(make!DSymbol(Mallocator.instance, internString("min"), CompletionKind.keyword, s), true);
		s.addChild(make!DSymbol(Mallocator.instance, internString("min_exp"), CompletionKind.keyword, int_), true);
		s.addChild(make!DSymbol(Mallocator.instance, internString("min_10_exp"), CompletionKind.keyword, int_), true);
		s.addChild(make!DSymbol(Mallocator.instance, internString("min_normal"), CompletionKind.keyword, s), true);
		s.addChild(make!DSymbol(Mallocator.instance, internString("nan"), CompletionKind.keyword, s), true);
		s.addChild(sizeof_, false);
		s.addChild(stringof_, false);
	}

	aggregateSymbols.insert(tupleof);
	aggregateSymbols.insert(mangleof_);
	aggregateSymbols.insert(alignof_);
	aggregateSymbols.insert(sizeof_);
	aggregateSymbols.insert(stringof_);
	aggregateSymbols.insert(init);

	classSymbols.insert(make!DSymbol(Mallocator.instance, internString("classinfo"), CompletionKind.variableName));
	classSymbols.insert(tupleof);
	classSymbols.insert(make!DSymbol(Mallocator.instance, internString("__vptr"), CompletionKind.variableName));
	classSymbols.insert(make!DSymbol(Mallocator.instance, internString("__monitor"), CompletionKind.variableName));
	classSymbols.insert(mangleof_);
	classSymbols.insert(alignof_);
	classSymbols.insert(sizeof_);
	classSymbols.insert(stringof_);
	classSymbols.insert(init);

	enumSymbols.insert(init);
	enumSymbols.insert(sizeof_);
	enumSymbols.insert(alignof_);
	enumSymbols.insert(mangleof_);
	enumSymbols.insert(stringof_);
	enumSymbols.insert(min);
	enumSymbols.insert(max);


	ireal_.addChild(make!DSymbol(Mallocator.instance, internString("im"), CompletionKind.keyword, real_), true);
	ifloat_.addChild(make!DSymbol(Mallocator.instance, internString("im"), CompletionKind.keyword, float_), true);
	idouble_.addChild(make!DSymbol(Mallocator.instance, internString("im"), CompletionKind.keyword, double_), true);
	ireal_.addChild(make!DSymbol(Mallocator.instance, internString("re"), CompletionKind.keyword, real_), true);
	ifloat_.addChild(make!DSymbol(Mallocator.instance, internString("re"), CompletionKind.keyword, float_), true);
	idouble_.addChild(make!DSymbol(Mallocator.instance, internString("re"), CompletionKind.keyword, double_), true);

	auto void_ = make!DSymbol(Mallocator.instance, builtinTypeNames[14], CompletionKind.keyword);

	builtinSymbols.insert(bool_);
	bool_.type = bool_;
	builtinSymbols.insert(int_);
	int_.type = int_;
	builtinSymbols.insert(long_);
	long_.type = long_;
	builtinSymbols.insert(byte_);
	byte_.type = byte_;
	builtinSymbols.insert(char_);
	char_.type = char_;
	builtinSymbols.insert(dchar_);
	dchar_.type = dchar_;
	builtinSymbols.insert(short_);
	short_.type = short_;
	builtinSymbols.insert(ubyte_);
	ubyte_.type = ubyte_;
	builtinSymbols.insert(uint_);
	uint_.type = uint_;
	builtinSymbols.insert(ulong_);
	ulong_.type = ulong_;
	builtinSymbols.insert(ushort_);
	ushort_.type = ushort_;
	builtinSymbols.insert(wchar_);
	wchar_.type = wchar_;
	builtinSymbols.insert(cdouble_);
	cdouble_.type = cdouble_;
	builtinSymbols.insert(cent_);
	cent_.type = cent_;
	builtinSymbols.insert(cfloat_);
	cfloat_.type = cfloat_;
	builtinSymbols.insert(creal_);
	creal_.type = creal_;
	builtinSymbols.insert(double_);
	double_.type = double_;
	builtinSymbols.insert(float_);
	float_.type = float_;
	builtinSymbols.insert(idouble_);
	idouble_.type = idouble_;
	builtinSymbols.insert(ifloat_);
	ifloat_.type = ifloat_;
	builtinSymbols.insert(ireal_);
	ireal_.type = ireal_;
	builtinSymbols.insert(real_);
	real_.type = real_;
	builtinSymbols.insert(ucent_);
	ucent_.type = ucent_;
	builtinSymbols.insert(void_);
	void_.type = void_;


	foreach (s; ["__DATE__", "__EOF__", "__TIME__", "__TIMESTAMP__", "__VENDOR__",
			"__VERSION__", "__FUNCTION__", "__PRETTY_FUNCTION__", "__MODULE__",
			"__FILE__", "__LINE__"])
		builtinSymbols.insert(make!DSymbol(Mallocator.instance, internString(s), CompletionKind.keyword));
}
