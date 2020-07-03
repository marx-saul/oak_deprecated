module ast.attribute;

import std.typecons;
import aatree, parser.lexer;

enum Attribute : ulong {
    none       = 0UL,

    shadow     = 1UL << 0,

    immut      = 1UL << 1,
    const_     = 1UL << 2,
    inout_     = 1UL << 3,

    private_   = 1UL << 4,
    protected_ = 1UL << 5,
    package_   = 1UL << 6,
    public_    = 1UL << 7,
    export_    = 1UL << 8,
    abstract_  = 1UL << 9,
    override_  = 1UL << 10,

    final_     = 1UL << 11,
    static_    = 1UL << 12,
    ref_       = 1UL << 13,
    lazy_      = 1UL << 14,

    pure_      = 1UL << 15, // @pure
    safe       = 1UL << 16, // @safe
    trusted    = 1UL << 17, // @trusted
    system     = 1UL << 18, // @system
    throwable  = 1UL << 19, // @throwable

    type_qualifier     = immut | const_ | inout_,
    access_level       = private_ | protected_ | package_ | public_ | export_,
    function_safety    = safe | trusted | system,
    all                = ulong.max,
}

static const tt_to_att = new AATree!(TokenType, (a,b)=>a<b, Attribute)([
    tuple(TokenType.shadow,     Attribute.shadow),
    tuple(TokenType.immut,      Attribute.immut),
    tuple(TokenType.const_,     Attribute.const_),
    tuple(TokenType.inout_,     Attribute.inout_),

    tuple(TokenType.private_,   Attribute.private_),
    tuple(TokenType.protected_, Attribute.protected_),
    tuple(TokenType.package_,   Attribute.package_),
    tuple(TokenType.public_,    Attribute.public_),
    tuple(TokenType.export_,    Attribute.export_),
    tuple(TokenType.abstract_,  Attribute.abstract_),
    tuple(TokenType.override_,  Attribute.override_),

    tuple(TokenType.final_,     Attribute.final_),
    tuple(TokenType.static_,    Attribute.static_),
    tuple(TokenType.ref_,       Attribute.ref_),
    tuple(TokenType.lazy_,      Attribute.lazy_),
]);

static const str_to_att = new StringDict!Attribute ([
    tuple("pure",      Attribute.pure_),
    tuple("safe",      Attribute.safe),
    tuple("trusted",   Attribute.trusted),
    tuple("system",    Attribute.system),
    tuple("throwable", Attribute.throwable),
]);

Attribute getAttribute(Token token) {
    if (tt_to_att.hasKey(token.type)) { return tt_to_att[token.type]; }

    if (str_to_att.hasKey(token.str)) { return str_to_att[token.str]; }

    return Attribute.none;
}
