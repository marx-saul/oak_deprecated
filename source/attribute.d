module attribute;

enum Attribute : ulong {
    none       = 0UL,

    //shadow     = 1UL << 0,
    ref_       = 1UL << 0,

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

    pure_      = 1UL << 11,
    lazy_      = 1UL << 12,
}
