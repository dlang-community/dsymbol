module dsymbol.makex;

version = safeAlloc;

auto makeX(T, Allocator, A...)(auto ref Allocator alloc, auto ref A args)
{
    import stdx.allocator : make;
    version(safeAlloc)
    {
        import stdx.allocator.gc_allocator : GCAllocator;
        return make!(T)(GCAllocator.instance, args);
    }
    else
        return make!(T)(alloc, args);
}
