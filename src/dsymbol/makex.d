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

void disposeX(A, T)(auto ref A alloc, auto ref T* p)
{
    version(safeAlloc)
    {
    }
    else
    {
        import stdx.allocator : dispose;
        dispose!(A, T)(alloc, p);
    }
}

void disposeX(A, T)(auto ref A alloc, auto ref T p)
if (is(T == class) || is(T == interface))
{
    version(safeAlloc)
    {
    }
    else
    {
        import stdx.allocator : dispose;
        dispose!(A, T)(alloc, p);
    }
}

void disposeX(A, T)(auto ref A alloc, auto ref T[] array)
{
    version(safeAlloc)
    {
    }
    else
    {
        import stdx.allocator : dispose;
        dispose!(A, T)(alloc, p);
    }
}
