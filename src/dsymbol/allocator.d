module dsymbol.allocator;

version = useSafeAllocator;

version(useSafeAllocator)
{
    import stdx.allocator.gc_allocator : GCAllocator;
    alias Mallocator = GCAllocator;
}
else
{
    import stdx.allocator.mallocator : Mallocator;
    alias Mallocator = stdx.allocator.mallocator.Mallocator;
}

void disposeX(A, T)(auto ref A alloc, auto ref T* p)
{
    version(useSafeAllocator) {}
    else
    {
        import stdx.allocator : dispose;
        dispose!(A, T)(alloc, p);
    }
}

void disposeX(A, T)(auto ref A alloc, auto ref T p)
if (is(T == class) || is(T == interface))
{
    version(useSafeAllocator) {}
    else
    {
        import stdx.allocator : dispose;
        dispose!(A, T)(alloc, p);
    }
}

void disposeX(A, T)(auto ref A alloc, auto ref T[] array)
{
    version(useSafeAllocator) {}
    else
    {
        import stdx.allocator : dispose;
        dispose!(A, T)(alloc, p);
    }
}
