// Copyright (C) by Vesa Karvonen

using System.Diagnostics;
using System.Runtime.CompilerServices;
using System.Threading;

namespace Infers.Core {
  /// <summary>Represents a kind of static, or code generation time, mapping of
  /// types to objects.</summary>
  public static class StaticMap<Key, Value> where Value : class {
    private static Value value;

    /// <summary>Returns the stored object or `null` in case the object has not
    /// been set.</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Value Get() {
      return Volatile.Read(ref StaticMap<Key, Value>.value);
    }

    /// <summary>Atomically, if an object has already been stored, returns it,
    /// otherwise sets the stored object and returns it.</summary>
    public static Value TrySetAndGet(Value value) {
      Debug.Assert(null != value);
      var was = Interlocked.CompareExchange(ref StaticMap<Key, Value>.value, value, null);
      if (null != was)
        value = was;
      return value;
    }
  }
}
