#if FSHARP_NON_INTERACTIVE
namespace Infers
#endif

open System

/// A type that has the `InferenceRules` attribute is assumed to contain
/// inference rule methods that are used by the inference engine.
type InferenceRules () = inherit Attribute ()

/// Untyped interface for typed proxies.  Not to be used by user code.
type [<AbstractClass>] RecObj () =
  abstract GetObj: unit -> obj
  abstract SetObj: obj -> unit

/// Proxy for a potentially recursive value.
type [<AbstractClass>] Rec<'x> () =
  inherit RecObj ()
  abstract Get: unit -> 'x
  abstract Set: 'x -> unit
  override this.GetObj () = box (this.Get ())
  override this.SetObj x = this.Set (unbox<'x> x)

/// Exception that can be raised by an otherwise matched rule to make the
/// inference engine to treat the rule as a non-match and backtrack.
exception Backtrack
