#if FSHARP_NON_INTERACTIVE
namespace Infers
#endif

open System

type RecRulesInvoker (rr: obj) =
  let rrt = rr.GetType ()

  let getMethod name =
    match rrt.GetMethod (name, BindingFlags.AnyDeclaredInstance) with
     | null -> failwithf "%A must have method named %s" rrt name
     | meth -> meth

  let tierMeth = getMethod "Tier"
  let tierParam =
    match tierMeth.GetParameters () with
     | [|p|] -> p.ParameterType
     | _ -> failwith "Tier method must have one dummy parameter"

  let untieMeth = getMethod "Untie"
  let untieResult = untieMeth.ReturnType

  let tieMeth = getMethod "Tie"
  let tieParam =
    match tieMeth.GetParameters () with
     | [|_; p|] -> p.ParameterType
     | _ -> failwithf "Tie method must have two parameters"

  member x.Tier (t: Type) : option<obj> =
    tryInvoke tierMeth tierParam t rr [|null|]
  member x.Untie (t: Type, tier: obj) : obj =
    match tryInvoke untieMeth untieResult t rr [|tier|] with
     | None -> failwith "Failed to call Untie"
     | Some x -> x
  member x.Tie (t: Type, tier: obj, result: obj) : unit  =
    match tryInvoke tieMeth tieParam t rr [|tier; result|] with
     | None -> failwith "Failed to call Tie"
     | Some _ -> ()
