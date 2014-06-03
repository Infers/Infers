namespace Infers

open System.Reflection
open System.Runtime.InteropServices

[<AutoOpen>]
module AssemblyInfo =
  [<Literal>]
  let Version = "0.0.0.1"

[<assembly: AssemblyTitle("Infers")>]
[<assembly: AssemblyDescription("Infers is a library for deriving F# values from their types.")>]
[<assembly: AssemblyConfiguration("")>]
[<assembly: AssemblyCompany("")>]
[<assembly: AssemblyProduct("Infers")>]
[<assembly: AssemblyCopyright("© Vesa Karvonen and Anton Tayanovskyy")>]
[<assembly: AssemblyTrademark("")>]
[<assembly: AssemblyCulture("")>]

[<assembly: ComVisible(false)>]

[<assembly: Guid("2c3b33a5-81d6-4e06-8f46-7b4105b39c7e")>]

[<assembly: AssemblyVersion(Version)>]
[<assembly: AssemblyFileVersion(Version)>]

()
