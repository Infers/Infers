// Copyright (C) by Vesa Karvonen

namespace Infers.Toys

open System.Reflection
open System.Runtime.InteropServices

[<AutoOpen>]
module AssemblyInfo =
  [<Literal>]
  let Version = "0.2.6"

[<assembly: AssemblyTitle("Infers.Toys")>]
[<assembly: AssemblyDescription("Infers.Toys implements a number of datatype generic functions.")>]
[<assembly: AssemblyConfiguration(Infers.Core.CommonAssemblyInfo.Configuration)>]
[<assembly: AssemblyCompany(Infers.Core.CommonAssemblyInfo.Company)>]
[<assembly: AssemblyProduct(Infers.Core.CommonAssemblyInfo.Product)>]
[<assembly: AssemblyCopyright(Infers.Core.CommonAssemblyInfo.Copyright)>]
[<assembly: AssemblyTrademark(Infers.Core.CommonAssemblyInfo.Trademark)>]
[<assembly: AssemblyCulture(Infers.Core.CommonAssemblyInfo.Culture)>]

[<assembly: ComVisible(false)>]

[<assembly: Guid("24e2775d-dc15-46fd-86a9-18bbc0e38c79")>]

[<assembly: AssemblyVersion(Version)>]
[<assembly: AssemblyFileVersion(Version)>]

()
