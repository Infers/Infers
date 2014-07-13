// Copyright (C) by Vesa Karvonen

using System.Reflection;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

[assembly: AssemblyTitle("Infers.Core")]
[assembly: AssemblyDescription(Infers.Core.CommonAssemblyInfo.Description)]
[assembly: AssemblyConfiguration(Infers.Core.CommonAssemblyInfo.Configuration)]
[assembly: AssemblyCompany(Infers.Core.CommonAssemblyInfo.Company)]
[assembly: AssemblyProduct(Infers.Core.CommonAssemblyInfo.Product)]
[assembly: AssemblyCopyright(Infers.Core.CommonAssemblyInfo.Copyright)]
[assembly: AssemblyTrademark(Infers.Core.CommonAssemblyInfo.Trademark)]
[assembly: AssemblyCulture(Infers.Core.CommonAssemblyInfo.Culture)]

[assembly: ComVisible(false)]

[assembly: Guid("03bcf3d5-874b-4869-a853-190bf040a0c4")]

[assembly: AssemblyVersion(Infers.Core.CommonAssemblyInfo.Version)]
[assembly: AssemblyFileVersion(Infers.Core.CommonAssemblyInfo.FileVersion)]

namespace Infers.Core {
#pragma warning disable 1591 // Missing XML comment

  public static class CommonAssemblyInfo {
    public const string Description =
      "Infers is a library for deriving F# values from their types.";
    public const string Configuration = "";
    public const string Product = "Infers";
    public const string Company = "";
    public const string Copyright = "© Vesa Karvonen and Anton Tayanovskyy";
    public const string Version = "0.0.0.5";
    public const string FileVersion = Version;
    public const string Trademark = "";
    public const string Culture = "";
  }
}
