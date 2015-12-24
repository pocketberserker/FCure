namespace Precure.TypeProviders

open System
open System.IO
open System.Linq
open System.Reflection
open Microsoft.FSharp.Core.CompilerServices
open ProviderImplementation.ProvidedTypes
 
#nowarn "13730"
[<TypeProvider>]
type public PrecureTypeProvider(cfg:TypeProviderConfig) as this = 
  inherit TypeProviderForNamespaces()
  let asm = Assembly.GetExecutingAssembly()
  let ns = "Precure.TypeProviders"
  let parameters = [
    ProvidedStaticParameter("source", typeof<string>)
  ]
 
  do
    let typ = ProvidedTypeDefinition(asm, ns, "Precure", Some (typeof<obj>), HideObjectMethods = true)
    typ.DefineStaticParameters(
      parameters,
      fun typeName parameters ->
        let source = string parameters.[0]
        let typ = ProvidedTypeDefinition(asm, ns, typeName, Some typeof<obj>, HideObjectMethods = true)
        let source =
          if String.IsNullOrWhiteSpace(source) then "https://rubicure.herokuapp.com"
          else source.TrimEnd([|'/'|])
        let ctor = ProvidedConstructor(parameters = [], InvokeCode= (fun _ -> <@@ source @@>))
        typ.AddMember ctor
        let series =
          Series.asyncLoad source
          |> Async.Catch
          |> Async.RunSynchronously
        match series with
        | Choice1Of2 series ->
          typ.AddMembersDelayed(fun () -> Series.generate series)
        | Choice2Of2 e -> raise e
        typ)
    this.AddNamespace(ns, [typ])
 
    let thisAssembly = Assembly.GetAssembly(typeof<Series>)
    let path = Path.GetDirectoryName(thisAssembly.Location)
    this.RegisterProbingFolder(path)

[<assembly:TypeProviderAssembly>]
do()
