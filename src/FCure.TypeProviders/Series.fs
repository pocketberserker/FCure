namespace Precure.TypeProviders

open System
open System.Globalization
open System.Reflection
open FSharp.Data
open FSharp.Data.JsonExtensions
open FSharp.Quotations
open ProviderImplementation.ProvidedTypes

type Series = {
  Title: string
  StartedDate: DateTime
  Girls: Girl []
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Series =

  let cultureInfo = CultureInfo.GetCultureInfo("ja-JP")

  let (|Series|) girls series =
    match series with
    | JsonValue.Array xs ->
      xs
      |> Array.map (fun x ->
        let title, startedDate, gs = x?title, x?started_date, x?girls
        {
          Title = title.AsString(cultureInfo)
          StartedDate = startedDate.AsDateTime()
          Girls =
            gs.AsArray()
            |> Array.map (fun x -> Array.find (fun y -> x.AsString(cultureInfo) = y.Tag) girls)
        }
      )
    | _ -> failwith "シリーズ一覧のロードに失敗しました"

  let asyncLoad baseUrl =
    async {
      let! girls = Girls.asyncLoad baseUrl
      let! s = JsonValue.AsyncLoad(sprintf "%s/series.json" baseUrl, cultureInfo)
      return
        match s with
        | Series girls series -> series
    }

  let load baseUrl = asyncLoad baseUrl |> Async.RunSynchronously

  let generateProperties isStatic { Title = title; StartedDate = started } =
    let title =
      ProvidedProperty(
        propertyName = "タイトル",
        propertyType = typeof<string>,
        IsStatic = isStatic,
        GetterCode = (fun _ -> <@@ title @@>)
      )
    let started =
      ProvidedProperty(
        propertyName = "開始日",
        propertyType = typeof<string>,
        IsStatic = isStatic,
        GetterCode = (fun _ -> <@@ started @@>)
      )
    [ title; started ]

  let generate series =
    series
    |> Array.toList
    |> List.collect (fun series ->
      let typ = ProvidedTypeDefinition(series.Title, Some typeof<obj>, HideObjectMethods = true)
      let ctor = ProvidedConstructor(parameters = [], InvokeCode = (fun _ -> <@@ () @@>))
      typ.AddMember(ctor)
      typ.AddMembersDelayed(fun () -> generateProperties false series)
      typ.AddMembersDelayed(fun () -> Girls.generate false series.Girls)
      let prop =
        ProvidedProperty(
          propertyName = series.Title,
          IsStatic = true,
          propertyType = typ,
          GetterCode = (fun _ -> Expr.NewObject(ctor, []))
        )
      [ typ :> MemberInfo; prop :> MemberInfo ]
    )
