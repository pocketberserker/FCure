namespace Precure.TypeProviders

open System
open System.Globalization
open System.Reflection
open FSharp.Data
open FSharp.Data.JsonExtensions
open FSharp.Quotations
open ProviderImplementation.ProvidedTypes

type Girl = {
  Tag: string
  Name: string
  PrecureName: string
  Cast: string
  Color: string
  TransformMessage: string
  ExtraNames: string []
  AttackMessages: string []
}

module Girls =

  let cultureInfo = CultureInfo.GetCultureInfo("ja-JP")

  let (|Girls|) girls =
    match girls with
    | JsonValue.Array xs ->
      xs
      |> Array.map (fun x ->
        let tag, name, precureName, cast, color, transformMessage, extraNames, attackMessages =
          x?girl_name, x?human_name, x?precure_name, x?cast_name, x?color, x?transform_message, x.TryGetProperty("extra_names"), x?attack_messages
        {
          Tag = tag.AsString(cultureInfo)
          Name = name.AsString(cultureInfo)
          PrecureName = precureName.AsString(cultureInfo)
          Cast = cast.AsString(cultureInfo)
          Color = color.AsString(cultureInfo)
          TransformMessage = transformMessage.AsString(cultureInfo)
          ExtraNames =
            extraNames
            |> Option.toArray
            |> Array.collect (fun extraNames ->
              extraNames.AsArray()
              |> Array.map (fun x -> x.AsString(cultureInfo)))
          AttackMessages = attackMessages.AsArray() |> Array.map (fun x -> x.AsString(cultureInfo))
        }
      )
    | _ -> failwith "プリキュア一覧のロードに失敗しました"

  let asyncLoad baseUrl = async {
    let! Girls s = JsonValue.AsyncLoad(sprintf "%s/girls.json" baseUrl, cultureInfo)
    return s
  }

  let load baseUrl = asyncLoad baseUrl |> Async.RunSynchronously

  let generateCure isStatic
    { Tag = _; Name = _; PrecureName = name; Cast = cast; Color = color; TransformMessage = transformMessage; ExtraNames = extraNames; AttackMessages = attackMessages } =
    let typ = ProvidedTypeDefinition(name, Some typeof<obj>, HideObjectMethods = true)
    let ctor = ProvidedConstructor(parameters = [], InvokeCode = (fun _ -> <@@ () @@>))
    typ.AddMember(ctor)
    typ.AddXmlDocDelayed(fun () -> sprintf "%s\nキャスト: %s\nカラー: %s" name cast color)
    typ.AddMemberDelayed(fun () ->
      ProvidedProperty(
        propertyName = "名前",
        propertyType = typeof<string>,
        IsStatic = isStatic,
        GetterCode = (fun _ -> <@@ name @@>)
      )
    )
    typ.AddMemberDelayed(fun () ->
      ProvidedProperty(
        propertyName = "キャスト",
        propertyType = typeof<string>,
        IsStatic = isStatic,
        GetterCode = (fun _ -> <@@ cast @@>)
      )
    )
    typ.AddMemberDelayed(fun () ->
      ProvidedProperty(
        propertyName = "カラー",
        propertyType = typeof<string>,
        IsStatic = isStatic,
        GetterCode = (fun _ -> <@@ color @@>)
      )
    )
    typ.AddMemberDelayed(fun () ->
      let a = attackMessages.[0]
      ProvidedMethod("攻撃", [], typeof<unit>, InvokeCode = fun _ ->
        Expr.Application(<@@ printfn "%s" @@>, <@@ a @@>)
      )
    )
    (ctor, typ)

  let generateProperties isStatic (cure, cureCtor)
    { Tag = _; Name = name; PrecureName = _; Cast = cast; Color = color; TransformMessage = transformMessage; ExtraNames = _; AttackMessages = attackMessages } =
    let name =
      ProvidedProperty(
        propertyName = "名前",
        propertyType = typeof<string>,
        IsStatic = isStatic,
        GetterCode = (fun _ -> <@@ name @@>)
      )
    let cast =
      ProvidedProperty(
        propertyName = "キャスト",
        propertyType = typeof<string>,
        IsStatic = isStatic,
        GetterCode = (fun _ -> <@@ cast @@>)
      )
    let color =
      ProvidedProperty(
        propertyName = "カラー",
        propertyType = typeof<string>,
        IsStatic = isStatic,
        GetterCode = (fun _ -> <@@ color @@>)
      )
    let transform =
      let name =
        transformMessage.Split([|'\n'|])
        |> Array.find (fun x -> not <| x.StartsWith("("))
      ProvidedProperty(
        propertyName = name,
        propertyType = cure,
        IsStatic = isStatic,
        GetterCode = (fun _ ->
          Expr.Sequential(
            Expr.Application(<@@ printfn "%s" @@>, <@@ transformMessage @@>),
            cureCtor
          )
        )
      )
    [ name; cast; color; transform ]

  let generate isStatic girls =
    girls
    |> Array.toList
    |> List.collect (fun girl ->
      let typ = ProvidedTypeDefinition(girl.Name, Some typeof<obj>, HideObjectMethods = true)
      let ctor = ProvidedConstructor(parameters = [], InvokeCode = (fun _ -> <@@ () @@>))
      typ.AddMember(ctor)
      let (cureCtor, cure) = generateCure false girl
      typ.AddMembersDelayed(fun () -> generateProperties false (cure, Expr.NewObject(cureCtor, [])) girl)
      typ.AddXmlDocDelayed(fun () -> sprintf "%s\nプリキュア名: %s\nキャスト: %s" girl.Name girl.PrecureName girl.Cast)
      let prop =
        ProvidedProperty(
          propertyName = girl.Name,
          IsStatic = isStatic,
          propertyType = typ,
          GetterCode = (fun _ -> Expr.NewObject(ctor, []))
        )
      [ typ :> MemberInfo; cure :> MemberInfo; prop :> MemberInfo ]
    )
