#I "./bin/Release"
#r "FCure.TypeProviders"

open Precure.TypeProviders

[<Literal>]
let Path = __SOURCE_DIRECTORY__

type プリキュア = Precure<Path>

let smile = プリキュア.``スマイルプリキュア！``

printfn "%s" smile.タイトル

let yayoi = smile.黄瀬やよい

let peace = yayoi.``プリキュア・スマイルチャージ！``

peace.攻撃()
