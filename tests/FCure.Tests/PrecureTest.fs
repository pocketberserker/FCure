namespace Precure.Tests

open Persimmon
open UseTestNameByReflection
open Precure.TypeProviders

module PrecureTest =

  [<Literal>]
  let Path = __SOURCE_DIRECTORY__

  type プリキュア = Precure<Path>

  let ``タイトルを取得できる`` = test {
    let p = プリキュア.``Go!プリンセスプリキュア``
    do! assertEquals "Go!プリンセスプリキュア"  p.タイトル
    return p
  }

  let ``キャラクターを選べる`` = test {
    let p = プリキュア.``Go!プリンセスプリキュア``.紅城トワ
    do! assertEquals "紅城トワ"  p.名前
    return p
  }

  let ``プリキュア！`` = test {
    let! p = ``キャラクターを選べる``
    let p = p.``プリキュア！プリンセスエンゲージ！``
    do! assertEquals "キュアスカーレット"  p.名前
    return p
  }
