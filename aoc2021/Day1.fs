module aoc2021.Day1

open Types
open Helpers

let solve1 (depths: seq<Depth>) =
    depths
    |> Seq.pairwise
    |> Seq.filter (fun (a,b)-> b > a)
    |> Seq.length

let solve2 (depths: seq<Depth>) =
    let windowed size (source: int seq) =
        Seq.windowed size source |> Seq.map Seq.sum |> Seq.toList

    windowed 3 depths |> solve1

let inputExample =
    [ 199
      200
      208
      210
      200
      207
      240
      269
      260
      263 ]

let answerExample1 = solve1 inputExample
let answerExample2 = solve2 inputExample

let answer1 = readNumberLines "Day1.txt" |> solve1
let answer2 = readNumberLines "Day1-2.txt" |> solve2
