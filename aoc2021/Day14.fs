module aoc2021.Day14

open Helpers
open System

let parse lines =
    let template, rules = lines |> Array.filter (fun (s: string) -> s <> "") |> Array.splitAt 1
    let parseRule (line:string) =
        let [| f; t |] = line.Split "->" |> Array.map (fun l -> l.Trim())
        f, t

    template.[0], rules |> Array.map parseRule

let ruleMapping (template, rules) =
    template, rules |> Seq.map (fun ((f:string), t) -> f, $"{f.[0]}{t}{f.[1]}") |> Map.ofSeq


let solve1 (template, ruleMapping: Map<_,_>) =
    template
    |> Seq.pairwise
    |> Seq.map (fun (a,b) -> $"{a}{b}")
    |> Seq.map (fun b -> ruleMapping.[b])
    |> Seq.pairwise
    |> Seq.map (fun (a:string,b:string) -> a.[0.. (a.Length - 2)] )
    |> Seq.toList

let answerExample1 = readLines "Day14-example.txt" |> parse |> ruleMapping |> solve1 // 1656
