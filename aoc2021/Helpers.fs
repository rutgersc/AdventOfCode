﻿module aoc2021.Helpers

// #r "nuget: dein.Colorify";;
open System

let readLines f =
    System.IO.File.ReadAllLines f

let readCsvLine f =
    readLines f
    |> fun l -> l.[0].Split ","

let readBitArrays f: int[][] =
    let charToBit c = if c = '1' then 1 else 0
    let charsToBits (line: string) = line.ToCharArray() |> Array.map charToBit

    readLines f
    |> Array.map charsToBits

let readNumberLines f =
    System.IO.File.ReadAllLines f
    |> Array.map int

let uncurry f (a,b) = f a b
let curry f a b = f(a,b)

let notWhitespace s = not (System.String.IsNullOrWhiteSpace (s))

let chars (s: string) = s.ToCharArray () |> Array.toList

let joinChars (s: char seq) = String.Join("", s)

module Option =
    let inspect f o =
        o
        |> Option.map (fun v -> f v; v)

module Seq =
    let inspect f o =
        o
        |> Seq.map (fun v -> f v; v)
