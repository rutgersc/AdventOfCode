module aoc2021.Helpers

let readNumberLines f =
    System.IO.File.ReadAllLines f
    |> Array.map int
