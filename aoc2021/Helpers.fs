module aoc2021.Helpers

let readLines f =
    System.IO.File.ReadAllLines f

let readNumberLines f =
    System.IO.File.ReadAllLines f
    |> Array.map int
