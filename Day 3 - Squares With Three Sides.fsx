let getInput() =
    let path = "input\day_3.txt"
    System.IO.File.ReadAllLines path


/// parse a line into an int array
let parseInts (line:string) =
    line.Split([|" "|], System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.map int


/// whether an int array represents the three sides of a possible triangle
let possibleTriangle sides =
    if Array.length sides <> 3 then
        false
    else
        sides |> Array.sortDescending |> fun xs -> xs.[0] < xs.[1] + xs.[2]

(*
    Runnables
*)

let part1() =
    getInput()
    |> Array.map parseInts
    |> Seq.filter possibleTriangle
    |> Seq.length
    |> printfn "%d possible triangles"


let test1() =
    let input = "5 10 25"
    let expected = false
    let result = parseInts input |> possibleTriangle
    printfn "Test %s" <| if result = expected then "passed" else "failed"
    

part1()
test1()