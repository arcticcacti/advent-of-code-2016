#load "AdventUtils.fsx"

let getInput() = AdventUtils.inputFromFile "day_3.txt"


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


/// pull 3-element columns from a sequence of rows
let transposeToColumns rows =
    rows
    |> Seq.chunkBySize 3
    |> Seq.collect (fun xs -> Seq.zip3 xs.[0] xs.[1] xs.[2])
    |> Seq.map (fun (x,y,z) -> [|x;y;z|])


/// Count the possible triangles in a sequence of side groups
let countPossibleTris sideGroups =
    sideGroups
    |> Seq.filter possibleTriangle
    |> Seq.length
    |> printfn "%d possible triangles"

(*
    Runnables
*)

let part1() =
    getInput()
    |> Array.map parseInts
    |> countPossibleTris


let part2() =
    getInput()
    |> Array.map parseInts
    |> transposeToColumns
    |> countPossibleTris


let test1() =
    let shouldBe = AdventUtils.printTestResult "1" (=)
    let input = "5 10 25"
    let result = parseInts input |> possibleTriangle
    result |> shouldBe false


let testTranspose() =
    let shouldMatch = AdventUtils.printTestResult "transposing" (=)
    let input =
        [| [|101; 201; 301|];
           [|102; 202; 302|];
           [|103; 203; 303|] |]
    let expected =
        [| [|101; 102; 103|];
           [|201; 202; 203|];
           [|301; 302; 303|] |]
    let result = input |> Seq.ofArray |> transposeToColumns |> Array.ofSeq
    result |> shouldMatch expected


part1()
part2()
test1()
testTranspose()