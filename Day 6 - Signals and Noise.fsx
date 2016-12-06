#load "AdventUtils.fsx"

let getInput() = AdventUtils.inputFromFile "day_6.txt"

type CharSelector = MostFrequent | LeastFrequent

/// break a repetition code by selecting characters by frequency
let unjam selectChar message =
    let sortByFrequency =
        match selectChar with
        | MostFrequent -> Seq.sortByDescending
        | LeastFrequent -> Seq.sortBy
    let enumerate = Seq.mapi (fun i x -> (i, x))
    let elementsInGroup = snd >> Seq.map snd
    let mostRelevant =
        Seq.countBy id >> sortByFrequency snd >> Seq.map fst >> Seq.head

    message
    |> Seq.collect enumerate
    |> Seq.groupBy fst
    |> Seq.map (elementsInGroup >> mostRelevant)
    |> Array.ofSeq
    |> fun arr -> new System.String(arr)


let part1() =
    getInput() |> unjam MostFrequent |> printfn "Part 1 - unjammed code: %s"

let part2() =
    getInput() |> unjam LeastFrequent |> printfn "Part 2 - unjammed code: %s"


//
// Tests
//

let testInput = [|
    "eedadn";
    "drvtee";
    "eandsr";
    "raavrd";
    "atevrs";
    "tsrnev";
    "sdttsa";
    "rasrtv";
    "nssdts";
    "ntnada";
    "svetve";
    "tesnvt";
    "vntsnd";
    "vrdear";
    "dvrsen";
    "enarar"
|]

let test1() =
    let shouldBe = AdventUtils.printTestResult "1 - most frequent" (=)
    let result = unjam MostFrequent testInput
    result |> shouldBe "easter"

let test2() =
    let shouldBe = AdventUtils.printTestResult "2 - least frequent" (=)
    let result = unjam LeastFrequent testInput
    result |> shouldBe "advent"


part1()
part2()
test1()
test2()