#load "AdventUtils.fsx"

let getInput() = AdventUtils.inputFromFile "day_6.txt"


/// break a repetition code by collecting the most frequent characters
let unjam message =
    let enumerate = Seq.mapi (fun i x -> (i, x))
    let elementsInGroup = snd >> Seq.map snd
    let mostCommon =
        Seq.countBy id >> Seq.sortByDescending snd >> Seq.map fst >> Seq.head

    message
    |> Seq.collect enumerate
    |> Seq.groupBy fst
    |> Seq.map (elementsInGroup >> mostCommon)
    |> Array.ofSeq
    |> fun arr -> new System.String(arr)


let part1() =
    getInput() |> unjam |> printfn "Unjammed code: %s" 


let test1() =
    let shouldBe = AdventUtils.printTestResult 1 (=)
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
    let result = unjam testInput
    result |> shouldBe "easter"


part1()
test1()