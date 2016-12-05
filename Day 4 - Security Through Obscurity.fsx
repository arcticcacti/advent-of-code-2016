open System.Text.RegularExpressions

type Room = { name: string; sectorId: string; checksum: string }

let getInput() =
    let path = "input\day_4.txt"
    System.IO.File.ReadAllLines path


/// parse a line of data into a Room
let parseRoomData line =
    let pattern = "([\w-]+)-(\d+)\[(\w+)]" 
    let m = Regex.Match(line, pattern)
    let groups = [ for g in m.Groups -> g.Value ]
    {name = groups.[1]; sectorId = groups.[2]; checksum = groups.[3]}


/// determine if a room is real by validating its name by its checksum
let isRealRoom room =
    /// letter, frequency pairs
    let nameLettersByFreq =
        room.name
        |> Seq.filter ((<>) '-')
        |> Seq.countBy id
        

    /// take (char, frequency) pairs and sort by highest frequency and alphabetical order
    let sortByFreqAndOrder letterFreqs =
        letterFreqs
        |> Seq.groupBy snd
        |> Seq.sortByDescending fst
        |> Seq.map snd
        |> Seq.collect (Seq.sortBy fst)
        |> Seq.map fst

    // list the name letters in the by frequency/alphabetical order,
    // then check the checksum matches the start of the list 
    nameLettersByFreq
    |> sortByFreqAndOrder
    |> Seq.zip room.checksum
    |> Seq.forall (fun (a, b) -> a = b)


(*
    Runnables
*)

let part1() =
    getInput()
    |> Seq.map parseRoomData
    |> Seq.filter isRealRoom
    |> Seq.sumBy (fun room -> int room.sectorId)
    |> printfn "Sector ID total: %d"

let test() =
    let testInput = parseRoomData >> isRealRoom
    let testData = [|
        "aaaaa-bbb-z-y-x-123[abxyz]", true;
        "a-b-c-d-e-f-g-h-987[abcde]", true;
        "not-a-real-room-404[oarel]", true;
        "totally-real-room-200[decoy]", false;
    |]
    let showResults (name, expected) =
        if testInput name = expected then
            printfn "Test passed"
        else
            printfn "Test failed for input: %s" name

    testData |> Array.iter showResults

part1()
test()

