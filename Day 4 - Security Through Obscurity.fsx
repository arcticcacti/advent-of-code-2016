open System.Text.RegularExpressions
#load "AdventUtils.fsx"

type Room = { name: string; sectorId: int; checksum: string }

let getInput() = AdventUtils.inputFromFile "day_4.txt"


/// parse a line of data into a Room
let parseRoomData line =
    let pattern = "([\w-]+)-(\d+)\[(\w+)]" 
    let m = Regex.Match(line, pattern)
    let groups = [ for g in m.Groups -> g.Value ]
    {name = groups.[1]; sectorId = int groups.[2]; checksum = groups.[3]}


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


/// rotate letters through the alphabet, wrapping from z to a
let rec shift steps letter =
    let nextChar = int >> (+) 1 >> char
    match letter, steps with
    |  c,  0 -> c
    | 'z', n -> shift (n-1) 'a' 
    |  c,  n -> shift (n-1) (nextChar c)


/// decrypt a room name by applying a shift to each letter, and replacing dashes with spaces
let decryptName name shiftSteps =
    name
    |> Seq.map (fun c -> if c = '-' then ' ' else shift shiftSteps c)
    |> Array.ofSeq
    |> fun x -> new System.String(x)


(*
    Runnables
*)

let part1() =
    getInput()
    |> Seq.map parseRoomData
    |> Seq.filter isRealRoom
    |> Seq.sumBy (fun room -> room.sectorId)
    |> printfn "Sector ID total: %d"


let part2() =
    getInput()
    |> Seq.map parseRoomData
    |> Seq.filter isRealRoom
    |> Seq.map (fun room -> {room with name = (decryptName room.name room.sectorId)})
    |> Seq.filter (fun room -> room.name.Contains "pole")
    |> Seq.iter (fun room -> printfn "%s - %d" room.name room.sectorId)


let test1() =
    let testData = [|
        "aaaaa-bbb-z-y-x-123[abxyz]", true;
        "a-b-c-d-e-f-g-h-987[abcde]", true;
        "not-a-real-room-404[oarel]", true;
        "totally-real-room-200[decoy]", false;
    |]
    let runTest (name, expected) =
        let result = name |> parseRoomData |> isRealRoom
        let testId = sprintf "1 - %s" name
        let shouldMatch = AdventUtils.printTestResult testId (=)
        result |> shouldMatch expected

    testData |> Array.iter runTest


let test2() =
    let shouldMatch = AdventUtils.printTestResult 2 (=)
    let testData = "qzmt-zixmtkozy-ivhz-343[whatever]"
    let testRoom = parseRoomData testData
    let result = decryptName testRoom.name testRoom.sectorId
    result |> shouldMatch "very encrypted name"

part1()
part2()

test1()
test2()

