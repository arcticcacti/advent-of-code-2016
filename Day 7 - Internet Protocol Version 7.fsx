#load "AdventUtils.fsx"

let getInput() = AdventUtils.inputFromFile "day_7.txt"

/// split an ordered sequence into its odd and even elements
let groupAlternating =
    let i = ref -1
    Seq.groupBy (fun _ -> incr i; !i % 2)
    >> Seq.map snd
    >> Seq.take 2
    >> Seq.toArray
    >> fun arrs -> arrs.[0], arrs.[1]


/// check if four elements form an ABBA
let isAbba =
    function
    | a,b,c,d when a=d && b=c && a<>b
        -> true
    | _ 
        -> false

let isAba =
    function
    | a,b,c when a=c && a<>b
        -> true
    | _
        -> false


/// all ABBA sequences in a string
let containedAbbas =
    Seq.windowed 4
    >> Seq.map (fun a->a.[0], a.[1], a.[2], a.[3])
    >> Seq.filter isAbba


/// all ABA sequences in a string
let containedAbas =
    Seq.windowed 3
    >> Seq.map (fun a -> a.[0], a.[1], a.[2])
    >> Seq.filter isAba


/// flip an ABA into a BAB
let abaToBab =
    function
    | (a,b,_) as t when t |> isAba
        -> (b,a,b)
    | t 
        -> failwithf "Not an ABA: %A" t


/// split a string into alternating 'normal' sections and sections that were enclosed in square brackets
let splitIp (ip:string) = ip.Split([|'['; ']'|]) |> groupAlternating


/// determine if an IP string conforms to the rules for TLS support
let supportsTls ip =
    let supernets, hypernets = ip |> splitIp
    let hasAnAbba = Seq.exists (containedAbbas >> Seq.isEmpty >> not)

    if hypernets |> hasAnAbba then false
    else supernets |> hasAnAbba

/// determine if an IP string conforms to the rules for SSL support
let supportsSsl ip =
    let supernets, hypernets = ip |> splitIp
    let abas = supernets |> Seq.collect containedAbas
    let potentialBabs = hypernets |> Seq.collect containedAbas

    abas
    |> Seq.map abaToBab
    |> Seq.exists (fun x -> Seq.exists ((=) x) potentialBabs)

//
// Main functions
//

let part1() =
    getInput()
    |> Seq.filter supportsTls
    |> Seq.length
    |> printfn "Part 1 - %d IPs support TLS"


let part2() =
    getInput()
    |> Seq.filter supportsSsl
    |> Seq.length
    |> printfn "Part 2 - %d IPs support SSL"

// Tests

let test1() =
    let testData = [|
        "abba[mnop]qrst", true;
        "abcd[bddb]xyyx", false;
        "aaaa[qwer]tyui", false;
        "ioxxoj[asdfgh]zxcvbn", true
    |]
    let test = AdventUtils.testResultIsExpected "1" supportsTls 
    testData |> Array.iter test

let test2() =
    let testData = [|
        "aba[bab]xyz", true;
        "xyx[xyx]xyx", false;
        "aaa[kek]eke", true;
        "zazbz[bzb]cdb", true
    |]
    let test = AdventUtils.testResultIsExpected "2" supportsSsl 
    testData |> Array.iter test

part1()
part2()

test1()
test2()