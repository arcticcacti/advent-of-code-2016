
/// read a file in the data directory
let inputFromFile filename =
    let pathToFiles = @"input\"
    System.IO.File.ReadAllLines (pathToFiles + filename)

/// compare values with a predicate and print the results
let printTestResult testId f expected result =
    if result |> f expected then
        printfn "Test (%A) passed" testId
    else
        printfn "Test (%A) failed\nExpected: %A\n Got: %A" testId expected result