
open System









let interpret() =
    let cmd = Environment.GetCommandLineArgs()
    let filePath = cmd |> Array.tail
    ()



interpret()