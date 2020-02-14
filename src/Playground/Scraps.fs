namespace Scraps

type FrameData = { Player: int; Frame: int; Inputs: int list }

module Funcs =
    // Deprecated, didn't realize chunkBySize existed. RIP.
    // Tested on a 100k list:
    // List.chunkBySize -> ~9ms
    // chunker -> ~14ms (without reverse, it's very close.)
    let rec chunker l chunk chunks =
        match l, chunk with
            | [], _ -> List.rev chunks
            | h :: t, [] -> chunker t [h] chunks
            | h :: t, [c] -> chunker t [] ([c; h] :: chunks)
            | _ -> failwith "List length was odd."

    // This one is faster! tested with 50k pairs:
    // frameBuilder -> ~10ms, ~3ms (another run)
    // frameBuilder2 -> ~24ms, ~10ms
    let frameBuilder pNum firstFrame inputPairs =
        let newFrame n i = { Player = pNum; Frame = n; Inputs = i }

        let rec map num inputs fs =
            match inputs with
            | [] -> fs
            | h :: t -> map (num - 1) t ((newFrame num h) :: fs)

        map firstFrame inputPairs []

    let frameBuilder2 pNum firstFrame (inputPairs: int list list) =
        let newFrame n i = { Player = pNum; Frame = n; Inputs = i }
        let fNums = [firstFrame .. -1 .. (firstFrame - inputPairs.Length + 1)]
        List.map2 newFrame fNums inputPairs
