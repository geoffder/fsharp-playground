module UDPMessages

open Newtonsoft.Json
open System.Net
open System.Net.Sockets

type Agent<'T> = MailboxProcessor<'T>

type Message =
    | Chat of string
    | Ping
    | Pong

let (<--) (a: Agent<'T>) msg = a.Post msg
let (<->) (a: Agent<'T>) msg = a.PostAndReply msg
let (<-?->) (a: Agent<'T>) (msg, ms) = a.TryPostAndReply (msg, ?timeout = Some ms)
let start (agent: Agent<'T> -> Async<unit>) = Agent<'T>.Start agent

let strToBytes (str: string) =
    str |> System.Text.Encoding.ASCII.GetBytes

let bytesToStr (bs: byte array) =
    bs |> System.Text.Encoding.ASCII.GetString

let delayAction ms f =
    async {
        do! Async.Sleep ms
        do f ()
    } |> Async.Start

let sendingAgent (port: int) = Agent<IPEndPoint * Message>.Start(fun inbox ->
    let client = new UdpClient (port)
    let rec loop () = async {
        let! endpoint, msg = inbox.Receive ()
        msg
        |> JsonConvert.SerializeObject
        |> strToBytes
        |> fun bs -> client.Send (bs, bs.Length, endpoint)
        |> ignore
        return! loop ()
    }
    loop ()
)

let receiving (port: int) (sender: Agent<IPEndPoint * Message>) =
    let client = new UdpClient (port)
    let rec loop i = async {
        let! result = client.ReceiveAsync() |> Async.AwaitTask
        do result.Buffer
        |> bytesToStr
        |> JsonConvert.DeserializeObject<Message>
        |> function
           | Chat str -> printfn "%A says: %s" result.RemoteEndPoint str
           | Ping ->
               printfn "Ping from %A!" result.RemoteEndPoint
               sender.Post (result.RemoteEndPoint, Pong)
           | Pong ->
               printfn "Pong from %A!" result.RemoteEndPoint
        do printfn "count = %i" i
        return! loop (i + 1)
    }
    loop 0 |> Async.Start

let ping (sender: Agent<IPEndPoint * Message>) port =
    sender <-- (IPEndPoint (IPAddress.Any, port), Ping)

let chat (sender: Agent<IPEndPoint * Message>) port str =
    sender <-- (IPEndPoint (IPAddress.Any, port), Chat str)

let slowHello (sender: Agent<IPEndPoint * Message>) port =
    fun () -> chat sender port "Hello"
    |> delayAction 5000

let blast port sender = async {
    for i in 1..10 do
        chat sender port (sprintf "%i" i)
        // do! Async.Sleep 1
}

// List.map (blast 4000) [s1; s2] |> Async.Parallel |> Async.RunSynchronously;;
