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

let sendingAgent () = Agent<IPEndPoint * Message>.Start(fun inbox ->
    let client = new UdpClient ()
    let rec loop () = async {
        let! endpoint, msg =  inbox.Receive ()
        msg
        |> JsonConvert.SerializeObject
        |> strToBytes
        |> fun bs -> client.Send (bs, bs.Length, endpoint)
        |> ignore
        return! loop ()
    }
    loop ()
)

// Non-lambda methods of creating Agents (exploration)
let sendingAgentLoop (inbox: Agent<IPEndPoint * Message>) =
    let client = new UdpClient ()
    let rec loop () = async {
        let! endpoint, msg =  inbox.Receive ()
        msg
        |> JsonConvert.SerializeObject
        |> strToBytes
        |> fun bs -> client.Send (bs, bs.Length, endpoint)
        |> ignore
        return! loop ()
    }
    loop ()

let sendingAgent2 () = start sendingAgentLoop

let sendingAgent3 () =
    let client = new UdpClient ()
    let agent (inbox: Agent<IPEndPoint * Message>) =
        let rec loop () = async {
            let! endpoint, msg =  inbox.Receive ()
            msg
            |> JsonConvert.SerializeObject
            |> strToBytes
            |> fun bs -> client.Send (bs, bs.Length, endpoint)
            |> ignore
            return! loop ()
        }
        loop ()
    start agent

let receiving (port: int) (sender: Agent<IPEndPoint * Message>) =
    let client = new UdpClient (port)
    let rec loop () = async {
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
        return! loop ()
    }
    loop () |> Async.Start

let ping (sender: Agent<IPEndPoint * Message>) port =
    sender <-- (IPEndPoint (IPAddress.Any, port), Ping)

let chat (sender: Agent<IPEndPoint * Message>) port str =
    sender <-- (IPEndPoint (IPAddress.Any, port), Chat str)

let slowHello (sender: Agent<IPEndPoint * Message>) port =
    fun () -> chat sender port "Hello"
    |> delayAction 5000
