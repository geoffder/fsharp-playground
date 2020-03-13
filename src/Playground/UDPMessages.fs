module UDPMessages

open Newtonsoft.Json
open System.Net
open System.Net.Sockets

type Message =
    | Chat of string
    | Ping
    | Pong

let strToBytes (str: string) =
    str |> System.Text.Encoding.ASCII.GetBytes

let bytesToStr (bs: byte array) =
    bs |> System.Text.Encoding.ASCII.GetString

let sendingAgent () = MailboxProcessor<IPEndPoint * Message>.Start(fun inbox ->
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

let receiving (port: int) (sender: MailboxProcessor<IPEndPoint * Message>) =
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

let ping (sender: MailboxProcessor<IPEndPoint * Message>) port =
    sender.Post <| (IPEndPoint (IPAddress.Any, port), Ping)
let chat (sender: MailboxProcessor<IPEndPoint * Message>) port str =
    sender.Post <| (IPEndPoint (IPAddress.Any, port), Chat str)
