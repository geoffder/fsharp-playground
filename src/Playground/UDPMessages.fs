module UDPMessages

open System.Net
open System.Net.Sockets

type Message = Msg of byte array

let strToBytes (str: string) =
    str |> System.Text.Encoding.ASCII.GetBytes

let bytesToStr (bs: byte array) =
    bs |> System.Text.Encoding.ASCII.GetString

let sendingAgent port = MailboxProcessor.Start(fun inbox ->
    let client = new UdpClient ()
    let endpoint = IPEndPoint (IPAddress.Any, port)
    let rec loop () = async {
        match! inbox.Receive () with
        | Msg bytes ->
            do client.Send (bytes, bytes.Length, endpoint) |> ignore
            return! loop ()
    }
    loop ()
)

let send (agent: MailboxProcessor<Message>) str =
    str |> strToBytes |> Msg |> agent.Post

let receiving (port: int) =
    let client = new UdpClient (port)
    let rec loop () = async {
        let! result = client.ReceiveAsync() |> Async.AwaitTask
        do result.RemoteEndPoint |> printf "%A says: "
        do result.Buffer |> bytesToStr |> printfn "%s"
        return! loop ()
    }
    loop () |> Async.Start
