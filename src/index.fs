module GcloudFunction
open Fable.Import
open System

module ExpressHelpers = 

  type ExpressContext = {
    Request: express.Request
    Response: express.Response
    Content: string
  }

  type ExpressPart = ExpressContext -> ExpressContext option

  let bind p1 ctx = 
    match ctx with
    | Some ctx' -> p1 ctx'
    | None -> None

  let compose p1 p2 ctx = 
    match p1 ctx with
    | None -> None
    | Some ctx' -> p2 ctx'

  let (>>=) x p = p x // ctx >>= expressPart --> context

  let (>=>) p1 p2 = compose p1 p2 // expressPart1 >=> expressPart2 --> expressPart3

  let rec choose parts ctx = 
    match parts with
    | [] -> None
    | p::ps ->
      match p ctx with
      | None -> choose ps ctx
      | Some ctx -> Some ctx

  let path s ctx = 
    if ctx.Request.path.StartsWith(s)
    then Some ctx
    else None

  let path1 s part =
    let p = 
      fun ctx ->
        match path s ctx with
        | None -> None
        | Some ctx ->
          let urlPath = ctx.Request.path
          let rest = urlPath.Substring(urlPath.Length)
          printfn "This is the rest: %s" rest
          printfn "urlPath: %s" urlPath
          let uParts = rest.Split('/')
          printfn "parts: %A" uParts
          if uParts |> Array.length > 0 
          then part (uParts.[0]) ctx
          else None
    p

  let answer = function
    | Some ctx ->
      ctx.Response.send ctx.Content
    | None -> raise (exn "Failed")

  let execute request response app = 
    let ctx = { Request = request; Response = response; Content = "" }
    ctx |> (app >> answer)

  let notFound str ctx =
    let res = ctx.Response.status 404.
    { ctx with Response = res; Content = str} |> Some

open ExpressHelpers

let hasBodyPart str ctx = 
  if ctx.Request.body.ToString().Contains(str) 
  then 
    {
      ctx with 
        Content = ctx.Content + "Hello: " + str + "Path: " + ctx.Request.path + "Route: " + (ctx.Request.route.ToString()) + "Url: " + ctx.Request.url + "Query: " + (ctx.Request.query.ToString()) + "OriginalUrl: " + ctx.Request.originalUrl
    } |> Some
  else
    None

let subApp1 = hasBodyPart "Hello" >=> hasBodyPart "Tomas"

let subApp2 = hasBodyPart "Yolo"

let app = 
      choose 
        [
          path "/app1" >=> subApp1
          path "/app2" >=> subApp2
          path1 "/tomas/" (fun str -> notFound ("This is a string" + str))
          notFound "Stupid stupid me"
        ]

let helloTomas (request: express.Request) (response: express.Response) = 
  execute request response app
