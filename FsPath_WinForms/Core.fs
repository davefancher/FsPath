namespace FsPath

open System
open System.Collections
open System.Collections.Generic
open System.Diagnostics
open System.Drawing
open System.Drawing.Drawing2D
open System.IO
open System.Text

type charEnum = IEnumerator<char>

type Point (x, y) =
  member this.X = x
  member this.Y = y

type FillRule =
  | EvenOdd = 0
  | NonZero = 1

type IPathCommand =
  abstract Invoke : unit -> unit

type FillStyleCommand(rule : FillRule) =
  member x.Rule = rule
  interface IPathCommand with
    member x.Invoke () = ()

type MoveToCommand(dest : Point) =
  member x.Destination = dest
  interface IPathCommand with
    member x.Invoke () = ()

type CubicBezierCurveCommand (cp1 : Point, cp2 : Point, ep : Point) =
  member x.ControlPoint1 = cp1
  member x.ControlPoint2 = cp2
  member x.EndPoint = ep
  interface IPathCommand with
    member x.Invoke () = ()

module PathParser =
  [<Literal>]
  let private UnexpectedEndOfStreamMessage = "Unexpected end of stream while parsing fill style command"
  [<Literal>]
  let private InvalidFillStyleMessage = "Invalid fill style option. Valid options are 0 (evenodd) and 1 (nonzero)"

  let private commandCharacters = [| 'M'; 'L'; 'C'; 'Q'; 'A'; 'Z' |]

  let private isCommandCharacter current = commandCharacters |> Array.exists ((=) current)
  let private isWhitespaceCharacter = Char.IsWhiteSpace
  let private isNegativeSign = (=) '-'
  let private isComma = (=) ','
  let private isDecimal = (=) '.'
  let private isDigit = Char.IsDigit

  let private skipWhitespace (e: charEnum) =
    let rec loop () =
      if e.Current |> isWhitespaceCharacter then
        e.MoveNext() |> ignore
        loop()
    loop()
    e
  
  let private skipComma (e: charEnum) =
    if e.Current |> isComma then e.MoveNext() |> ignore
    e
  
  let private skipArgumentSeparator : (charEnum -> charEnum) =
    skipWhitespace >> skipComma >> skipWhitespace
  
  let private readNumber (e : charEnum) =
    let readNextDigits () =
      let rec loop (digits : string) =
        let current = e.Current
        if current |> isDigit then
          e.MoveNext() |> ignore
          loop (string current) + digits
        else
          digits
      "" |> loop
    let appendAndMoveNext (sb : StringBuilder) =
      e.Current |> sb.Append |> ignore
      e.MoveNext() |> ignore
    let number = StringBuilder();
    if e.Current |> isNegativeSign then number |> appendAndMoveNext
    if e.Current |> isDigit then
      readNextDigits() |> number.Append |> ignore
    if e.Current |> isDecimal then
      number |> appendAndMoveNext
      if e.Current |> isDigit then
        readNextDigits() |> number.Append |> ignore
        if e.Current = 'E' then
          number |> appendAndMoveNext
          if e.Current |> isNegativeSign then number |> appendAndMoveNext
          if not (e.Current |> isDigit) then failwith "Invalid Number"
          readNextDigits |> number.Append |> ignore
    double (number.ToString())

  let readNumberAndSkipSeparator e =
    let num = e |> readNumber
    e |> skipArgumentSeparator |> ignore
    num

  let readNumberAndSkipWhitespace e =
    let num = e |> readNumber
    e |> skipWhitespace |> ignore
    num

  let readPoint e =
    let x = e |> readNumberAndSkipSeparator
    let y = e |> readNumber
    Point(x, y)

  let readPointAndSkipSeparator e =
    let p = e |> readPoint
    e |> skipArgumentSeparator |> ignore
    p

  let readPointAndSkipWhitespace e =
    let p = e |> readPoint
    e |> skipWhitespace |> ignore
    p

  let parseFillStyleCommand (e : charEnum) =
    if not (e.MoveNext()) then failwith UnexpectedEndOfStreamMessage
    let rule =
      match e |> readNumberAndSkipWhitespace with
      | 0. -> FillRule.EvenOdd
      | 1. -> FillRule.NonZero
      | _ -> failwith InvalidFillStyleMessage
    [ FillStyleCommand(rule) :> IPathCommand ]

  let parseMoveToCommand (e : charEnum) =
    if not (e.MoveNext()) then failwith UnexpectedEndOfStreamMessage
    let p = e |> skipWhitespace |> readPointAndSkipWhitespace
    [ MoveToCommand(p) :> IPathCommand ]

  let parseCubicBezierCurveCommand (e : charEnum) =
    if not (e.MoveNext()) then failwith UnexpectedEndOfStreamMessage
    let rec getNextCommand commands =
      if not (e.Current |> isCommandCharacter) then
        CubicBezierCurveCommand(
          e |> readPointAndSkipSeparator,
          e |> readPointAndSkipSeparator,
          e |> readPointAndSkipSeparator) :> IPathCommand :: getNextCommand commands
      else
        commands
    getNextCommand []




  let Parse (pathText : string) =
    let enumerator = (pathText |> Seq.cast<char>).GetEnumerator()
    while(enumerator.MoveNext()) do
//      sprintf "Character %c" enumerator.Current |> Debug.WriteLine
//      sprintf "- Is Command Character? %b" (isCommandCharacter enumerator.Current) |> Debug.WriteLine
//      sprintf "- Is Whitespace? %b" (isWhitespaceCharacter enumerator.Current) |> Debug.WriteLine
//      sprintf "- Is Negative? %b" (isNegativeSign enumerator.Current) |> Debug.WriteLine
//      sprintf "- Is Comma? %b" (isComma enumerator.Current) |> Debug.WriteLine
//      sprintf "- Is Decimal? %b" (isDecimal enumerator.Current) |> Debug.WriteLine
//      sprintf "- Is Digit? %b" (isDigit enumerator.Current) |> Debug.WriteLine
    ()