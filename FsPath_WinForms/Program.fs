// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

module FsPath.Program

open System
open System.Windows.Forms
open FsPath.PathParser

let createForm () =
    new Form(
      Name = "FsPath",
      Text = "FsPath",
      Width = 800,
      Height = 600,
      MaximizeBox = false)

[<STAThread>]
do 
  use f = createForm()

  Parse "M,test -1.234"

  f |> Application.Run  
