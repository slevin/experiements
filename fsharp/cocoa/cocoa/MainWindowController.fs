namespace cocoa

open System
open MonoMac.ObjCRuntime
open MonoMac.Foundation
open MonoMac.AppKit

[<Register("MainWindowController")>]
type MainWindowController =
    inherit NSWindowController

    new () = { inherit NSWindowController ("MainWindow") }
    new (handle : IntPtr) = { inherit NSWindowController (handle) }
