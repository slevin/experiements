namespace cocoa

open System
open MonoMac.ObjCRuntime
open MonoMac.Foundation
open MonoMac.AppKit

[<Register ("MainWindow")>]
type MainWindow =
    inherit NSWindow

    new () = { inherit NSWindow () }
    new (handle : IntPtr) = { inherit NSWindow (handle) }

    [<Export ("initWithCoder:")>]
    new (coder : NSCoder) = { inherit NSWindow (coder) }

