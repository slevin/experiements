namespace cocoa

open System
open System.Drawing
open MonoMac.ObjCRuntime
open MonoMac.Foundation
open MonoMac.AppKit

[<Register("AppDelegate")>]
type AppDelegate() = 
    inherit NSApplicationDelegate()
    override x.FinishedLaunching(notification) = 
        let mainWindowController = new cocoa.MainWindowController()
        mainWindowController.Window.MakeKeyAndOrderFront(x)

module main = 
    [<EntryPoint>]
    let main args = 
        NSApplication.Init()
        NSApplication.Main(args)
        0

