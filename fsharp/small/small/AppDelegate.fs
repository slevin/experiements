namespace small

open System
open MonoTouch.UIKit
open MonoTouch.Foundation

[<Register("AppDelegate")>]
type AppDelegate() = 
    inherit UIApplicationDelegate()
    member val Window = null with get, set
    // This method is invoked when the application is ready to run.
    override this.FinishedLaunching(app, options) = 
        this.Window <- new UIWindow(UIScreen.MainScreen.Bounds)
        let viewController = new smallViewController()
        viewController.View.BackgroundColor <- UIColor.White
        this.Window.RootViewController <- viewController
        this.Window.MakeKeyAndVisible()
        true

module Main = 
    [<EntryPoint>]
    let main args = 
        UIApplication.Main(args, null, "AppDelegate")
        0

