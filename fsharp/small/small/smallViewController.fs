namespace small

open System
open System.Drawing
open MonoTouch.UIKit
open MonoTouch.Foundation

[<Register("smallViewController")>]
type smallViewController() = 
    inherit UIViewController()
    // Release any cached data, images, etc that aren't in use.
    override this.DidReceiveMemoryWarning() = base.DidReceiveMemoryWarning()
    // Perform any additional setup after loading the view, typically from a nib.
    override this.ViewDidLoad() = 
        base.ViewDidLoad()
        let iv = new UIImageView(this.View.Bounds)
        let im = new UIImage("ziggy.jpg")
        iv.Image <- im
        this.View.AddSubview(iv)
    // Return true for supported orientations
    override this.ShouldAutorotateToInterfaceOrientation(orientation) = 
        orientation <> UIInterfaceOrientation.PortraitUpsideDown

