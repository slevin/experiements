namespace small

open System
open System.Drawing
open MonoTouch.UIKit
open MonoTouch.Foundation
open MonoTouch.CoreGraphics

[<Register("SnakeView")>]
type SnakeView() =
    inherit UIView()

    override this.Draw(rect) =
        base.Draw(rect)
        let ctx = UIGraphics.GetCurrentContext()
        UIColor.Red.SetFill()
        ctx.FillRect(rect)


[<Register("smallViewController")>]
type smallViewController() = 
    inherit UIViewController()

    // Release any cached data, images, etc that aren't in use.
    override this.DidReceiveMemoryWarning() = 
        base.DidReceiveMemoryWarning()
    
    // Perform any additional setup after loading the view, typically from a nib.
    override this.ViewDidLoad() = 
        base.ViewDidLoad()
        let iv = new UIImageView(this.View.Bounds)
        let im = new UIImage("ziggy.jpg")
        iv.Image <- im
        this.View.AddSubview(iv)
        let pt = CoreGraphics.CGPoint(this.View.Bounds.X + 10, this.View.Bounds.Y + 10)
        let sz = CGSize(CGPoint(this.View.Bounds.Width - 20, this.View.Bounds.Height - 20))
        let less = CGRect(pt, sz)
        let sv = new SnakeView(less)
        this.View.AddSubview(sv)

    // Return true for supported orientations
    override this.ShouldAutorotateToInterfaceOrientation(orientation) = 
        orientation <> UIInterfaceOrientation.PortraitUpsideDown
