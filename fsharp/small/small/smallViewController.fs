namespace small

open System
open System.Drawing
open MonoTouch.UIKit
open MonoTouch.Foundation
open MonoTouch.CoreGraphics

type SnakeSquare = int * int
type Snake = SnakeSquare list

[<Register("SnakeView")>]
type SnakeView() =
    inherit UIView()
    member val Snake : Snake = [] with get, set

    override this.Draw(rect) =
        base.Draw(rect)
        let ctx = UIGraphics.GetCurrentContext()
        UIColor.Red.SetFill()
        // for each of my snakesquare elements should draw a square
        this.Snake |> Seq.iter (fun u -> this.DrawSquare (ctx, u))

    member this.DrawSquare(ctx, sq) =
        // given a snake square how does it draw?
        ctx.FillRect(RectangleF(fst sq * 20, snd sq * 20, 20, 20))
    

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
        let sv = new SnakeView()
        sv.Frame <- RectangleF(
            this.View.Bounds.X + 10.0f,
            this.View.Bounds.Y + 10.0f,
            this.View.Bounds.Width - 20.0f,
            this.View.Bounds.Height - 20.0f)
        this.View.AddSubview(sv)

    // Return true for supported orientations
    override this.ShouldAutorotateToInterfaceOrientation(orientation) = 
        orientation <> UIInterfaceOrientation.PortraitUpsideDown
