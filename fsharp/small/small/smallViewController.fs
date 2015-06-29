namespace small

open System
open System.Drawing
open MonoTouch.UIKit
open MonoTouch.Foundation
open MonoTouch.CoreGraphics

type SnakeMove =
    | Up
    | Down
    | Left
    | Right

type SnakeSquare = int * int
type Snake = {
    direction : SnakeMove;
    body : SnakeSquare list
    }


[<Register("SnakeView")>]
type SnakeView() =
    inherit UIView()
    member val Snake : Snake = SnakeView.NewSnake() with get, set

    override this.Draw(rect) =
        base.Draw(rect)
        let ctx = UIGraphics.GetCurrentContext()
        UIColor.Red.SetFill()
        // for each of my snakesquare elements should draw a square
        this.Snake.body |> Seq.iter (fun u -> this.DrawSquare (ctx, u))

    member this.DrawSquare(ctx, sq) =
        // given a snake square how does it draw?
        let piece = RectangleF(float32 (fst sq) * 20.0f, float32 (snd sq) * 20.0f, 20.0f, 20.0f)
        ctx.FillRect(piece)

    static member NewSnake() =
        { Snake.direction = Right; Snake.body = [ (0, 0) ] }


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
