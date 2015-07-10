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
        ctx.ClearRect rect
        UIColor.Red.SetFill()
        // for each of my snakesquare elements should draw a square
        this.Snake.body |> Seq.iter (fun u -> this.DrawSquare (ctx, u))

    member this.DrawSquare(ctx, sq) =
        // given a snake square how does it draw?
        let piece = RectangleF(float32 (fst sq) * 20.0f, float32 (snd sq) * 20.0f, 20.0f, 20.0f)
        ctx.FillRect(piece)

    static member NewSnake() =
        { Snake.direction = Right; Snake.body = [ (0, 0) ] }

    member this.Step() =
        this.MoveForward()
        this.SetNeedsDisplay()

    member this.MoveForward() =
        let newBody = this.NextSquare(this.Snake.body.Head, this.Snake.direction) :: this.Snake.body
        this.Snake <- { 
            direction = this.Snake.direction;
            body = Seq.take (newBody.Length - 1) newBody |> List.ofSeq
            }

            

    member this.NextSquare( (x, y), dir : SnakeMove) =
        match dir with
        | Up -> (x, y - 1)
        | Down -> (x, y + 1)
        | Left -> (x - 1, y)
        | Right -> (x + 1, y)

    member this.MoveTowards( dir : SnakeMove ) =
        this.Snake <- {
            direction = dir;
            body = this.Snake.body
            }

[<Register("smallViewController")>]
type smallViewController() = 
    inherit UIViewController()
    member val MySnakeView : SnakeView = new SnakeView() with get, set

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
        this.MySnakeView.Frame <- RectangleF(
            this.View.Bounds.X + 10.0f,
            this.View.Bounds.Y + 10.0f,
            this.View.Bounds.Width - 20.0f,
            this.View.Bounds.Height - 20.0f)
        this.View.AddSubview(this.MySnakeView)
        let b = UIButton.FromType(UIButtonType.System)
        b.SetTitle("Update", UIControlState.Normal)
        b.Frame <- RectangleF(
            10.0f,
            this.View.Bounds.Height - 50.0f,
            100.0f,
            44.0f)
        b.TouchUpInside.Add (fun x -> this.MySnakeView.Step())
        this.View.AddSubview(b)

        let lft = new UISwipeGestureRecognizer(Action<_> this.HandleSwipe)
        lft.Direction <- UISwipeGestureRecognizerDirection.Left
        this.View.AddGestureRecognizer lft

        let rgt = new UISwipeGestureRecognizer(Action<_> this.HandleSwipe)
        rgt.Direction <- UISwipeGestureRecognizerDirection.Right
        this.View.AddGestureRecognizer rgt

        let up = new UISwipeGestureRecognizer(Action<_> this.HandleSwipe)
        up.Direction <- UISwipeGestureRecognizerDirection.Up
        this.View.AddGestureRecognizer up

        let dwn = new UISwipeGestureRecognizer(Action<_> this.HandleSwipe)
        dwn.Direction <- UISwipeGestureRecognizerDirection.Down
        this.View.AddGestureRecognizer dwn



    member this.HandleSwipe(gr : UISwipeGestureRecognizer) =
        this.MySnakeView.MoveTowards <| 
        match gr.Direction with
        | UISwipeGestureRecognizerDirection.Up -> Up
        | UISwipeGestureRecognizerDirection.Left -> Left
        | UISwipeGestureRecognizerDirection.Right -> Right
        | UISwipeGestureRecognizerDirection.Down -> Down
        | _ -> Up

    // Return true for supported orientations
    override this.ShouldAutorotateToInterfaceOrientation(orientation) = 
        orientation <> UIInterfaceOrientation.PortraitUpsideDown
