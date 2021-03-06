package main

import "flag"
import "fmt"
import "time"
import "runtime"
import "github.com/veandco/go-sdl2/sdl"
import "github.com/veandco/go-sdl2/sdl_image"
import "github.com/ungerik/go3d/vec2"
import "os"
import "log"
import "runtime/pprof"

const width = 800
const height = 600

var cpuprofile = flag.String("cpuprofile", "", "write cpu profile to file")

func rect32(x int, y int, w int, h int) sdl.Rect {
	return sdl.Rect{int32(x), int32(y), int32(w), int32(h)}
}

func point32(x int, y int) sdl.Point {
	return sdl.Point{int32(x), int32(y)}
}

type Vehicle struct {
	pos vec2.T
	vel vec2.T
	acc vec2.T
	maxSpeed float32
	maxSteer float32
	box sdl.Rect // for drawing
}

func (v *Vehicle) updateBox() {
	v.box.X = int32(v.pos[0])
	v.box.Y = int32(v.pos[1])
}

func (v *Vehicle) bound(width int, height int) {
	w := float32(width) - float32(v.box.W)
	h := float32(height) - float32(v.box.H)

	if v.pos[0] < 0 {
		v.pos[0] = 0
		v.vel[0] = -1 * v.vel[0]
	}
	if v.pos[0] > w  {
		v.pos[0] = w
		v.vel[0] = -1 * v.vel[0]
	}
	if v.pos[1] < 0 {
		v.pos[1] = 0
		v.vel[1] = -1 * v.vel[1]
	}
	if v.pos[1] > h {
		v.pos[1] = h
		v.vel[1] = -1 * v.vel[1]
	}
}

func limitVec(t *vec2.T, max float32) {
	// limits magnitude to max
	if t.LengthSqr() > max * max {
		t.Normalize()
		t.Scale(max)
	}
}

type Crosshairs struct {
	pos vec2.T
	sz float32
}

func (c *Crosshairs) render(r *sdl.Renderer) {
	r.SetDrawColor(0x00, 0xFF, 0x00, 0xFF)
	r.DrawLine(int(c.pos[0] - c.sz), int(c.pos[1]), int(c.pos[0] + c.sz), int(c.pos[1]))
	r.DrawLine(int(c.pos[0]), int(c.pos[1] - c.sz), int(c.pos[0]), int(c.pos[1] + c.sz))
}

func main() {
	flag.Parse()
    if *cpuprofile != "" {
        f, err := os.Create(*cpuprofile)
        if err != nil {
            log.Fatal(err)
        }
        pprof.StartCPUProfile(f)
        defer pprof.StopCPUProfile()
    }

	fmt.Println("Started...")
	runtime.LockOSThread()

	window, renderer := initSDL()
	defer window.Destroy()
	defer renderer.Destroy()

	// example draw green square
	//	renderer.SetDrawColor(0x00, 0xFF, 0x00, 0xFF)
	//	renderer.DrawRect(&sdl.Rect{0,0,100,100})

	shipTexture, _ := img.LoadTexture(renderer, "../plane.png")
	defer shipTexture.Destroy()

	// correct size
	_, _, shipW, shipH, _ := shipTexture.Query()
	sdlBox := rect32(0, 0, shipW, shipH)

	// center it
	x1 := (width - shipW) / 2
	y1 := (height - shipH) / 2
	pos := vec2.T{float32(x1), float32(y1)}

	ship := Vehicle{pos, vec2.Zero, vec2.Zero, 5, 2, sdlBox}

	// clear with black
	renderer.SetDrawColor(0, 0, 0, 0xFF)

	crosshairs := Crosshairs{vec2.T{200, 200}, 20}

//	gravity := vec2.T{0, 10}
//	wind := vec2.T{1, 0}
	speed := 10

	ticker := time.NewTicker(time.Millisecond * 16)
	keepRunning := true
	ticks := sdl.GetTicks()

	for keepRunning {
		for {
			e := sdl.PollEvent()
			if e == nil {
				break
			}
			switch t := e.(type) {
			case *sdl.QuitEvent:
				ticker.Stop()
				keepRunning = false
			case *sdl.MouseButtonEvent:
				if t.State == sdl.RELEASED {
					crosshairs.pos[0] = float32(t.X)
					crosshairs.pos[1] = float32(t.Y)
				}
			}


		}

		now := sdl.GetTicks()
		timePassed := now - ticks
		delta := float32(timePassed) * .0001 * float32(speed)
		ticks = now

		currentCenter := ship.pos
		desired := vec2.Sub(&crosshairs.pos, &currentCenter)
		desired.Normalize()
		desired.Scale(ship.maxSpeed)
		steerForce := vec2.Sub(&desired, &ship.vel)
		steerForce.Scale(delta)
		limitVec(&steerForce, ship.maxSteer)
		ship.acc.Add(&steerForce)
		//gr := gravity.Scaled(delta)
		//wd := wind.Scaled(delta)
		//ship.acc.Add(&gr)
		//ship.acc.Add(&wd)
		ship.vel.Add(&ship.acc)
		ship.pos.Add(&ship.vel)
		ship.acc.Mul(&vec2.Zero)

		ship.bound(width, height)

		ship.updateBox()

		// clear
		renderer.SetDrawColor(0, 0, 0, 0xFF)
		renderer.Clear()

		// crosshairs
		crosshairs.render(renderer)

		// ship
		angle := 90 + ship.vel.Angle() * 57.29
		_ = renderer.CopyEx(shipTexture, nil, &ship.box, float64(angle), nil, sdl.FLIP_NONE)


		renderer.Present()
		<-ticker.C
		//fmt.Println("ticker ticked")
	}

}

func initSDL() (*sdl.Window, *sdl.Renderer) {
	// Initialize
	err := sdl.Init(sdl.INIT_VIDEO)
	if err != nil {
		panic(err)
	}

	// Setup Window
	window, err := sdl.CreateWindow("Here's a Screen", sdl.WINDOWPOS_UNDEFINED, sdl.WINDOWPOS_UNDEFINED,
		width, height, sdl.WINDOW_SHOWN)
	if err != nil {
		panic(err)
	}

	// Renderer
	renderer, err := sdl.CreateRenderer(window, -1, sdl.RENDERER_ACCELERATED|sdl.RENDERER_PRESENTVSYNC)
	if renderer == nil {
		panic(err)
	}

	return window, renderer
}
