package main

import "fmt"
import "time"
import "runtime"
import "github.com/veandco/go-sdl2/sdl"
import "github.com/veandco/go-sdl2/sdl_image"
import "github.com/ungerik/go3d/vec2"

const width = 800
const height = 600

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
	box sdl.Rect  // for drawing
}

func main() {
	fmt.Println("Started...")
	runtime.LockOSThread()

	window, renderer := initSDL()
	defer window.Destroy()
	defer renderer.Destroy()


	// example draw green square
//	renderer.SetDrawColor(0x00, 0xFF, 0x00, 0xFF)
//	renderer.DrawRect(&sdl.Rect{0,0,100,100})

	shipTexture, _ := img.LoadTexture(renderer, "plane.png")
	defer shipTexture.Destroy()


	// correct size
	_, _, shipW, shipH, _ := shipTexture.Query()
	sdlBox := rect32(0, 0, shipW, shipH)

	// center it
	x1 := (width - shipW) / 2
	y1 := (height - shipH) / 2
	pos := vec2.T{float32(x1), float32(y1)}

	ship := Vehicle{pos, vec2.Zero, vec2.Zero, sdlBox}


	// clear with black
	renderer.SetDrawColor(0, 0, 0, 0xFF)


	gravity := vec2.T{0, 10}
	wind := vec2.T{1, 0}
	speed := 5

	ticker := time.NewTicker(time.Millisecond * 20)
	keepRunning := true
	ticks := sdl.GetTicks()

	for keepRunning {
		for {
			e := sdl.PollEvent()
			if e == nil {
				break
			}
			switch e.(type) {
			case *sdl.QuitEvent:
				ticker.Stop()
				keepRunning = false
			}

		}

		now := sdl.GetTicks()
		delta := float32(now - ticks) * .0001 * float32(speed)
		ticks = now

		gr := gravity.Scaled(delta)
		wd := wind.Scaled(delta)
		ship.acc.Add(&gr)
		ship.acc.Add(&wd)
		ship.vel.Add(&ship.acc)
		ship.pos.Add(&ship.vel)
		ship.acc.Mul(&vec2.Zero)

		ship.box.X = int32(ship.pos[0])
		ship.box.Y = int32(ship.pos[1])

		renderer.Clear()
		_ = renderer.CopyEx(shipTexture, nil, &ship.box, 35, nil, sdl.FLIP_NONE)
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
