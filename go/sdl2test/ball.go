package main

import "fmt"
import "time"
import "github.com/veandco/go-sdl2/sdl"
import "github.com/veandco/go-sdl2/sdl_image"

//import "github.com/ungerik/go3d"

const width = 800
const height = 600

func rect32(x int, y int, w int, h int) sdl.Rect {
	return sdl.Rect{int32(x), int32(y), int32(w), int32(h)}
}

func point32(x int, y int) sdl.Point {
	return sdl.Point{int32(x), int32(y)}
}

func main() {

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
	x1 := (width - shipW) / 2
	y1 := (height - shipH) / 2
	pos := rect32(x1, y1, shipW, shipH)



	// clear with black
	renderer.SetDrawColor(0, 0, 0, 0xFF)

	ticker := time.NewTicker(time.Millisecond * 30)
	keepRunning := true
	for keepRunning {
		for {
			e := sdl.PollEvent()
			if e == nil {
				break
			}
			switch e.(type) {
			case *sdl.QuitEvent:
				fmt.Println("wow it quit")
				keepRunning = false
			}

		}

		renderer.Clear()
		_ = renderer.CopyEx(shipTexture, nil, &pos, 35, nil, sdl.FLIP_NONE)
		renderer.Present()
		<-ticker.C
		pos.X = pos.X + 2
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
