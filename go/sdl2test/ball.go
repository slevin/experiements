package main

//import "fmt"
import "github.com/veandco/go-sdl2/sdl"
import "github.com/veandco/go-sdl2/sdl_image"

//import "github.com/ungerik/go3d"

const width = 800
const height = 600

func main() {

	window, renderer := initSDL()
	defer window.Destroy()
	defer renderer.Destroy()

	// clear with black
	renderer.SetDrawColor(0, 0, 0, 0xFF)
	renderer.FillRect(nil)


	renderer.SetDrawColor(0x00, 0xFF, 0x00, 0xFF)
	renderer.DrawRect(&sdl.Rect{0,0,100,100})
	// draw green circle in the middle

/*


load plane and draw it in the middle



*/

	renderer.Present()

	sdl.Delay(5000)

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
