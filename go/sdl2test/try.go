package main

import "fmt"
import "github.com/veandco/go-sdl2/sdl"

func main() {

	var width int32 = 800
	var height int32 = 600

	// Initialize
	err := sdl.Init(sdl.INIT_VIDEO)
	if err != nil {
		panic(err)
	}


	// Setup Window
	window, err := sdl.CreateWindow("Here's a Screen", sdl.WINDOWPOS_UNDEFINED, sdl.WINDOWPOS_UNDEFINED,
		int(width), int(height), sdl.WINDOW_SHOWN)
	if err != nil {
		panic(err)
	}
	defer window.Destroy()

	// Renderer
	renderer, err := sdl.CreateRenderer(window, -1, sdl.RENDERER_ACCELERATED|sdl.RENDERER_PRESENTVSYNC)
	if renderer == nil {
		panic(err)
	}
	defer renderer.Destroy()

	duckTexture := loadTexture("duck.bmp", renderer)
	defer duckTexture.Destroy()

	marblesTexture := loadTexture("marbles.bmp", renderer)
	defer marblesTexture.Destroy()

	// erase the screen
	if err = renderer.Clear(); err != nil {
		panic(err)
	}

	var count int32 = 2
	partW := width  / count
	partH := height / count
	for y := int32(0); y < count; y = y + 1 {
		for x := int32(0); x < count; x = x + 1 {
			rect := sdl.Rect{partW * x, partH * y, partW, partH}
			if err = renderer.Copy(marblesTexture, nil, &rect); err != nil {
				panic(err)
			}
		}
	}

	var outW, outH int32
	_, _, outW64, outH64, _ := duckTexture.Query()
	outW, outH = int32(outW64), int32(outH64)

	var two int32 = 2
	halfW := width / two
	halfH := height / two
	newH := outH * halfW / outW
	duckRect := sdl.Rect{halfW - halfW / two, halfH - newH / two, halfW, newH}
	_ = renderer.Copy(duckTexture, nil, &duckRect)

	renderer.Present()

	sdl.Delay(2000)
}

func loadTexture(path string, renderer *sdl.Renderer) *sdl.Texture {
	surface := sdl.LoadBMP(path)
	if surface == nil {
		panic(fmt.Sprintf("can't load bitmap: %s", path))
	}

	texture, err := renderer.CreateTextureFromSurface(surface)
	surface.Free()
	if texture == nil {
		panic(fmt.Sprintf("cant create texture from %s, %v", path, err))
	}
	return texture
}
