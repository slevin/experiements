package main

import "github.com/veandco/go-sdl2/sdl"

func main() {

	err := sdl.Init(sdl.INIT_VIDEO)
	if err != nil {
		panic(err)
	}

	window, err := sdl.CreateWindow("Here's a Screen", sdl.WINDOWPOS_UNDEFINED, sdl.WINDOWPOS_UNDEFINED,
		800, 600, sdl.WINDOW_SHOWN)
	if err != nil {
		panic(err)
	}
	defer window.Destroy()

	renderer, err := sdl.CreateRenderer(window, -1, sdl.RENDERER_ACCELERATED|sdl.RENDERER_PRESENTVSYNC)
	if renderer == nil {
		panic(err)
	}
	defer renderer.Destroy()

	bmp := sdl.LoadBMP("duck.bmp")
	if bmp == nil {
		panic("can't load duck bitmap")
	}

	tex, err := renderer.CreateTextureFromSurface(bmp)
	bmp.Free()
	if tex == nil {
		panic("cant create texture")
	}
	defer tex.Destroy()

	err = renderer.Clear()
	if err != nil {
		panic(err)
	}

	err = renderer.Copy(tex, nil, nil)
	if err != nil {
		panic(err)
	}

	renderer.Present()

	sdl.Delay(2000)
}
