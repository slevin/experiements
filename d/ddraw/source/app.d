import std.stdio;
import std.string;
import core.thread;

import derelict.sdl2.sdl;
import derelict.sdl2.image;

void main()
{
    int width = 800;
    int height = 600;
    SDLEnv env = SDLEnv(width, height);

    auto shipTexture = Texture("plane.png", env.ren);

    auto keepRunning = true;
    SDL_Event event;

    auto timer = new Timer();

    while (keepRunning) {
        while (SDL_PollEvent(&event)) {
            if (event.type == SDL_QUIT) {
                keepRunning = false;
                break;
            }
        }
        if (!keepRunning) { break; }

        auto delta = timer.update();

        //env.clear();

        auto r = SDL_Rect(0,0,100,100);
        SDL_RenderCopyEx(env.ren, shipTexture.tex, null, &r, 0, null, SDL_FLIP_NONE);

        SDL_RenderPresent(env.ren);

        Thread.sleep(dur!("msecs")(16));

    }


}

struct SDLEnv {
    SDL_Window *win;
    SDL_Renderer *ren;

    this(int width, int height) {
        DerelictSDL2.load();
        DerelictSDL2Image.load();
        SDL_Init(SDL_INIT_VIDEO);
        this.win = SDL_CreateWindow("A Window", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, width, height, SDL_WINDOW_SHOWN);
        this.ren = SDL_CreateRenderer(win, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
    }

    void clear() {
        SDL_SetRenderDrawColor(this.ren, 0, 0, 0, 0xFF);
    }

    ~this() {
        SDL_DestroyRenderer(this.ren);
        SDL_DestroyWindow(this.win);
    }
}

struct Texture {
    SDL_Texture *tex;

    this(string path, SDL_Renderer *ren) {
        this.tex = IMG_LoadTexture(ren, toStringz(path));
    }

    ~this() {
        SDL_DestroyTexture(this.tex);
    }
}

class Timer {
    Uint32 ticks = 0;
    enum speed = 10;

    this() {
        this.ticks = SDL_GetTicks();
    }

    double update() {
        auto now = SDL_GetTicks();
        auto delta = (now - this.ticks) * .0001 * this.speed;
        this.ticks = now;
        return delta;
    }
}
