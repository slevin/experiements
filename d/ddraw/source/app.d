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

    SDL_Rect sBox = SDL_Rect(0, 0, shipTexture.width, shipTexture.height);

    int cX = 0;
    int cY = 0;

    while (keepRunning) {
        while (SDL_PollEvent(&event)) {
            if (event.type == SDL_QUIT) {
                keepRunning = false;
                break;
            } else if (event.type == SDL_MOUSEBUTTONUP) {
                auto mb = event.button;
                if (mb.state == SDL_RELEASED) {
                    // get position
                    cX = mb.x;
                    cY = mb.y;
                }
            }
        }
        if (!keepRunning) { break; }

        auto delta = timer.update();

        env.clear();

        // draw crosshairs
        SDL_SetRenderDrawColor(env.ren, 0x00, 0xFF, 0x00, 0xFF);
        int cSize = 20;
        SDL_RenderDrawLine(env.ren,
                           cX - cSize, cY,
                           cX + cSize, cY);
        SDL_RenderDrawLine(env.ren,
                           cX, cY - cSize,
                           cX, cY + cSize);


        // draw ship
        SDL_RenderCopyEx(env.ren, shipTexture.tex, null, &sBox, 0, null, SDL_FLIP_NONE);

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
        SDL_RenderClear(this.ren);
    }

    ~this() {
        SDL_DestroyRenderer(this.ren);
        SDL_DestroyWindow(this.win);
    }
}


struct Texture {
    SDL_Texture *tex;
    int width;
    int height;

    this(string path, SDL_Renderer *ren) {
        this.tex = IMG_LoadTexture(ren, toStringz(path));
        Uint32 format;
        int access;
        SDL_QueryTexture(tex, &format, &access, &this.width, &this.height);
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
