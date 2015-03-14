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

    auto keepRunning = true;
    SDL_Event event;

    auto timer = new Timer();

    auto ship = Ship("plane.png", env.ren);

    auto crosshairs = Crosshairs();

    while (keepRunning) {
        while (SDL_PollEvent(&event)) {
            if (event.type == SDL_QUIT) {
                keepRunning = false;
                break;
            } else if (event.type == SDL_MOUSEBUTTONUP) {
                auto mb = event.button;
                if (mb.state == SDL_RELEASED) {
                    crosshairs.update(mb.x, mb.y);
                }
            }
        }
        if (!keepRunning) { break; }

        auto delta = timer.update();

        env.clear();

        crosshairs.render(env.ren);

        ship.steerToPosition(&crosshairs.pos, delta);
        ship.update();

        ship.render(env.ren);

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


struct Vec2 {
    float x = 0, y = 0;

    void scale(float val) {
        this.x *= val;
        this.y *= val;
    }

    Vec2 opBinary(string op)(Vec2 rhs) if (op == "-") {
        auto result = Vec2(this.x - rhs.x, this.y - rhs.y);
        return result;
    }

    Vec2 opBinary(string op)(Vec2 rhs) if (op == "+") {
        auto result = Vec2(this.x + rhs.x, this.y + rhs.y);
        return result;
    }
}

struct Ship {
    Vec2 pos, vel, acc;
    SDL_Texture *tex;
    SDL_Rect box;
    float maxSpeed = 5;
    float maxSteer = 2;

    this(string path, SDL_Renderer *ren) {
        this.tex = IMG_LoadTexture(ren, toStringz(path));
        this.box = SDL_Rect();
        SDL_QueryTexture(tex, null, null, &this.box.w, &this.box.h);
    }

    ~this() {
        SDL_DestroyTexture(this.tex);
    }

    void steerToPosition(Vec2 *desired, float delta) {
        auto direction = *desired - this.pos;
        auto steerForce = direction - this.vel;
        steerForce.scale(delta);
        this.acc = this.acc + steerForce;
    }

    void update() {
        this.vel = this.vel + this.acc;
        this.pos = this.pos + this.vel;
        this.acc.scale(0);
    }

    void render(SDL_Renderer *ren) {
        this.box.x = cast(int)(this.pos.x - this.box.w * 0.5);
        this.box.y = cast(int)(this.pos.y - this.box.h * 0.5);

        SDL_RenderCopyEx(ren,
                         this.tex,
                         null,
                         &this.box,
                         0,
                         null,
                         SDL_FLIP_NONE);
    }
}


struct Crosshairs {
    Vec2 pos;
    int sz = 20;

    void update(int x, int y) {
        this.pos.x = x;
        this.pos.y = y;
    }

    void render(SDL_Renderer *ren) {
        SDL_SetRenderDrawColor(ren, 0x00, 0xFF, 0x00, 0xFF);
        SDL_RenderDrawLine(ren,
                           cast(int)(this.pos.x - this.sz),
                           cast(int)this.pos.y,
                           cast(int)(this.pos.x + this.sz),
                           cast(int)this.pos.y);
        SDL_RenderDrawLine(ren,
                           cast(int)this.pos.x,
                           cast(int)(this.pos.y - this.sz),
                           cast(int)this.pos.x,
                           cast(int)(this.pos.y + this.sz));
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
        auto delta = (now - this.ticks) * .001 * this.speed;
        this.ticks = now;
        return delta;
    }
}
