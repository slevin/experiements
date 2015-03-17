import std.stdio;
import std.string;
import std.math;
import core.thread;

import derelict.sdl2.sdl;
import derelict.sdl2.image;

import gl3n.linalg;

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
        ship.update(delta);

        ship.render(env.ren);

        SDL_RenderPresent(env.ren);


        //Thread.sleep(dur!("msecs")(16));
        //        writeln(delta);

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


float angle(vec2 vec) {
    return atan2(vec.y, vec.x);
}

void limit(ref vec2 vec, float max) {
    float l2 = vec.magnitude_squared();
    if (l2 > (max ^^ 2)) {
        vec *= (max / (l2 ^^ 0.5));
    }
}

struct Ship {
    vec2 pos = vec2(0,0), vel = vec2(0,0), acc = vec2(0, 0);
    SDL_Texture *tex;
    SDL_Rect box;
    float maxSpeed = 5;
    float maxSteer = 0.2;
    float slowRange = 100;

    this(string path, SDL_Renderer *ren) {
        this.tex = IMG_LoadTexture(ren, toStringz(path));
        this.box = SDL_Rect();
        SDL_QueryTexture(tex, null, null, &this.box.w, &this.box.h);
    }

    ~this() {
        SDL_DestroyTexture(this.tex);
    }

    void steerToPosition(vec2 *desired, float delta) {
        auto direction = *desired - this.pos;
        auto mag = direction.magnitude();
        direction.normalize();
        if (mag <= slowRange) {
            // scale from 0 - maxSpeed over distance to slowRange
            direction *= (mag * maxSpeed / slowRange);
        } else {
            direction *= maxSpeed;
        }
        auto steerForce = direction - this.vel;
        steerForce.limit(this.maxSteer);
        steerForce *= delta;
        this.acc = this.acc + steerForce;
    }

    void update(float delta) {
        this.vel = this.vel + this.acc;
        this.vel.limit(this.maxSpeed);
        this.pos = this.pos + this.vel * delta;
        this.acc *= 0;
    }

    void render(SDL_Renderer *ren) {
        this.box.x = cast(int)(this.pos.x - this.box.w * 0.5);
        this.box.y = cast(int)(this.pos.y - this.box.h * 0.5);
        double angle = 90 + this.vel.angle() * 57.29;

        SDL_RenderCopyEx(ren,
                         this.tex,
                         null,
                         &this.box,
                         angle,
                         null,
                         SDL_FLIP_NONE);
    }
}


struct Crosshairs {
    vec2 pos = vec2(0, 0);
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
    enum speed = 40;

    this() {
        this.ticks = SDL_GetTicks();
    }

    double update() {
        //        return .16;
        auto now = SDL_GetTicks();
        auto delta = (now - this.ticks) * .001 * this.speed;
        this.ticks = now;
        return delta;
    }
}
