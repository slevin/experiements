import std.stdio;
import std.string;
import std.math;
import core.thread;
import std.random;

import derelict.sfml2.window;
import derelict.sfml2.graphics;
import derelict.sfml2.system;

import gl3n.linalg;

void main()
{
    int width = 800;
    int height = 600;
    vec2 area = vec2(width, height);
    SFMLEnv env = SFMLEnv(width, height);

    sfClock *clock = sfClock_create();

    //    auto ship = Ship("plane.png", env.ren);

    //auto crosshairs = Crosshairs();

    sfEvent event;

 mainLoop:
    while (sfRenderWindow_isOpen(env.win)) {
        while (sfRenderWindow_pollEvent(env.win, &event)) {
            if (event.type == sfEvtClosed) {
                sfRenderWindow_close(env.win);
                break mainLoop;
            }
            /*
            else if (event.type == SDL_MOUSEBUTTONUP) {
                auto mb = event.button;
                if (mb.state == SDL_RELEASED) {
                    crosshairs.reposition(mb.x, mb.y);
                }
                if (mb.button == SDL_BUTTON_RIGHT) {
                    crosshairs.followType = FollowType.Flee;
                } else {
                    crosshairs.followType = FollowType.Follow;
                }
            }
            */

        }

        // 40 is speed
        float delta = sfClock_restart(clock).sfTime_asSeconds * 40;

        //writeln(delta);

        // calculate forces
        /*
        crosshairs.stayWithinWalls(delta, area);

        if (crosshairs.followType == FollowType.Flee) {
            ship.steerAwayFrom(&crosshairs.pos, delta);
        } else {
            ship.steerToPosition(&crosshairs.pos, delta);
        }

        // update positions
        crosshairs.update(delta);
        ship.update(delta);
        */

        env.clear();

        sfRenderWindow_display(env.win);
        /*
        // render
        crosshairs.render(env.ren);
        ship.render(env.ren);
        SDL_RenderPresent(env.ren);
        */
    }
}

struct SFMLEnv {
    sfRenderWindow *win;
    sfVideoMode mode;
    //SDL_Renderer *ren;

    this(int width, int height) {
        DerelictSFML2System.load("/usr/local/Cellar/csfml/2.2/lib/libcsfml-system.2.2.dylib");
        DerelictSFML2Window.load("/usr/local/Cellar/csfml/2.2/lib/libcsfml-window.2.2.dylib");
        DerelictSFML2Graphics.load("/usr/local/Cellar/csfml/2.2/lib/libcsfml-graphics.2.2.dylib");

        mode = sfVideoMode(width, height, 32);
        win = sfRenderWindow_create(mode, "A Window", sfResize | sfClose, null);
        //        this.win = SDL_CreateWindow("A Window", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, width, height, SDL_WINDOW_SHOWN);
        //        this.ren = SDL_CreateRenderer(win, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
        // SDL_SetHint( SDL_HINT_RENDER_SCALE_QUALITY, "1" );

        }

    void clear() {
        sfRenderWindow_clear(win, sfBlack);
        //SDL_SetRenderDrawColor(this.ren, 0, 0, 0, 0xFF);
        //SDL_RenderClear(this.ren);
    }

    ~this() {
        sfRenderWindow_destroy(win);
        //        SDL_DestroyRenderer(this.ren);
        //        SDL_DestroyWindow(this.win);
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

/*
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

    void steerAwayFrom(vec2 *fleeing, float delta) {
        auto direction = pos - *fleeing + pos;
        direction.normalize();
        direction *= maxSpeed;

        auto steerForce = direction - vel;
        steerForce.limit(maxSteer);
        steerForce *= delta;
        acc += steerForce;
    }

    void update(float delta) {
        this.vel = this.vel + this.acc;
        this.vel.limit(this.maxSpeed);
        this.pos += vel * delta;
        this.acc *= 0;
    }

    void render(SDL_Renderer *ren) {
        this.box.x = cast(int)round(this.pos.x - this.box.w * 0.5);
        this.box.y = cast(int)round(this.pos.y - this.box.h * 0.5);
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

enum FollowType {
    Follow,
    Flee
}

struct Crosshairs {
    vec2 pos = vec2(0, 0);
    vec2 vel = vec2(0, 0);
    vec2 acc = vec2(0, 0);

    int sz = 20;
    FollowType followType = FollowType.Follow;
    float maxSpeed = 7;

    void update(float delta) {
        vel += acc;
        vel.limit(maxSpeed);
        pos += vel * delta;
        acc *= 0;
    }

    void render(SDL_Renderer *ren) {
        if (followType == FollowType.Follow) {
            SDL_SetRenderDrawColor(ren, 0x00, 0xFF, 0x00, 0xFF);
        } else {
            SDL_SetRenderDrawColor(ren, 0xFF, 0x00, 0x00, 0xFF);
        }
        SDL_RenderDrawLine(ren,
                           cast(int)round(this.pos.x - this.sz),
                           cast(int)round(this.pos.y),
                           cast(int)round(this.pos.x + this.sz),
                           cast(int)round(this.pos.y));
        SDL_RenderDrawLine(ren,
                           cast(int)round(this.pos.x),
                           cast(int)round(this.pos.y - this.sz),
                           cast(int)round(this.pos.x),
                           cast(int)round(this.pos.y + this.sz));
    }

    void stayWithinWalls(float delta, vec2 area) {
        auto dir = stayInsideDirection(pos, area, 20);
        dir.normalize();
        dir *= maxSpeed;
        acc += dir * delta;
    }

    void reposition(int x, int y) {
        pos.x = x;
        pos.y = y;
        randomizeVelocity();
    }

    void randomizeVelocity() {
        auto x = uniform(-1.0f, 1.0f);
        auto y = uniform(-1.0f, 1.0f);
        vel = vec2(x, y);
        vel.normalize();
        vel *= maxSpeed;
    }
}

vec2 stayInsideDirection(ref vec2 current, ref vec2 area, float distance) {
    float xDir = 0;
    float yDir = 0;
    if (current.x < distance) {
        xDir = distance - current.x;
    } else if (current.x > area.x - distance) {
        xDir = area.x - distance - current.x;
    }
    if (current.y < distance) {
        yDir = distance - current.y;
    } else if (current.y > area.y - distance) {
        yDir = area.y - distance - current.y;
    }
    auto stayForce = vec2(xDir, yDir);
    return stayForce;
}

unittest {
    auto distance = 10;
    vec2 area = vec2(100, 100);
    vec2 stayDir, current;

    // outside range
    current = vec2(20, 20);
    stayDir = stayInsideDirection(current, area, distance);
    assert(stayDir == vec2(0, 0));

    // should head directly right
    current = vec2(8, 30);
    stayDir = stayInsideDirection(current, area, distance);
    assert(stayDir == vec2(2, 0));

    // should head directly left
    current = vec2(92, 30);
    stayDir = stayInsideDirection(current, area, distance);
    assert(stayDir == vec2(-2, 0));

    // up and right
    current = vec2(8, 92);
    stayDir = stayInsideDirection(current, area, distance);
    assert(stayDir == vec2(2, -2));
}
*/
