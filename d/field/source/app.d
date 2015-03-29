static import noise;

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

    auto ship = Ship("plane.png");

    auto crosshairs = Crosshairs();

    sfEvent event;

 mainLoop:
    while (sfRenderWindow_isOpen(env.win)) {
        while (sfRenderWindow_pollEvent(env.win, &event)) {
            if (event.type == sfEvtClosed) {
                sfRenderWindow_close(env.win);
                break mainLoop;
            } else if (event.type == sfEvtMouseButtonReleased) {
                auto mb = event.mouseButton;
                crosshairs.reposition(mb.x, mb.y);
                if (mb.button == sfMouseRight) {
                    crosshairs.followType = FollowType.Flee;
                } else {
                    crosshairs.followType = FollowType.Follow;
                }
            }

        }

        // 40 is speed
        //float delta = sfClock_restart(clock).sfTime_asSeconds * 40;
        float millis = sfClock_restart(clock).sfTime_asMilliseconds;
        //writeln(millis);
        float delta = millis * .001 * 40;

        // calculate forces

        crosshairs.stayWithinWalls(delta, area);

        if (crosshairs.followType == FollowType.Flee) {
            ship.steerAwayFrom(&crosshairs.pos, delta);
        } else {
            ship.steerToPosition(&crosshairs.pos, delta);
        }

        // update positions
        crosshairs.update(delta);
        ship.update(delta);


        env.clear();
        crosshairs.render(env.win);
        ship.render(env.win);

        sfRenderWindow_display(env.win);
    }
}

struct SFMLEnv {
    sfRenderWindow *win;
    sfVideoMode mode;

    this(int width, int height) {
        DerelictSFML2System.load("/usr/local/Cellar/csfml/2.2/lib/libcsfml-system.2.2.dylib");
        DerelictSFML2Window.load("/usr/local/Cellar/csfml/2.2/lib/libcsfml-window.2.2.dylib");
        DerelictSFML2Graphics.load("/usr/local/Cellar/csfml/2.2/lib/libcsfml-graphics.2.2.dylib");

        mode = sfVideoMode(width, height, 32);
        win = sfRenderWindow_create(mode, "A Window", sfResize | sfClose, null);
        sfRenderWindow_setVerticalSyncEnabled(win, true);
        }

    void clear() {
        sfRenderWindow_clear(win, sfBlack);
    }

    ~this() {
        sfRenderWindow_destroy(win);
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
    sfTexture *tex;
    sfSprite *sprite;
    float maxSpeed = 5;
    float maxSteer = 0.2;
    float slowRange = 100;

    this(string path) {
        tex = sfTexture_createFromFile(toStringz(path), null);
        sfTexture_setSmooth(tex, true);
        sprite = sfSprite_create();
        sfSprite_setTexture(sprite, tex, true);
        sfFloatRect rect = sfSprite_getLocalBounds(sprite);
        sfSprite_setOrigin(sprite, sfVector2f(rect.width / 2.0, rect.height /2.0));
    }

    ~this() {
        sfSprite_destroy(sprite);
        sfTexture_destroy(tex);
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

    void render(sfRenderWindow *win) {
        double angle = 90 + this.vel.angle() * 57.29;
        sfSprite_setRotation(sprite, angle);
        sfSprite_setPosition(sprite, sfVector2f(pos.x, pos.y));

        sfRenderWindow_drawSprite(win, sprite, null);
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


    void render(sfRenderWindow *win) {
        sfColor color;
        if (followType == FollowType.Follow) {
            color = sfColor(0x00, 0xFF, 0x00, 0xFF);
        } else {
            color = sfColor(0xFF, 0x00, 0x00, 0xFF);
        }

        sfVertex[] line1 = [sfVertex(sfVector2f(pos.x - sz, pos.y), color),
                            sfVertex(sfVector2f(pos.x + sz, pos.y), color)];
        sfVertex[] line2 = [sfVertex(sfVector2f(pos.x, pos.y - sz), color),
                            sfVertex(sfVector2f(pos.x, pos.y + sz), color)];

        sfRenderWindow_drawPrimitives(win, line1.ptr, 2, sfLines, null);
        sfRenderWindow_drawPrimitives(win, line2.ptr, 2, sfLines, null);
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

struct Field {
    double slopes[][];
    int cols;
    int rows;

    void fill(int cols, int rows) {
        // noise * 360 to radians sin and cos of that angle to
        // get my y and x

        // can have an array of prebuilt lines as well that
        // are positioned correctl based on slopes
    }

    //void lookup
    /*
      field get slope
       returns unit vector at position x and y
      given an x, y look up in noise field
      x / cols, y / rows

      ship is at position
      normalized vectors that I can position in the middle

      how to draw if I have normalized vectors I can position them as
      an offset into the field (just a bunch of lines) that I rerender
      each time

      so assuming noiseVal
     */

}

unittest {
    // I have pos, I want a vector to multiply by
    auto v = vec2(10.5, 13.1);
    auto f = flowVectorForPosition(v);
    assert(f == vec2(0, -1));
    /*
    uint col, row;
    //posToRowCol(x, y, ref col, ref row);

    int slopes[][];
    slopes[0][0] = vec2()
    slopeAt(10.5, 13.1, slopes);
    */
}
