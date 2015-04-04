module field;

static import noise;

import std.math;
import std.stdio;

import derelict.sfml2.graphics;
import derelict.sfml2.system;

import gl3n.linalg;

struct Field {
    enum size_t cols = 10;
    enum size_t rows = 10;
    enum int width = 20;
    enum int height = 20;
    vec2 slopes[cols][rows];
    sfVertex[cols * rows] lineVertices;

    vec2 flowVectorForPosition(vec2 pos) {
        size_t x = cast(size_t)floor(pos.x / width);
        size_t y = cast(size_t)floor(pos.y / height);
        return slopes[x][y];
    }

    uint numberOfLines() {
        return cast(uint)(lineVertices.length);
    }

    void fill(float function(int, int) fillFunction) {
        // takes the slopes and creates a line that matches the slope
        // with the proper offset
        foreach(y; 0 .. rows) {
            foreach(x; 0 .. cols) {
                lineVertices[x + y * rows] = sfVertex(sfVector2f(0,0), sfRed);
            }
        }
    }

    void render(sfRenderWindow *win) {
        sfRenderWindow_drawPrimitives(win,
                                      lineVertices.ptr,
                                      numberOfLines(),
                                      sfLines,
                                      null
                                      );
    }
}

unittest {
    Field field;
    vec2 pos;
    vec2 dir;

    field.slopes[0][0] = vec2(0,-1);
    pos = vec2(10.5, 13.1);
    dir= field.flowVectorForPosition(pos);
    assert(dir == field.slopes[0][0]);

    // because x >= 20 which is width of box
    field.slopes[1][0] = vec2(3, 4);
    pos = vec2(20, 10);
    dir = field.flowVectorForPosition(pos);
    assert(dir == field.slopes[1][0]);

    // because y >= 20 which is height of box
    field.slopes[0][1] = vec2(5, 6);
    pos = vec2(5, 21);
    dir = field.flowVectorForPosition(pos);
    assert(dir == field.slopes[0][1]);

    // fill gets called with delegate method

    // should have rows * cols lines
    field.fill(function float(int x, int y) {
            return 0.0;
        });

    assert(100 == field.numberOfLines());

    // line

    /*
        sfVertex[] line1 = [sfVertex(sfVector2f(pos.x - sz, pos.y), color),
                            sfVertex(sfVector2f(pos.x + sz, pos.y), color)];
        sfVertex[] line2 = [sfVertex(sfVector2f(pos.x, pos.y - sz), color),
                            sfVertex(sfVector2f(pos.x, pos.y + sz), color)];

        sfRenderWindow_drawPrimitives(win, line1.ptr, 2, sfLines, null);
        sfRenderWindow_drawPrimitives(win, line2.ptr, 2, sfLines, null);
    */

}

vec2 unitAngleToVec(float angle)
    in { assert(angle >= 0 && angle <= 1.0); }
    body {
        // turns unit angle [0, 1)
        // into a unit vector

        auto theta = 2 * PI * angle;
        auto x = round(sin(theta) * 100) / 100;
        auto y = round(cos(theta) * 100) / 100;
        return vec2(x, y);
    }


unittest {
    vec2 v;
    v = unitAngleToVec(0);
    assert(v == vec2(0, 1));

    v = unitAngleToVec(0.25);
    assert(v == vec2(1, 0));

    v = unitAngleToVec(0.5);
    assert(v == vec2(0, -1));

    v = unitAngleToVec(0.75);
    assert(v == vec2(-1, 0));

    v = unitAngleToVec(1.0);
    assert(v == vec2(0, 1));
}
