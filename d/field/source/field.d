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
    enum float lineLength = 14;
    vec2 slopes[cols][rows];
    sfVertex[cols * rows * 2] lineVertices; // 2 points per line

    vec2 flowVectorForPosition(vec2 pos) {
        size_t x = cast(size_t)floor(pos.x / width);
        size_t y = cast(size_t)floor(pos.y / height);
        return slopes[x][y];
    }

    uint numberOfLines() {
        return cast(uint)(lineVertices.length);
    }

    void fill(float function(ulong, ulong) fillFunction) {
        foreach(y; 0 .. rows) {
            foreach(x; 0 .. cols) {
                vec2 slope = unitAngleToVec(fillFunction(x, y));
                slopes[x][y] = slope;
                // center around 0
                vec2 v1 = vec2(slope.x * lineLength / 2.0f,
                               slope.y * lineLength / 2.0f);
                vec2 v2 = v1 * -1;
                // move to center of matching square
                vec2 offset = boxCenter(x, y);
                v1 += offset;
                v2 += offset;
                sfVertex vtx1 = sfVertex(sfVector2f(v1.x, v1.y), sfRed);
                sfVertex vtx2 = sfVertex(sfVector2f(v2.x, v2.y), sfRed);
                lineVertices[(x + y * rows) * 2] = vtx1;
                lineVertices[(x + y * rows) * 2 + 1] = vtx2;
            }
        }
    }

    vec2 boxCenter(ulong x, ulong y) {
        return vec2(x * width + (width / 2.0f), y * height + (height / 2.0f));
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

    // fill gets called with delegate method
    field.fill(function float(ulong x, ulong y) {
            if (x == 0 && y == 0) {
                return 0.0;
            } else if (x == 1 && y == 0) {
                return 0.25;
            } else if (x == 0 && y == 1) {
                return 0.5;
            } else {
                return 0.3;
            }
        });

    // slopes fill up normals for angle
    assert(field.slopes[0][0] == unitAngleToVec(0.0));
    assert(field.slopes[1][0] == unitAngleToVec(0.25));

    // find slope by position
    pos = vec2(10.5, 13.1);
    dir = field.flowVectorForPosition(pos);
    assert(dir == field.slopes[0][0]);

    // because x >= 20 which is width of box
    pos = vec2(20, 10);
    dir = field.flowVectorForPosition(pos);
    assert(dir == field.slopes[1][0]);

    // because y >= 20 which is height of box
    pos = vec2(5, 21);
    dir = field.flowVectorForPosition(pos);
    assert(dir == field.slopes[0][1]);


    // drawable lines

    assert(field.boxCenter(0, 0) == vec2(10, 10));
    assert(field.boxCenter(1, 0) == vec2(30, 10));
    assert(field.boxCenter(1, 1) == vec2(30, 30));

    // should have rows * cols * 2 (2 pts per line)
    assert(200 == field.numberOfLines());

    sfVector2f v1;
    sfVector2f v2;

    // first line
    v1 = field.lineVertices[0].position;
    v2 = field.lineVertices[1].position;
    assert(v1.x == 17 && v1.y == 10);
    assert(v2.x ==  3 && v2.y == 10);


    /*
should 1 == 0 seems like there will be a harsh line at that point
maybe should go from 0 fully rotated to 0 again something to think about

could go out of range with some of these queries
need to catch that? or with position queries
    */

}


vec2 unitAngleToVec(float angle)
    in { assert(angle >= 0 && angle <= 1.0); }
    body {
        // turns unit angle [0, 1)
        // into a unit vector

        auto theta = 2 * PI * angle;
        auto x = round(cos(theta) * 100) / 100;
        auto y = round(sin(theta) * 100) / 100;
        return vec2(x, y);
    }

unittest {
    vec2 v;
    v = unitAngleToVec(0);
    assert(v == vec2(1, 0));

    v = unitAngleToVec(0.25);
    assert(v == vec2(0, 1));

    v = unitAngleToVec(0.5);
    assert(v == vec2(-1, 0));

    v = unitAngleToVec(0.75);
    assert(v == vec2(0, -1));

    v = unitAngleToVec(1.0);
    assert(v == vec2(1, 0));
}
