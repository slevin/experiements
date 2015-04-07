module field;

static import noise;

import std.math;
import std.stdio;
import std.format;

import derelict.sfml2.graphics;
import derelict.sfml2.system;

import gl3n.linalg;

struct Field(size_t cols, size_t rows, float width, float height) {
    enum float boxWidth = width / cols;
    enum float boxHeight = height / rows;
    enum float lineLength = boxWidth * 0.7;
    vec2 slopes[cols][rows];
    sfVertex[cols * rows * 2] lineVertices; // 2 points per line
    float zFill = 0.0f;

    vec2 flowVectorForPosition(vec2 pos) {
        size_t x = cast(size_t)floor(pos.x / boxWidth);
        size_t y = cast(size_t)floor(pos.y / boxHeight);
        if (x < 0 || x >= cols || y < 0 || y >= rows) {
            return vec2(0, 0);
        } else {
            return slopes[x][y];
        }
    }

    uint numberOfLines() {
        return cast(uint)(lineVertices.length);
    }

    void fill(float function(ulong, ulong, float) fillFunction) {
        foreach(y; 0 .. rows) {
            foreach(x; 0 .. cols) {
                vec2 slope = unitAngleToVec(fillFunction(x, y, zFill));
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

    void fillWithNoise() {
        fill(function float(ulong x, ulong y, float z) {
                return (cast(float)noise.noise(x * 0.1, y * 0.1, z) + 1) / 2.0f;
            });
    }

    vec2 boxCenter(ulong x, ulong y) {
        return vec2(x * boxWidth + (boxWidth / 2.0f), y * boxHeight + (boxHeight / 2.0f));
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
    Field!(10, 10, 200, 200) field;
    vec2 pos;
    vec2 dir;

    // fill gets called with delegate method
    field.fill(function float(ulong x, ulong y, float zFill) {
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

    // outside range gives 0 vector
    pos = vec2(-100, -100);
    dir = field.flowVectorForPosition(pos);
    assert(dir == vec2(0, 0));

    pos = vec2(100000, 100000);
    dir = field.flowVectorForPosition(pos);
    assert(dir == vec2(0, 0));

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

}


vec2 unitAngleToVec(float angle)
    in { assert(angle >= 0 && angle <= 1.0, format("Angle must be between 0 and 1. You gave: %f", angle)); }
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
