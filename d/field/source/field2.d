module field2;

static import noise;

import std.math;
import std.stdio;
import std.format;

import derelict.sfml2.graphics;
import derelict.sfml2.system;

import gl3n.linalg;

struct Field2(size_t inCols, size_t inRows, float inWidth, float inHeight) {
    enum size_t cols = inCols;
    enum size_t rows = inRows;
    enum float width = inWidth;
    enum float height = inHeight;
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
