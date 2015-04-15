module path;

import std.math;
import std.stdio;
import std.format;

import derelict.sfml2.graphics;
import derelict.sfml2.system;

import gl3n.linalg;

struct Path(size_t inPoints) {
    enum size_t points = inPoints;
    sfVertex[points * 2] lineVertices; // 2 points per line

    uint numberOfLines() {
        return points;
    }

    void fill(vec2 function(ulong index) fillFunction) {
        foreach(pt; 0 .. points) {
            auto nextPoint = pt + 1;
            if (pt == points - 1) {
                // last point wraps
                nextPoint = 0;
            }
            vec2 v1 = fillFunction(pt);
            lineVertices[pt * 2] = sfVertex(sfVector2f(v1.x, v1.y), sfBlue);
            vec2 v2 = fillFunction(nextPoint);
            lineVertices[pt * 2 + 1] = sfVertex(sfVector2f(v2.x, v2.y), sfBlue);
        }
    }

    void render(sfRenderWindow *win) {
        sfRenderWindow_drawPrimitives(win,
                                      lineVertices.ptr,
                                      numberOfLines(),
                                      sfLines,
                                      null);
    }

}
