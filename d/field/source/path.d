module path;

import std.math;
import std.stdio;
import std.format;

import derelict.sfml2.graphics;
import derelict.sfml2.system;

import gl3n.linalg;

struct Path(size_t points) {

    sfVertex[points * 2] lineVertices; // 2 points per line

    uint numberOfLines() {
        return points;
    }

    void fill(float function(int index) fillFunction) {

    }

    void render(sfRenderWindow *win) {
        sfRenderWindow_drawPrimitives(win,
                                      lineVertices.ptr,
                                      numberOfLines(),
                                      sfLines,
                                      null);
    }

}

unittest {

    Path!3 path;

    path.fill((int index) {
            if (index == 0) {
                return vec2(0, 0);
            } else if (index == 1) {
                return vec2(100, 100);
            } else if (index == 2) {
                return vec2(200, 50);
            }
        });


    assert(path.numberOfLines() == 3);
    assert(path.lineVertices.length == 6);



}
