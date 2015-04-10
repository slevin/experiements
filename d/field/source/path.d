module path;

import std.math;
import std.stdio;
import std.format;

import derelict.sfml2.graphics;
import derelict.sfml2.system;

import gl3n.linalg;

struct Path(size_t points) {


    uint numberOfLines() {
        return points;
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

    Path p(3);



}
