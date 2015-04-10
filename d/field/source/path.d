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

    assert(path.numberOfLines() == 3);
    assert(path.lineVertices.length == 6);



}
