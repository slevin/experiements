module test.path;

import std.stdio;
import unit_threaded;
import gl3n.linalg;
import derelict.sfml2.graphics;
import derelict.sfml2.system;
import path;

void testPathLine() {

    float nearRadius = 10;
    PathLine pl = PathLine(vec2(0, 0), vec2(100, 0), 10, 25);

        // normal point on line
    checkEqual(pl.normalForPoint(vec2(50, 100)).normalPoint,
               vec2(50, 0));

    pl = PathLine(vec2(0, 0), vec2(100, 100), 10, 25);
    checkEqual(pl.normalForPoint(vec2(100, 0)).normalPoint,
               vec2(50, 50));

    // other side
    checkEqual(pl.normalForPoint(vec2(0, 100)).normalPoint,
               vec2(50, 50));
}

void testPath() {


    Path!(3, 10, 0) path;

    path.fill((ulong index) {
            if (index == 0) {
                return vec2(0, 0);
            } else if (index == 1) {
                return vec2(100, 100);
            } else if (index == 2) {
                return vec2(200, 50);
            } else {
                return vec2(-1, -1);
            }
        });


    // has enough lines
    checkEqual(path.numberOfLines(), 3);
    checkEqual(path.lineVertices.length, 6);

    // points are correct
    auto v1 = path.lineVertices[0].position;
    v1.x.shouldEqual(0);
    v1.y.shouldEqual(0);

    auto v2 = path.lineVertices[1].position;
    v2.x.shouldEqual(100);
    v2.y.shouldEqual(100);

    v1 = path.lineVertices[2].position;
    v1.x.shouldEqual(100);
    v1.y.shouldEqual(100);

    v2 = path.lineVertices[3].position;
    v2.x.shouldEqual(200);
    v2.y.shouldEqual(50);

    // last point is first point
    v1 = path.lineVertices[4].position;
    v1.x.shouldEqual(200);
    v1.y.shouldEqual(50);

    v2 = path.lineVertices[5].position;
    v2.x.shouldEqual(0);
    v2.y.shouldEqual(0);

    // point near line should be within range and not need steering
    checkFalse(path.steerCheckForPoint(vec2(1, 1)).needsSteering);

    // point outside path radius should require steering
    checkTrue(path.steerCheckForPoint(vec2(0, 50)).needsSteering);
    checkEqual(path.steerCheckForPoint(vec2(0, 50)).steerTarget,
               vec2(25, 25));

    // if within multiple should find closest
    //checkEqual(path.steerCheckForPoint(vec2(100, 75)).steerTarget,
    //               vec2(87.5, 87.5));
    // need an epsilon check here

    // if not perp to any line then we are screwed
    // but should have a test
}
