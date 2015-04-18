module test.path;

import unit_threaded;
import gl3n.linalg;
import derelict.sfml2.graphics;
import derelict.sfml2.system;
import path;

void testPathLine() {

    float nearRadius = 10;
    PathLine pl = PathLine(vec2(0, 0), vec2(100, 0), nearRadius);

    // normal point on line
    checkEqual(pl.normalForPoint(vec2(50, 100)),
               vec2(50, 0));

    //checkEqual(pl.normalForPoint(vec2()))
    // distance from point to line
    // on line equal
    bool within;
    //within = pl.within(vec2(0, 0));
    //checkTrue(within);
}

void testPath() {


    Path!3 path;

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

    // given point is that within some radius of a line
    // a point within
    //path.pointClose(vec2())

    /*
      need to say given a position
whats the closest normal that is on each line
so need a way of getting each line
and given a line what is the normal
and is that normal on that line
and given a set of normals that are on lines
which one is the closest

and also need to say given where I will be (another point)
is that within a radius of a line


    */

}
