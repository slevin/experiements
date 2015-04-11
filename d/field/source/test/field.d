module test.field;

import unit_threaded;
import field;
import gl3n.linalg;
import derelict.sfml2.graphics;
import derelict.sfml2.system;


class UnitAngleTest : TestCase {
    override void test() {
        vec2 v;
        v = unitAngleToVec(0);
        checkEqual(v, vec2(1, 0));

        v = unitAngleToVec(0.25);
        checkEqual(v, vec2(0, 1));

        v = unitAngleToVec(0.5);
        checkEqual(v, vec2(-1, 0));

        v = unitAngleToVec(0.75);
        checkEqual(v, vec2(0, -1));

        v = unitAngleToVec(1.0);
        checkEqual(v, vec2(1, 0));
    }
}

void testField() {

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
