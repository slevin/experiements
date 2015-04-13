module test.path;

import unit_threaded;
import gl3n.linalg;
import derelict.sfml2.graphics;
import derelict.sfml2.system;
import path;

class PathTest : CompositeTestCase {
void testEquals() {
    auto a = 1;
    auto b = 2;
    a.shouldNotEqual(b);
}
