module test.path;

import unit_threaded;

void testEquals() {
    auto a = 1;
    auto b = 2;
    a.shouldEqual(b);
}
