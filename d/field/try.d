#! /usr/local/bin/rdmd

import std.algorithm;
    /*


struct with a method on it and a parameter

then an array of those structs call with parameter



     */

struct TryIt {
    int a;

    int addTo(int p) {
        return a + p;
    }
}

void main() {
    TryIt[5] t;

    auto param = 5;

    int[5] res = map!(delegate int(TryIt p) { return p.addTo(param); })(t);
    //auto res = map!(a => a.addTo(param))(t);

}
