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

int addSome(TryIt t, int num) {
    return t.a + num;
}

void main() {
    TryIt[5] t;
    int[5] vals;

    auto param = 5;

    //int[5] res = map!(delegate int(TryIt p) { return p.addTo(param); })(t);
    //auto res = map!(a => a.addTo(param))(t);
    //auto res = map!(a => a.addTo(5))(t);
    //auto res = map!(a => addSome(a, 5))(t);
    int[] arr1 = [1,2,3,4,5];
    int[5] arr2 = [1,2,3,4,5];
    //auto res = map!((a => a + 5))(vals);
    auto squares = map!(a => a * a)(arr2[]);

}
