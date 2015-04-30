#!/usr/local/bin/rdmd -I/Users/slevin/.dub/packages/gl3n-1.0.1

import gl3n.linalg;

void main(string[] args) {
    enum size_t sz = 100000;
    vec2 slopes[sz];
    //Field!(sz) fld;
}

struct Field(size_t cols) {
    vec2 slopes[cols];
}
