#!/usr/local/bin/rdmd -unittest -I/Users/slevin/.dub/packages/unit-threaded-0.4.7/source/ -Isource -I/Users/slevin/.dub/packages/derelict-sfml2-1.1.0/source/ -I/Users/slevin/.dub/packages/derelict-util-1.9.1/source/ -I/Users/slevin/.dub/packages/gl3n-1.0.1

import unit_threaded.runner;
import test.path;
import test.field;

int main(string[] args) {
    return args.runTests!(
                          test.path,
                          test.field
                          );
}
