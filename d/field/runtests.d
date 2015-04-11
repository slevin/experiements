#!/usr/local/bin/rdmd -unittest

import unit_threaded.runner;
import test.path;
       //import std.stdio;

int main(string[] args) {
    return args.runTests!(
                          test.path
                          );
}
