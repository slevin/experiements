#! /usr/local/bin/rdmd

struct Field(size_t cols, size_t rows) {
    int spots[cols][rows];
    int allSpots[cols * rows * 2];
}

void main() {

    Field!(100, 100) fld;


}
    /*

want a struct that takes some parameters
maybe some init that uses them like field
and a main, basically simplify as much as possible
but still recreate the problem




     */
