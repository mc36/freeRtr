#include "io_incl.h"
#include "io_util.h"
#include "in_nil.h"
#include "out_raw.h"


int main(int argc, char**argv) {
    if (argc <= 1) err("usage this <file>");
    ply_init(argv[1]);
    rec_init();
    iou_loop();
    return 0;
}
