#include "io_incl.h"
#include "io_util.h"
#include "in_dev.h"
#include "out_raw.h"


int main(int argc, char**argv) {
    if (argc <= 2) err("usage this <file> <target>");
    rec_init(argv[1]);
    ply_init(argv[2]);
    iou_loop();
    return 0;
}
