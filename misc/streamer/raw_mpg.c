#include "io_incl.h"
#include "io_util.h"
#include "in_mpg.h"
#include "out_raw.h"


int main(int argc, char**argv) {
    if (argc <= 3) err("usage this <file> <seek> <target>");
    ply_init(argv[3]);
    rec_init(argv[1], argv[2]);
    iou_loop();
    return 0;
}
