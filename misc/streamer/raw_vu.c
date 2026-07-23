#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <math.h>
#include <alsa/asoundlib.h>

#include "io_util.h"
#include "in_dev.h"
#include "out_vu.h"


int main(int argc, char**argv) {
    rec_init(argv[1]);
    ply_init();
    iou_loop();
    return 0;
}
