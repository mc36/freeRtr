#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <alsa/asoundlib.h>

#include "io_util.h"
#include "in_raw.h"
#include "out_dev.h"


int main(int argc, char**argv) {
    ply_init(argv[1]);
    rec_init(argv[2]);
    iou_loop();
    return 0;
}
