int plyHnd;

void ply_init(char*fil) {
    plyHnd = open(fil, O_WRONLY | O_CREAT);
    if (plyHnd < 0) err("error creating");
    write(plyHnd, &bufD[padln], wavln);
}

void iou_write() {
    if (write(plyHnd, &bufD[padln], bufS) != bufS) err("error writing");
}

void iou_stop() {
    long len = lseek(plyHnd, 0, SEEK_END);
    lseek(plyHnd, 0, SEEK_SET);
    char riff[] = {0x72, 0x69, 0x66, 0x66, 0x2E, 0x91, 0xCF, 0x11, 0xA5, 0xD6, 0x28, 0xDB, 0x04, 0xC1, 0x00, 0x00};
    char wave[] = {0x77, 0x61, 0x76, 0x65, 0xF3, 0xAC, 0xD3, 0x11, 0x8C, 0xD1, 0x00, 0xC0, 0x4F, 0x8E, 0xDB, 0x8A};
    char frmt[] = {
        0x66, 0x6D, 0x74, 0x20, 0xF3, 0xAC, 0xD3, 0x11, 0x8C, 0xD1, 0x00, 0xC0, 0x4F, 0x8E, 0xDB, 0x8A,
        0x28, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
    };
    char data[] = {0x64, 0x61, 0x74, 0x61, 0xF3, 0xAC, 0xD3, 0x11, 0x8C, 0xD1, 0x00, 0xC0, 0x4F, 0x8E, 0xDB, 0x8A};
    struct {
        char riff[sizeof(riff)];
        long total;
        char wave[sizeof(wave)];
        char frmt[sizeof(frmt)];
        short format;
        short channel;
        int frequency;
        int bytesPerSec;
        short bytesPerBlock;
        short bitsPerSample;
        char data[sizeof(data)];
        long payload;
    } hdr;
    memcpy(hdr.riff, riff, sizeof(riff));
    hdr.total = len;
    memcpy(hdr.wave, wave, sizeof(wave));
    memcpy(hdr.frmt, frmt, sizeof(frmt));
    hdr.format = 1; // pcm
    hdr.channel = 2; // stereo
    hdr.frequency = srate;
    hdr.bytesPerSec = srate * smpbt * 2;
    hdr.bytesPerBlock = smpbt * 2;
    hdr.bitsPerSample = smpbt * 8;
    memcpy(hdr.data, data, sizeof(data));
    hdr.payload = len - 0x50;
    write(plyHnd, &hdr, sizeof(hdr));
    close(plyHnd);
}
