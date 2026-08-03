void dumpOnePacket(unsigned char *buf, int pos, int len) {
    printf("buffer: ");
    for (int i=0; i < len; i++) {
        printf(" %02x", buf[pos + i]);
    }
    printf("\n");
}



void readTestCommands(char *fn, struct packetContext *ctx) {
    FILE* fil = fopen(fn, "r");
    if (fil == NULL) err("error opening commands");
    for (;;) {
        char* lin = NULL;
        size_t len = 0;
        if (getline(&lin, &len, fil) < 0) break;
        doOneCommand(ctx, (unsigned char*) lin);
        free(lin);
    }
    fclose(fil);
}



int readTestBytes(char *fn, unsigned char *origD) {
    FILE* fil = fopen(fn, "r");
    if (fil == NULL) err("error opening bytes");
    int origS = 0;
    for (;;) {
        char* lin = NULL;
        size_t len = 0;
        if (getline(&lin, &len, fil) < 0) break;
        for (int i = 0;; i++) {
            if (lin[i] == 0) break;
            if (lin[i] == 32) continue;
            if (sscanf(&lin[i], "%hhx", &origD[origS]) != 1) continue;
            origS++;
            i++;
        }
        free(lin);
    }
    fclose(fil);
    return origS;
}
