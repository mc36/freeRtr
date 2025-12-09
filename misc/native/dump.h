void hexdump(unsigned char *buf, int pos, int len) {
    printf("buffer: ");
    for (int i=0; i < len; i++) {
        printf(" %02x", buf[pos + i]);
    }
    printf("\n");
}
