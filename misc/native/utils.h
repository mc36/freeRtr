#define put16bits(buf, ofs, val) buf[ofs+0] = val >> 8; buf[ofs+1] = val & 0xff;

#define get16bits(buf, ofs) ((buf[ofs+0] << 8) | buf[ofs+1])
