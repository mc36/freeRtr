#define put16bits(buf, ofs, val) buf[ofs+0] = val >> 8; buf[ofs+1] = val & 0xff;

#define get16bits(buf, ofs) ((buf[ofs+0] << 8) | buf[ofs+1])

#define put32bits(buf, ofs, val) buf[ofs+0] = val >> 24; buf[ofs+1] = val >> 16; buf[ofs+2] = val >> 8; buf[ofs+3] = val & 0xff;

#define get32bits(buf, ofs) ((buf[ofs+0] << 24) | (buf[ofs+1] << 16) | (buf[ofs+2] << 8) | buf[ofs+3])
