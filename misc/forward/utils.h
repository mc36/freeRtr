#define put16msb(buf, ofs, val) buf[ofs+0] = val >> 8; buf[ofs+1] = val & 0xff;

#define get16msb(buf, ofs) ((buf[ofs+0] << 8) | buf[ofs+1])

#define put32msb(buf, ofs, val) buf[ofs+0] = val >> 24; buf[ofs+1] = val >> 16; buf[ofs+2] = val >> 8; buf[ofs+3] = val & 0xff;

#define get32msb(buf, ofs) ((buf[ofs+0] << 24) | (buf[ofs+1] << 16) | (buf[ofs+2] << 8) | buf[ofs+3])

#define put16lsb(buf, ofs, val) buf[ofs+1] = val >> 8; buf[ofs+0] = val & 0xff;

#define get16lsb(buf, ofs) ((buf[ofs+1] << 8) | buf[ofs+0])

#define put32lsb(buf, ofs, val) buf[ofs+3] = val >> 24; buf[ofs+2] = val >> 16; buf[ofs+1] = val >> 8; buf[ofs+0] = val & 0xff;

#define get32lsb(buf, ofs) ((buf[ofs+3] << 24) | (buf[ofs+2] << 16) | (buf[ofs+1] << 8) | buf[ofs+0])
