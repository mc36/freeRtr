
/**
 * byte swapper codecs
 *
 * @author matecsaba
 */
public abstract interface codec {

    /**
     * perform byte swap
     *
     * @param buf buffer
     * @param len size
     */
    public void byteSwap(byte[] buf, int len);

    /**
     * decode values
     *
     * @param val values
     * @param buf buffer
     * @param len size
     */
    public void decode(int[] val, byte[] buf, int len);

    /**
     * decode values
     *
     * @param val values
     * @param buf buffer
     * @param len size
     */
    public void encode(int[] val, byte[] buf, int len);

    /**
     * get the codec
     *
     * @return codec to use
     */
    public static codec getCodec() {
        switch (consts.smpb) {
            case 1:
                return new codec1b();
            case 2:
                return new codec2b();
            case 3:
                return new codec3b();
            case 4:
                return new codec4b();
            default:
                return null;
        }
    }

}

class codec1b implements codec {

    public void byteSwap(byte[] buf, int len) {
    }

    public void decode(int[] val, byte[] buf, int len) {
        for (int i = 0, p = 0; i < len; i += 1, p++) {
            int tmp0 = buf[i + 0] & 0xff;
            val[p] = tmp0 << 24;
        }
    }

    public void encode(int[] val, byte[] buf, int len) {
        for (int i = 0, p = 0; i < len; i += 1, p++) {
            int v = val[p];
            buf[i + 0] = (byte) (v >>> 24);
        }
    }

}

class codec2b implements codec {

    public void byteSwap(byte[] buf, int len) {
        for (int i = 0; i < len; i += 2) {
            byte tmp0 = buf[i + 0];
            byte tmp1 = buf[i + 1];
            buf[i + 0] = tmp1;
            buf[i + 1] = tmp0;
        }
    }

    public void decode(int[] val, byte[] buf, int len) {
        for (int i = 0, p = 0; i < len; i += 2, p++) {
            int tmp0 = buf[i + 0] & 0xff;
            int tmp1 = buf[i + 1] & 0xff;
            val[p] = (tmp0 << 24) | (tmp1 << 16);
        }
    }

    public void encode(int[] val, byte[] buf, int len) {
        for (int i = 0, p = 0; i < len; i += 2, p++) {
            int v = val[p];
            buf[i + 0] = (byte) (v >>> 24);
            buf[i + 1] = (byte) (v >>> 16);
        }
    }

}

class codec3b implements codec {

    public void byteSwap(byte[] buf, int len) {
        for (int i = 0; i < len; i += 3) {
            byte tmp0 = buf[i + 0];
            byte tmp1 = buf[i + 1];
            byte tmp2 = buf[i + 2];
            buf[i + 0] = tmp2;
            buf[i + 1] = tmp1;
            buf[i + 2] = tmp0;
        }
    }

    public void decode(int[] val, byte[] buf, int len) {
        for (int i = 0, p = 0; i < len; i += 3, p++) {
            int tmp0 = buf[i + 0] & 0xff;
            int tmp1 = buf[i + 1] & 0xff;
            int tmp2 = buf[i + 2] & 0xff;
            val[p] = (tmp0 << 24) | (tmp1 << 16) | (tmp2 << 8);
        }
    }

    public void encode(int[] val, byte[] buf, int len) {
        for (int i = 0, p = 0; i < len; i += 3, p++) {
            int v = val[p];
            buf[i + 0] = (byte) (v >>> 24);
            buf[i + 1] = (byte) (v >>> 16);
            buf[i + 2] = (byte) (v >>> 8);
        }
    }

}

class codec4b implements codec {

    public void byteSwap(byte[] buf, int len) {
        for (int i = 0; i < len; i += 4) {
            byte tmp0 = buf[i + 0];
            byte tmp1 = buf[i + 1];
            byte tmp2 = buf[i + 2];
            byte tmp3 = buf[i + 3];
            buf[i + 0] = tmp3;
            buf[i + 1] = tmp2;
            buf[i + 2] = tmp1;
            buf[i + 3] = tmp0;
        }
    }

    public void decode(int[] val, byte[] buf, int len) {
        for (int i = 0, p = 0; i < len; i += 4, p++) {
            int tmp0 = buf[i + 0] & 0xff;
            int tmp1 = buf[i + 1] & 0xff;
            int tmp2 = buf[i + 2] & 0xff;
            int tmp3 = buf[i + 3] & 0xff;
            val[p] = (tmp0 << 24) | (tmp1 << 16) | (tmp2 << 8) | tmp3;
        }
    }

    public void encode(int[] val, byte[] buf, int len) {
        for (int i = 0, p = 0; i < len; i += 4, p++) {
            int v = val[p];
            buf[i + 0] = (byte) (v >>> 24);
            buf[i + 1] = (byte) (v >>> 16);
            buf[i + 2] = (byte) (v >>> 8);
            buf[i + 3] = (byte) (v);
        }
    }

}
