
/**
 * interpret samples
 *
 * @author matecsaba
 */
public abstract interface samples {

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
     * get the samples
     *
     * @return codec to use
     */
    public static samples getSamples() {
        switch (devicer.smpb) {
            case 1:
                return new samples1b();
            case 2:
                return new samples2b();
            case 3:
                return new samples3b();
            case 4:
                return new samples4b();
            default:
                return null;
        }
    }

}

class samples1b implements samples {

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

class samples2b implements samples {

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

class samples3b implements samples {

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

class samples4b implements samples {

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
