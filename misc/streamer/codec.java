
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
     * get the codec
     *
     * @return codec to use
     */
    public static codec getCodec() {
        switch (devicer.smpb) {
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

}
