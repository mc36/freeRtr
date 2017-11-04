package cry;

/**
 * keyed hashing (ssl30) message authentication
 *
 * @author matecsaba
 */
public class cryHashSslMac extends cryHashGeneric {

    private cryHashGeneric alg;

    private byte[] ipad;

    private byte[] opad;

    private boolean prePad;

    private static int getPadSize(int i) {
        switch (i) {
            case 16:
                return 48;
            case 20:
                return 40;
            default:
                return 32;
        }
    }

    /**
     * set up sslmac
     *
     * @param mac mac algorithm
     * @param key key to use
     * @param pp pre pad
     */
    public cryHashSslMac(cryHashGeneric mac, byte[] key, boolean pp) {
        alg = mac;
        prePad = pp;
        alg.init();
        ipad = new byte[key.length + getPadSize(alg.getHashSize())];
        opad = new byte[ipad.length];
        for (int i = 0; i < ipad.length; i++) {
            ipad[i] = 0x36;
            opad[i] = 0x5c;
        }
        for (int i = 0; i < key.length; i++) {
            ipad[i] = key[i];
            opad[i] = key[i];
        }
    }

    public void init() {
        alg.init();
        if (prePad) {
            alg.update(ipad);
        }
    }

    public String getName() {
        return "sslmac-" + alg.getName();
    }

    public int getHashSize() {
        return alg.getHashSize();
    }

    public int getBlockSize() {
        return alg.getBlockSize();
    }

    public void update(int i) {
        alg.update(i);
    }

    public byte[] finish() {
        if (!prePad) {
            alg.update(ipad);
        }
        byte[] buf = alg.finish();
        alg.init();
        alg.update(opad);
        alg.update(buf);
        return alg.finish();
    }

}
