
/**
 * packet io kinds
 *
 * @author matecsaba
 */
public abstract class packet {

    /**
     * packer
     */
    protected final packer pck;

    /**
     * create instance
     *
     * @param p packer
     */
    protected packet(packer p) {
        pck = p;
    }

    /**
     * read data
     *
     * @param buf msb bytes
     * @return bytes
     * @throws Exception on error
     */
    public abstract int readKind(byte[] buf) throws Exception;

    /**
     * write scream data
     *
     * @param buf msb bytes
     * @param len length
     * @throws Exception on error
     */
    public abstract void writeKind(byte[] buf, int len) throws Exception;

    /**
     * string to kind
     *
     * @param a string
     * @param p packer
     * @return kind
     */
    public static packet string2kind(String a, packer p) throws Exception {
        if (a.equals("rtp")) {
            return new kindRtp(p);
        }
        if (a.equals("scr")) {
            return new kindScr(p);
        }
        if (a.equals("vba")) {
            return new kindVba(p);
        }
        if (a.equals("wfa")) {
            return new kindWfa(p);
        }
        if (a.equals("udp")) {
            return new kindUdp(p);
        }
        throw new Exception("unknown kind");
    }

}

class kindRtp extends packet {

    public kindRtp(packer p) {
        super(p);
    }

    public int readKind(byte[] buf) throws Exception {
        return pck.readRtp(buf);
    }

    public void writeKind(byte[] buf, int len) throws Exception {
        pck.writeRtp(buf, len);
    }

}

class kindScr extends packet {

    public kindScr(packer p) {
        super(p);
    }

    public int readKind(byte[] buf) throws Exception {
        return pck.readScr(buf);
    }

    public void writeKind(byte[] buf, int len) throws Exception {
        pck.writeScr(buf, len);
    }

}

class kindVba extends packet {

    public kindVba(packer p) {
        super(p);
    }

    public int readKind(byte[] buf) throws Exception {
        return pck.readVba(buf);
    }

    public void writeKind(byte[] buf, int len) throws Exception {
        pck.writeVba(buf, len);
    }

}

class kindWfa extends packet {

    public kindWfa(packer p) {
        super(p);
    }

    public int readKind(byte[] buf) throws Exception {
        return pck.readWfa(buf);
    }

    public void writeKind(byte[] buf, int len) throws Exception {
        pck.writeWfa(buf, len);
    }

}

class kindUdp extends packet {

    public kindUdp(packer p) {
        super(p);
    }

    public int readKind(byte[] buf) throws Exception {
        return pck.readUdp(buf);
    }

    public void writeKind(byte[] buf, int len) throws Exception {
        pck.writeUdp(buf, len);
    }

}
