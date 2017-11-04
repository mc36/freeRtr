package auth;

import pack.packHolder;

/**
 * authentication header
 *
 * @author matecsaba
 */
public class authenHead {

    /**
     * size of header
     */
    public final static int size = 4;

    /**
     * code
     */
    public int code;

    /**
     * id
     */
    public int id;

    /**
     * parse packet
     *
     * @param pck packet to parse
     * @return false on success, true on error
     */
    public boolean parsePack(packHolder pck) {
        code = pck.getByte(0); // code
        id = pck.getByte(1); // identification
        int i = pck.msbGetW(2); // size
        if (i > pck.dataSize()) {
            return true;
        }
        if (i < size) {
            return true;
        }
        pck.setDataSize(i);
        pck.getSkip(size);
        return false;
    }

    /**
     * update packet
     *
     * @param pck packet to update
     */
    public void updatePack(packHolder pck) {
        pck.merge2beg();
        pck.putByte(0, code); // code
        pck.putByte(1, id); // identification
        pck.msbPutW(2, pck.dataSize() + size); // size
        pck.putSkip(size);
        pck.merge2beg();
    }

    public String toString() {
        return "code=" + code + " id=" + id;
    }

}
