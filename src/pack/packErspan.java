package pack;

/**
 * encapsulated remote switch port analizer (erspan) packet
 *
 * @author matecsaba
 */
public class packErspan {

    /**
     * protocol number
     */
    public final static int prot = 47;

    /**
     * ethertype
     */
    public final static int ethtyp = 0x88be;

    /**
     * sequence number
     */
    public int seq;

    /**
     * vlan id
     */
    public int vlan;

    /**
     * span id
     */
    public int span;

    /**
     * create one header
     *
     * @param pck packet to update
     */
    public void createHeader(packHolder pck) {
        pck.msbPutW(0, 0x1000); // flags
        pck.msbPutW(2, ethtyp); // ethertype
        pck.msbPutD(4, seq); // sequence number
        pck.putSkip(8);
        pck.msbPutW(0, (vlan & 0xfff) | 0x1000); // vlan id
        pck.msbPutW(2, (span & 0x3ff) | 0xe000); // span id
        pck.msbPutD(4, 0x80);
        pck.putSkip(8);
        pck.merge2beg();
    }

    /**
     * parse one header
     *
     * @param pck packet to update
     * @return false on success, true on error
     */
    public boolean parseHeader(packHolder pck) {
        if (pck.msbGetW(0) != 0x1000) { // flags
            return true;
        }
        if (pck.msbGetW(2) != ethtyp) { // ethertype
            return true;
        }
        seq = pck.msbGetD(4); // sequence number
        pck.getSkip(8);
        vlan = pck.msbGetW(0) & 0xfff; // vlan id
        span = pck.msbGetW(2) & 0x3ff; // span id
        pck.getSkip(8);
        return false;
    }

}
