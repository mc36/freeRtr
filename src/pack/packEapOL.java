package pack;

/**
 * eap over lan (ieee 802.1x) protocol packet
 *
 * @author matecsaba
 */
public class packEapOL {

    /**
     * ethertype of these packets
     */
    public final static int type = 0x888e;

    /**
     * size of header
     */
    public final static int size = 6;

    /**
     * eap packet
     */
    public final static int typData = 0;

    /**
     * start frame
     */
    public final static int typStart = 1;

    /**
     * logoff frame
     */
    public final static int typLogoff = 2;

    /**
     * key frame
     */
    public final static int typKey = 3;

    /**
     * encapsulated asf start frame
     */
    public final static int typEncap = 4;

    /**
     * protocol version
     */
    public int ver;

    /**
     * message type
     */
    public int typ;

    /**
     * convert type to string
     *
     * @param i type
     * @return string
     */
    public static String type2string(int i) {
        switch (i) {
            case typData:
                return "data";
            case typStart:
                return "start";
            case typLogoff:
                return "logoff";
            case typKey:
                return "key";
            case typEncap:
                return "encap";
            default:
                return "unknown=" + i;
        }
    }

    /**
     * parse one packet
     *
     * @param pck packet to read
     * @return false on success, true on error
     */
    public boolean parseHeader(packHolder pck) {
        if (pck.msbGetW(0) != type) {
            return true;
        }
        ver = pck.getByte(2); // version
        typ = pck.getByte(3); // type
        int len = pck.msbGetW(4); // body length
        pck.getSkip(size);
        if (len > pck.dataSize()) {
            return true;
        }
        pck.setDataSize(len);
        return false;
    }

    /**
     * create header
     *
     * @param pck packet to update
     */
    public void createHeader(packHolder pck) {
        pck.msbPutW(0, type);
        pck.putByte(2, ver); // version
        pck.putByte(3, typ); // type
        pck.msbPutW(4, pck.dataSize()); // body length
        pck.putSkip(size);
        pck.merge2beg();
        pck.ETHtrg.fromString("0180:c200:0003");
    }

    /**
     * create command packet
     *
     * @param pck packet to update
     * @param type packet to update
     */
    public void createCmd(packHolder pck, int type) {
        ver = 2;
        typ = type;
        createHeader(pck);
    }

    /**
     * create data packet
     *
     * @param pck packet to update
     */
    public void createData(packHolder pck) {
        createCmd(pck, typData);
    }

    /**
     * dump one packet
     *
     * @return string
     */
    public String dump() {
        return "ver=" + ver + " type=" + type2string(typ);
    }

}
