package pack;

/**
 * our proprietary replicator packets
 *
 * @author matecsaba
 */
public class packReplicator {

    /**
     * ethertype to use
     */
    public final static int ethTyp = 0x8086;

    /**
     * data packet
     */
    public final static int typData = 1;

    /**
     * keepalive packet
     */
    public final static int typKeep = 2;

    /**
     * counter packet
     */
    public final static int typCntr = 3;

    /**
     * type of packet
     */
    public int typ;

    /**
     * sequence number
     */
    public int seq;

    /**
     * parse one header
     *
     * @param pck packet to parse
     * @return false on success, true on error
     */
    public boolean parseHeader(packHolder pck) {
        if (pck.msbGetW(0) != ethTyp) {
            return true;
        }
        typ = pck.getByte(2);
        seq = pck.msbGetD(3);
        pck.getSkip(7);
        return false;
    }

    /**
     * create one header
     *
     * @param pck packet to update
     */
    public void createHeader(packHolder pck) {
        pck.msbPutW(0, ethTyp);
        pck.putByte(2, typ);
        pck.msbPutD(3, seq);
        pck.putSkip(7);
    }

}
