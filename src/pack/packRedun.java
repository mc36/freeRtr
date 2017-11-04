package pack;

/**
 * our proprietary redundancy packet
 *
 * @author matecsaba
 */
public class packRedun {

    /**
     * ethertype
     */
    public static final int ethtyp = 0x8087;

    /**
     * header size
     */
    public static final int size = 12;

    /**
     * hello
     */
    public static final int typHello = 1;

    /**
     * reload
     */
    public static final int typReload = 2;

    /**
     * acknowledge
     */
    public static final int typAck = 3;

    /**
     * file begin
     */
    public static final int typFilBeg = 4;

    /**
     * file data
     */
    public static final int typFilDat = 5;

    /**
     * file end
     */
    public static final int typFilEnd = 6;

    /**
     * filename begin
     */
    public static final String fnBegin = "//";

    /**
     * config file
     */
    public static final String fnStart = fnBegin + "startup";

    /**
     * image file
     */
    public static final String fnCore = fnBegin + "rtr+jar";

    /**
     * speaking
     */
    public final static int statSpeak = 1;

    /**
     * standby
     */
    public final static int statStandby = 2;

    /**
     * active
     */
    public final static int statActive = 3;

    /**
     * keepalive time
     */
    public final static int timeKeep = 200;

    /**
     * time multiplier
     */
    public final static int timeMult = 5;

    /**
     * hold time
     */
    public final static int timeHold = timeKeep * timeMult;

    /**
     * max size
     */
    public static final int dataMax = 1024;

    /**
     * type
     */
    public int type;

    /**
     * my state
     */
    public int state;

    /**
     * magic
     */
    public int magic;

    /**
     * peer state
     */
    public int peer;

    /**
     * type to string
     *
     * @param i type
     * @return string
     */
    public static String typ2str(int i) {
        switch (i) {
            case typHello:
                return "hello";
            case typReload:
                return "reload";
            case typAck:
                return "ack";
            case typFilBeg:
                return "filBeg";
            case typFilDat:
                return "filDat";
            case typFilEnd:
                return "filEnd";
            default:
                return "unknown=" + i;
        }
    }

    /**
     * parse header
     *
     * @param pck packet to read
     * @return false on success, true on error
     */
    public boolean parseHeader(packHolder pck) {
        if (pck.msbGetW(0) != ethtyp) {
            return true;
        }
        type = pck.getByte(2);
        state = pck.getByte(3);
        magic = pck.msbGetD(4);
        peer = pck.msbGetD(8);
        pck.getSkip(size);
        return false;
    }

    /**
     * create packet
     *
     * @param pck packet to update
     */
    public void createHeader(packHolder pck) {
        pck.msbPutW(0, ethtyp);
        pck.putByte(2, type);
        pck.putByte(3, state);
        pck.msbPutD(4, magic);
        pck.msbPutD(8, peer);
        pck.putSkip(size);
        pck.merge2beg();
    }

    public String toString() {
        return "type=" + typ2str(type) + " state=" + state + " magic=" + magic + " peer=" + peer;
    }

}
