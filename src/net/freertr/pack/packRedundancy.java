package net.freertr.pack;

/**
 * our proprietary redundancy packet
 *
 * @author matecsaba
 */
public class packRedundancy {

    /**
     * create instance
     */
    public packRedundancy() {
    }

    /**
     * ethertype
     */
    public static final int ethtyp = 0x8087;

    /**
     * header size
     */
    public static final int size = 20;

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
     * file checksum request
     */
    public static final int typSumReq = 7;

    /**
     * file checksum reply
     */
    public static final int typSumVal = 8;

    /**
     * config file
     */
    public static final String fnStart = "config";

    /**
     * image file
     */
    public static final String fnCore = "code";

    /**
     * init
     */
    public final static int statInit = 0;

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
     * peer magic
     */
    public int peer;

    /**
     * uptime
     */
    public int uptime;

    /**
     * priority
     */
    public int priority;

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
            case typSumReq:
                return "sumReq";
            case typSumVal:
                return "sumVal";
            default:
                return "unknown=" + i;
        }
    }

    /**
     * state to string
     *
     * @param i type
     * @return string
     */
    public static String stat2str(int i) {
        switch (i) {
            case statInit:
                return "init";
            case statSpeak:
                return "speak";
            case statStandby:
                return "standby";
            case statActive:
                return "active";
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
        uptime = pck.msbGetD(12);
        priority = pck.msbGetD(16);
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
        pck.msbPutD(12, uptime);
        pck.msbPutD(16, priority);
        pck.putSkip(size);
        pck.merge2beg();
    }

    /**
     * check if other better
     *
     * @param o other
     * @return reason, null if not
     */
    public String otherBetter(packRedundancy o) {
        if (priority < o.priority) {
            return "priority";
        }
        if (priority > o.priority) {
            return null;
        }
        if (uptime < o.uptime) {
            return "uptime";
        }
        if (uptime > o.uptime) {
            return null;
        }
        if (magic < o.magic) {
            return "magic";
        }
        if (magic > o.magic) {
            return null;
        }
        return null;
    }

    public String toString() {
        return "type=" + typ2str(type) + " state=" + stat2str(state) + " magic=" + magic + " peer=" + peer + " priority=" + priority + " uptime=" + uptime;
    }

}
