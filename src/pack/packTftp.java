package pack;

/**
 * trivial file transfer protocol (rfc1350) packet
 *
 * @author matecsaba
 */
public class packTftp {

    /**
     * port number
     */
    public final static int port = 69;

    /**
     * block size
     */
    public final static int size = 512;

    /**
     * type of packet
     */
    public int typ;

    /**
     * block number
     */
    public int blk;

    /**
     * name of file
     */
    public String nam;

    /**
     * mode of file
     */
    public String mod;

    /**
     * data of packet
     */
    public byte[] dat;

    /**
     * read request
     */
    public final static int msgRead = 1;

    /**
     * write request
     */
    public final static int msgWrite = 2;

    /**
     * data
     */
    public final static int msgData = 3;

    /**
     * acknowledgement
     */
    public final static int msgAck = 4;

    /**
     * acknowledgement
     */
    public final static int msgError = 5;

    /**
     * convert message type to string
     *
     * @param i message type
     * @return string
     */
    public static String type2string(int i) {
        switch (i) {
            case msgRead:
                return "read";
            case msgWrite:
                return "write";
            case msgData:
                return "data";
            case msgAck:
                return "ack";
            case msgError:
                return "error";
            default:
                return "unknown=" + i;
        }
    }

    /**
     * dump out packet
     *
     * @return string
     */
    public String dump() {
        int i;
        if (dat == null) {
            i = -1;
        } else {
            i = dat.length;
        }
        return "typ=" + type2string(typ) + " blk=" + blk + " fil=" + nam + " mod=" + mod + " dat=" + i;
    }

    /**
     * parse one packet
     *
     * @param pck packet to read
     * @return false on success, true on error
     */
    public boolean parsePacket(packHolder pck) {
        typ = pck.msbGetW(0); // message type
        pck.getSkip(2);
        switch (typ) {
            case msgRead:
            case msgWrite:
                nam = pck.getAsciiZ(0, pck.dataSize(), 0);
                pck.getSkip(nam.length() + 1);
                mod = pck.getAsciiZ(0, pck.dataSize(), 0);
                return false;
            case msgData:
                blk = pck.msbGetW(0); // block number
                pck.getSkip(2);
                dat = pck.getCopy();
                return false;
            case msgAck:
                blk = pck.msbGetW(0); // block number
                return false;
            case msgError:
                blk = pck.msbGetW(0); // error code
                nam = pck.getAsciiZ(2, pck.dataSize(), 0);
                return false;
            default:
                return true;
        }
    }

    /**
     * create one packet
     *
     * @return packet created
     */
    public packHolder createPacket() {
        packHolder pck = new packHolder(true, true);
        pck.msbPutW(0, typ);
        pck.putSkip(2);
        switch (typ) {
            case msgRead:
            case msgWrite:
                pck.putAsciiZ(0, nam.length() + 1, nam, 0);
                pck.putSkip(nam.length() + 1);
                pck.putAsciiZ(0, mod.length() + 1, mod, 0);
                pck.putSkip(mod.length() + 1);
                return pck;
            case msgData:
                pck.msbPutW(0, blk);
                pck.putSkip(2);
                pck.putCopy(dat, 0, 0, dat.length);
                pck.putSkip(dat.length);
                return pck;
            case msgAck:
                pck.msbPutW(0, blk);
                pck.putSkip(2);
                return pck;
            case msgError:
                pck.msbPutW(0, blk);
                pck.putSkip(2);
                pck.putAsciiZ(0, nam.length() + 1, nam, 0);
                pck.putSkip(nam.length() + 1);
                return pck;
            default:
                return null;
        }
    }

}
