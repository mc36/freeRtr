package org.freertr.pack;

import org.freertr.util.bits;

/**
 * trivial file transfer protocol (rfc1350) packet
 *
 * @author matecsaba
 */
public class packTftp {

    /**
     * create instance
     */
    public packTftp() {
    }

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
     * saw options
     */
    public boolean opts;

    /**
     * total size
     */
    public long tSiz;

    /**
     * block size
     */
    public int bSiz;

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
     * option acknowledgement
     */
    public final static int msgOAck = 6;

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
            case msgOAck:
                return "oack";
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
        return "typ=" + type2string(typ) + " blk=" + blk + " fil=" + nam + " mod=" + mod + " ts=" + tSiz + " bs=" + bSiz + " dat=" + i;
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
        opts = false;
        tSiz = -1;
        bSiz = -1;
        switch (typ) {
            case msgRead:
            case msgWrite:
                nam = pck.getAsciiZ(0, pck.dataSize(), 0);
                pck.getSkip(nam.length() + 1);
                mod = pck.getAsciiZ(0, pck.dataSize(), 0);
                pck.getSkip(mod.length() + 1);
                break;
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
            case msgOAck:
                break;
            default:
                return true;
        }
        for (;;) {
            String t = pck.getAsciiZ(0, pck.dataSize(), 0);
            pck.getSkip(t.length() + 1);
            if (t.length() < 1) {
                break;
            }
            String v = pck.getAsciiZ(0, pck.dataSize(), 0);
            pck.getSkip(v.length() + 1);
            if (v.length() < 1) {
                break;
            }
            if (t.equals("tsize")) {
                tSiz = bits.str2long(v);
                opts = true;
                continue;
            }
            if (t.equals("blksize")) {
                bSiz = bits.str2num(v);
                opts = true;
                continue;
            }
        }
        return false;
    }

    private void putStr(packHolder pck, String t) {
        pck.putAsciiZ(0, t.length() + 1, t, 0);
        pck.putSkip(t.length() + 1);
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
                putStr(pck, nam);
                putStr(pck, mod);
                break;
            case msgData:
                pck.msbPutW(0, blk);
                pck.putSkip(2);
                pck.putCopy(dat, 0, 0, dat.length);
                pck.putSkip(dat.length);
                break;
            case msgAck:
                pck.msbPutW(0, blk);
                pck.putSkip(2);
                return pck;
            case msgError:
                pck.msbPutW(0, blk);
                pck.putSkip(2);
                putStr(pck, nam);
                return pck;
            case msgOAck:
                break;
            default:
                return null;
        }
        if (!opts) {
            return pck;
        }
        if (tSiz >= 0) {
            putStr(pck, "tsize");
            putStr(pck, "" + tSiz);
        }
        if (bSiz >= 0) {
            putStr(pck, "blksize");
            putStr(pck, "" + bSiz);
        }
        return pck;
    }

}
