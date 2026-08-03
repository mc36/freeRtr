package org.freertr.pack;

import org.freertr.pipe.pipeSide;

/**
 * plan9 file protocol packet
 *
 * @author matecsaba
 */
public class packPlan9 {

    /**
     * create instance
     */
    public packPlan9() {
    }

    /**
     * port number
     */
    public final static int port = 564;

    /**
     * size of header
     */
    public final static int size = 7;

    /**
     * version
     */
    public final static int opcTversion = 100;

    /**
     * version
     */
    public final static int opcRversion = 101;

    /**
     * authentication
     */
    public final static int opcTauth = 102;

    /**
     * authentication
     */
    public final static int opcRauth = 103;

    /**
     * attach
     */
    public final static int opcTattach = 104;

    /**
     * attach
     */
    public final static int opcRattach = 105;

    /**
     * error
     */
    public final static int opcTerror = 106;

    /**
     * error
     */
    public final static int opcRerror = 107;

    /**
     * flush
     */
    public final static int opcTflush = 108;

    /**
     * flush
     */
    public final static int opcRflush = 109;

    /**
     * walk
     */
    public final static int opcTwalk = 110;

    /**
     * walk
     */
    public final static int opcRwalk = 111;

    /**
     * open
     */
    public final static int opcTopen = 112;

    /**
     * open
     */
    public final static int opcRopen = 113;

    /**
     * create
     */
    public final static int opcTcreate = 114;

    /**
     * create
     */
    public final static int opcRcreate = 115;

    /**
     * read
     */
    public final static int opcTread = 116;

    /**
     * read
     */
    public final static int opcRread = 117;

    /**
     * write
     */
    public final static int opcTwrite = 118;

    /**
     * write
     */
    public final static int opcRwrite = 119;

    /**
     * close
     */
    public final static int opcTclunk = 120;

    /**
     * close
     */
    public final static int opcRclunk = 121;

    /**
     * remove
     */
    public final static int opcTremove = 122;

    /**
     * remove
     */
    public final static int opcRremove = 123;

    /**
     * stat
     */
    public final static int opcTstat = 124;

    /**
     * stat
     */
    public final static int opcRstat = 125;

    /**
     * stat
     */
    public final static int opcTwstat = 126;

    /**
     * stat
     */
    public final static int opcRwstat = 127;

    /**
     * packet data
     */
    public packHolder pack;

    /**
     * pipeline to use
     */
    public pipeSide pipe;

    /**
     * opcode value
     */
    public int opcode;

    /**
     * tag value
     */
    public int tag;

    /**
     * convert opcode to string
     *
     * @param i opcode
     * @return string
     */
    public static String opcode2string(int i) {
        switch (i) {
            case opcTversion:
                return "tversion";
            case opcRversion:
                return "rversion";
            case opcTauth:
                return "tauth";
            case opcRauth:
                return "rauth";
            case opcTattach:
                return "tattach";
            case opcRattach:
                return "rattach";
            case opcTerror:
                return "terror";
            case opcRerror:
                return "rerror";
            case opcTflush:
                return "tflush";
            case opcRflush:
                return "rflush";
            case opcTwalk:
                return "twalk";
            case opcRwalk:
                return "rwalk";
            case opcTopen:
                return "topen";
            case opcRopen:
                return "ropen";
            case opcTcreate:
                return "tcreate";
            case opcRcreate:
                return "rcreate";
            case opcTread:
                return "tread";
            case opcRread:
                return "rread";
            case opcTwrite:
                return "twrite";
            case opcRwrite:
                return "rwrite";
            case opcTclunk:
                return "tclunk";
            case opcRclunk:
                return "rclunk";
            case opcTremove:
                return "tremove";
            case opcRremove:
                return "rremove";
            case opcTstat:
                return "tstat";
            case opcRstat:
                return "rstat";
            case opcTwstat:
                return "twstat";
            case opcRwstat:
                return "rwstat";
            default:
                return "unknown=" + i;
        }
    }

    /**
     * receive one packet
     *
     * @return false on success, true on error
     */
    public boolean packRecv() {
        opcode = -1;
        pack.clear();
        if (pack.pipeRecv(pipe, 0, size, 144) != size) {
            pipe.setClose();
            return true;
        }
        int datSiz = pack.lsbGetD(0) - size;
        if (datSiz < 0) {
            pipe.setClose();
            return true;
        }
        opcode = pack.getByte(4);
        tag = pack.lsbGetW(5);
        if (pack.pipeRecv(pipe, size, datSiz, 144) != datSiz) {
            pipe.setClose();
            return true;
        }
        pack.getSkip(size);
        return false;
    }

    /**
     * send one packet
     */
    public void packSend() {
        pack.merge2beg();
        int datSiz = pack.dataSize();
        pack.lsbPutD(0, datSiz + size);
        pack.putByte(4, opcode);
        pack.lsbPutW(5, tag);
        pack.putSkip(size);
        pack.merge2beg();
        pack.pipeSend(pipe, 0, pack.dataSize(), 3);
    }

    /**
     * dump message
     *
     * @return string
     */
    public String dumpHdr() {
        return "opc=" + opcode2string(opcode) + " tag=" + tag;
    }

    /**
     * get one string
     *
     * @return string, null on error
     */
    public String getStr() {
        int i = pack.lsbGetW(0);
        pack.getSkip(2);
        if (i > pack.dataSize()) {
            return null;
        }
        byte[] b = new byte[i];
        pack.getCopy(b, 0, 0, b.length);
        pack.getSkip(b.length);
        return new String(b);
    }

    /**
     * put one string
     *
     * @param a string to write
     */
    public void putStr(String a) {
        byte[] b = a.getBytes();
        pack.lsbPutW(0, b.length);
        pack.putSkip(2);
        pack.putCopy(b, 0, 0, b.length);
        pack.putSkip(b.length);
    }

}
