package pack;

import pipe.pipeSide;
import util.version;
import cfg.cfgAll;

/**
 * point to point tunneling protocol (rfc2637) packet
 *
 * @author matecsaba
 */
public class packPptp {

    /**
     * protocol number
     */
    public final static int prot = 47;

    /**
     * port number
     */
    public final static int port = 1723;

    /**
     * ethertype
     */
    public final static int ethtyp = 0x880b;

    /**
     * Start-Control-Connection-Request
     */
    public final static int msgStartReq = 1;

    /**
     * Start-Control-Connection-Reply
     */
    public final static int msgStartRep = 2;

    /**
     * Stop-Control-Connection-Request
     */
    public final static int msgStopReq = 3;

    /**
     * Stop-Control-Connection-Reply
     */
    public final static int msgStopRep = 4;

    /**
     * Echo-Control-Connection-Request
     */
    public final static int msgEchoReq = 5;

    /**
     * Echo-Control-Connection-Reply
     */
    public final static int msgEchoRep = 6;

    /**
     * Outgoing-Call-Request
     */
    public final static int msgOutReq = 7;

    /**
     * Outgoing-Call-Reply
     */
    public final static int msgOutRep = 8;

    /**
     * Incoming-Call-Request
     */
    public final static int msgInReq = 9;

    /**
     * Incoming-Call-Reply
     */
    public final static int msgInRep = 10;

    /**
     * Incoming-Call-Connected
     */
    public final static int msgInCon = 11;

    /**
     * Call-Clear-Request
     */
    public final static int msgClrReq = 12;

    /**
     * Call-Disconnect-Notify
     */
    public final static int msgDscNot = 13;

    /**
     * WAN-Error-Notify
     */
    public final static int msgWanErr = 14;

    /**
     * Set-Link-Info
     */
    public final static int msgLnkInf = 15;

    /**
     * magic cookie
     */
    public final static int magicCookie = 0x1a2b3c4d;

    /**
     * type of packet
     */
    public int typ;

    /**
     * result code
     */
    public int resCod;

    /**
     * error code
     */
    public int errCod;

    /**
     * framing capability
     */
    public int frmCap;

    /**
     * bearer capability
     */
    public int berCap;

    /**
     * channel number
     */
    public int chnNum;

    /**
     * firmware version
     */
    public int frmVer;

    /**
     * hostname
     */
    public String hstNam;

    /**
     * vendor is
     */
    public String vndNam;

    /**
     * local call id
     */
    public int callLoc;

    /**
     * remote call id
     */
    public int callRem;

    /**
     * minimum bps
     */
    public int bpsMin;

    /**
     * maximum bps
     */
    public int bpsMax;

    /**
     * window size
     */
    public int winSiz;

    /**
     * processing delay
     */
    public int prcDel;

    /**
     * cause code
     */
    public int cause;

    /**
     * receive sequence number
     */
    public int seqRx = -1;

    /**
     * transmit sequence number
     */
    public int seqTx = -1;

    /**
     * convert message type to string
     *
     * @param i message type
     * @return string
     */
    public final String type2string(int i) {
        switch (i) {
            case msgStartReq:
                return "startReq";
            case msgStartRep:
                return "startRep";
            case msgStopReq:
                return "stopReq";
            case msgStopRep:
                return "stopRep";
            case msgEchoReq:
                return "echoReq";
            case msgEchoRep:
                return "echoRep";
            case msgOutReq:
                return "outReq";
            case msgOutRep:
                return "outRep";
            case msgInReq:
                return "inReq";
            case msgInRep:
                return "inRep";
            case msgInCon:
                return "inCon";
            case msgClrReq:
                return "clearReq";
            case msgDscNot:
                return "disconNotif";
            case msgWanErr:
                return "wanErr";
            case msgLnkInf:
                return "linkInfo";
            default:
                return "unknown=" + i;
        }
    }

    /**
     * dump this packet
     *
     * @return packet dump
     */
    public String dump() {
        return "typ=" + type2string(typ) + " res=" + resCod + " err=" + errCod + " fram=" + frmCap + " bear=" + berCap
                + " chan=" + chnNum + " firm=" + frmVer + " host=" + hstNam + " vend=" + vndNam + " call=" + callLoc + "/"
                + callRem + " bps=" + bpsMin + "-" + bpsMax + " win=" + winSiz + " delay=" + prcDel + " cause=" + cause;
    }

    /**
     * receive one packet
     *
     * @param pip pipeline to use
     * @param pck packet to use
     * @return false on success, true on error
     */
    public boolean recvPack(pipeSide pip, packHolder pck) {
        packSize rdr = new packSize(pip, 2, true, 1, 2);
        return rdr.recvPacket(pck);
    }

    /**
     * send one packet
     *
     * @param pip pipeline to use
     * @param pck packet to use
     * @return false on success, true on error
     */
    public boolean sendPack(pipeSide pip, packHolder pck) {
        packSize rdr = new packSize(pip, 2, true, 1, 2);
        return rdr.sendPacket(pck);
    }

    /**
     * parse data header
     *
     * @param pck packet to use
     * @return false on success, true on error
     */
    public boolean parseData(packHolder pck) {
        int flg = pck.msbGetW(0); // flags
        if ((flg & 7) != 1) { // version
            return true;
        }
        int len = pck.msbGetW(2); // ethertype
        if (len != ethtyp) {
            return true;
        }
        len = pck.msbGetW(4); // payload length
        callRem = pck.msbGetW(6); // call id
        pck.getSkip(8);
        if ((flg & 0x1000) != 0) {
            seqTx = pck.msbGetD(0); // sequence number
            pck.getSkip(4);
        }
        if ((flg & 0x80) != 0) {
            seqRx = pck.msbGetD(0); // sequence number
            pck.getSkip(4);
        }
        if (pck.dataSize() < len) {
            return true;
        }
        pck.setDataSize(len);
        return false;
    }

    /**
     * create data header
     *
     * @param pck packet to use
     */
    public void createData(packHolder pck) {
        boolean ts = (seqTx >= 0);
        boolean rs = (seqRx >= 0);
        int i = 0x2001;
        if (ts) {
            i |= 0x1000;
        }
        if (rs) {
            i |= 0x80;
        }
        pck.msbPutW(0, i); // flags
        pck.msbPutW(2, ethtyp); // ethertype
        pck.msbPutW(4, pck.dataSize()); // payload length
        pck.msbPutW(6, callRem); // call id
        pck.putSkip(8);
        if (ts) {
            pck.msbPutD(0, seqTx);
            pck.putSkip(4);
        }
        if (rs) {
            pck.msbPutD(0, seqRx);
            pck.putSkip(4);
        }
        pck.merge2beg();
    }

    /**
     * parse control header
     *
     * @param pck packet to use
     * @return false on success, true on error
     */
    public boolean parseControl(packHolder pck) {
        typ = pck.msbGetW(0); // message type
        if (typ != 1) {
            return true;
        }
        int i = pck.msbGetD(2); // magic cookie
        if (i != magicCookie) {
            return true;
        }
        typ = pck.msbGetW(6); // message type
        pck.getSkip(8);
        return false;
    }

    /**
     * create control header
     *
     * @param pck packet to use
     */
    public void createControl(packHolder pck) {
        pck.merge2beg();
        pck.msbPutW(0, 1); // message type
        pck.msbPutD(2, magicCookie); // magic cookie
        pck.msbPutW(6, typ); // message type
        pck.putSkip(8);
        pck.merge2beg();
    }

    /**
     * fill start
     *
     * @param req true for request, false for reply
     */
    public void fillStart(boolean req) {
        if (req) {
            resCod = 0;
            typ = msgStartReq;
        } else {
            resCod = 1;
            typ = msgStartRep;
        }
        errCod = 0;
        frmCap = 3;
        berCap = 3;
        chnNum = 0;
        frmVer = 0;
        hstNam = cfgAll.hostName;
        vndNam = version.namVer;
    }

    /**
     * parse start
     *
     * @param pck packet to use
     * @param req true for request, false for reply
     * @return false on success, true on error
     */
    public boolean parseStart(packHolder pck, boolean req) {
        if (req) {
            if (typ != msgStartReq) {
                return true;
            }
        } else {
            if (typ != msgStartRep) {
                return true;
            }
        }
        int i = pck.msbGetW(2); // protocol version
        if (i != 0x100) {
            return true;
        }
        resCod = pck.getByte(4); // result code
        errCod = pck.getByte(5); // error code
        frmCap = pck.msbGetD(6); // framing capability
        berCap = pck.msbGetD(10); // bearer capability
        chnNum = pck.msbGetW(14); // maximum channels
        frmVer = pck.msbGetW(16); // firmware version
        hstNam = pck.getAsciiZ(18, 64, 0); // hostname
        vndNam = pck.getAsciiZ(82, 64, 0); // vendor
        return false;
    }

    /**
     * create start
     *
     * @param pck packet to use
     */
    public void createStart(packHolder pck) {
        pck.clear();
        pck.msbPutW(0, 0); // reserved
        pck.msbPutW(2, 0x100); // protocol version
        pck.putByte(4, resCod); // result code
        pck.putByte(5, errCod); // error code
        pck.msbPutD(6, frmCap); // framing capability
        pck.msbPutD(10, berCap); // bearer capability
        pck.msbPutW(14, chnNum); // maximum channels
        pck.msbPutW(16, frmVer); // firmware version
        pck.putAsciiZ(18, 64, hstNam, 0); // hostname
        pck.putAsciiZ(82, 64, vndNam, 0); // vendor
        pck.putSkip(146);
    }

    /**
     * fill stop
     *
     * @param req true for request, false for reply
     */
    public void fillStop(boolean req) {
        if (req) {
            typ = msgStopReq;
        } else {
            typ = msgStopRep;
        }
        resCod = 1;
    }

    /**
     * parse stop
     *
     * @param pck packet to use
     * @param req true for request, false for reply
     * @return false on success, true on error
     */
    public boolean parseStop(packHolder pck, boolean req) {
        if (req) {
            if (typ != msgStopReq) {
                return true;
            }
        } else {
            if (typ != msgStopRep) {
                return true;
            }
        }
        resCod = pck.getByte(2); // reason code
        errCod = pck.getByte(3); // error code
        return false;
    }

    /**
     * create stop
     *
     * @param pck packet to use
     */
    public void createStop(packHolder pck) {
        pck.clear();
        pck.msbPutW(0, 0); // reserved
        pck.putByte(2, resCod); // reason code
        pck.putByte(3, errCod); // error code
        pck.msbPutW(4, 0); // reserved
        pck.putSkip(6);
    }

    /**
     * fill echo
     *
     * @param req true for request, false for reply
     * @param id identification
     */
    public void fillEcho(boolean req, int id) {
        if (req) {
            typ = msgEchoReq;
        } else {
            typ = msgEchoRep;
        }
        resCod = 1;
        callLoc = id;
    }

    /**
     * parse echo
     *
     * @param pck packet to use
     * @param req true for request, false for reply
     * @return false on success, true on error
     */
    public boolean parseEcho(packHolder pck, boolean req) {
        if (req) {
            if (typ != msgEchoReq) {
                return true;
            }
        } else {
            if (typ != msgEchoRep) {
                return true;
            }
        }
        callLoc = pck.msbGetD(2); // identification
        resCod = pck.getByte(6); // result code
        errCod = pck.getByte(7); // error code
        return false;
    }

    /**
     * create echo
     *
     * @param pck packet to use
     */
    public void createEcho(packHolder pck) {
        pck.clear();
        pck.msbPutW(0, 0); // reserved
        pck.msbPutD(2, callLoc); // identification
        pck.putSkip(6);
        if (typ == msgEchoReq) {
            return;
        }
        pck.putByte(0, resCod); // reason code
        pck.putByte(1, errCod); // error code
        pck.msbPutW(2, 0); // reserved
        pck.putSkip(4);
    }

    /**
     * fill out call request
     *
     * @param loc local session
     * @param called called number
     */
    public void fillOutReq(int loc, String called) {
        typ = msgOutReq;
        callLoc = loc;
        callRem = loc;
        if (called == null) {
            hstNam = "";
        } else {
            hstNam = called;
        }
        vndNam = "";
        bpsMin = 1;
        bpsMax = 1000000000;
        frmCap = 1;
        berCap = 1;
        winSiz = 1024;
        prcDel = 0;
    }

    /**
     * parse out call request
     *
     * @param pck packet to use
     * @return false on success, true on error
     */
    public boolean parseOutReq(packHolder pck) {
        if (typ != msgOutReq) {
            return true;
        }
        callLoc = pck.msbGetW(2); // call id
        callRem = pck.msbGetW(4); // call serial
        bpsMin = pck.msbGetD(6); // minimum bps
        bpsMax = pck.msbGetD(10); // maximum bps
        berCap = pck.msbGetD(14); // bearer capability
        frmCap = pck.msbGetD(18); // framing capability
        winSiz = pck.msbGetW(22); // rx window size
        prcDel = pck.msbGetW(24); // processing delay
        hstNam = pck.getAsciiZ(30, 64, 0); // phone number
        vndNam = pck.getAsciiZ(94, 64, 0); // subaddress
        return false;
    }

    /**
     * create out call request
     *
     * @param pck packet to use
     */
    public void createOutReq(packHolder pck) {
        pck.clear();
        pck.msbPutW(0, 0); // reserved
        pck.msbPutW(2, callLoc); // call id
        pck.msbPutW(4, callRem); // call serial
        pck.msbPutD(6, bpsMin); // minimum bps
        pck.msbPutD(10, bpsMax); // maximum bps
        pck.msbPutD(14, berCap); // bearer capability
        pck.msbPutD(18, frmCap); // framing capability
        pck.msbPutW(22, winSiz); // rx window size
        pck.msbPutW(24, prcDel); // processing delay
        pck.msbPutW(26, hstNam.length()); // phone number length
        pck.msbPutW(28, 0); // reserved
        pck.putAsciiZ(30, 64, hstNam, 0); // phone number
        pck.putAsciiZ(94, 64, vndNam, 0); // subaddress
        pck.putSkip(158);
    }

    /**
     * fill out call reply
     *
     * @param loc local session
     * @param rem remote session
     */
    public void fillOutRep(int loc, int rem) {
        typ = msgOutRep;
        callLoc = loc;
        callRem = rem;
        resCod = 1;
        bpsMin = 64000;
        winSiz = 1024;
        prcDel = 0;
    }

    /**
     * parse out call reply
     *
     * @param pck packet to use
     * @return false on success, true on error
     */
    public boolean parseOutRep(packHolder pck) {
        if (typ != msgOutRep) {
            return true;
        }
        callLoc = pck.msbGetW(2); // call id
        callRem = pck.msbGetW(4); // call serial
        resCod = pck.getByte(6); // result code
        errCod = pck.getByte(7); // error code
        cause = pck.msbGetW(8); // cause code
        bpsMin = pck.msbGetD(10); // connect speed
        winSiz = pck.msbGetW(14); // rx window size
        prcDel = pck.msbGetW(16); // processing delay
        chnNum = pck.msbGetD(18); // channel id
        return false;
    }

    /**
     * create out call reply
     *
     * @param pck packet to use
     */
    public void createOutRep(packHolder pck) {
        pck.clear();
        pck.msbPutW(0, 0); // reserved
        pck.msbPutW(2, callLoc); // call id
        pck.msbPutW(4, callRem); // call serial
        pck.putByte(6, resCod); // result code
        pck.putByte(7, errCod); // error code
        pck.msbPutW(8, cause); // cause code
        pck.msbPutD(10, bpsMin); // connect speed
        pck.msbPutW(14, winSiz); // rx window size
        pck.msbPutW(16, prcDel); // processing delay
        pck.msbPutD(18, chnNum); // channel id
        pck.putSkip(22);
    }

    /**
     * fill in call request
     *
     * @param loc local session
     * @param called called number
     * @param calling calling number
     */
    public void fillInReq(int loc, String called, String calling) {
        typ = msgInReq;
        callLoc = loc;
        callRem = loc;
        if (called == null) {
            hstNam = "";
        } else {
            hstNam = called;
        }
        if (calling == null) {
            vndNam = "";
        } else {
            vndNam = calling;
        }
        berCap = 1;
        winSiz = 1024;
        prcDel = 0;
    }

    /**
     * parse in call request
     *
     * @param pck packet to use
     * @return false on success, true on error
     */
    public boolean parseInReq(packHolder pck) {
        if (typ != msgInReq) {
            return true;
        }
        callLoc = pck.msbGetW(2); // call id
        callRem = pck.msbGetW(4); // call serial
        berCap = pck.msbGetD(6); // bearer capability
        chnNum = pck.msbGetD(10); // channel number
        hstNam = pck.getAsciiZ(18, 64, 0); // called number
        vndNam = pck.getAsciiZ(82, 64, 0); // calling number
        return false;
    }

    /**
     * create in call request
     *
     * @param pck packet to use
     */
    public void createInReq(packHolder pck) {
        pck.clear();
        pck.msbPutW(0, 0); // reserved
        pck.msbPutW(2, callLoc); // call id
        pck.msbPutW(4, callRem); // call serial
        pck.msbPutD(6, berCap); // bearer capability
        pck.msbPutD(10, chnNum); // channel number
        pck.msbPutW(14, hstNam.length()); // length of called number
        pck.msbPutW(16, vndNam.length()); // length of calling number
        pck.putAsciiZ(18, 64, hstNam, 0); // called number
        pck.putAsciiZ(82, 64, vndNam, 0); // calling number
        pck.putAsciiZ(146, 64, "", 0); // subaddress
        pck.putSkip(210);
    }

    /**
     * fill in call reply
     *
     * @param loc local session
     * @param rem remote session
     */
    public void fillInRep(int loc, int rem) {
        typ = msgInRep;
        callLoc = loc;
        callRem = rem;
        resCod = 1;
        winSiz = 1024;
        prcDel = 0;
    }

    /**
     * parse in call reply
     *
     * @param pck packet to use
     * @return false on success, true on error
     */
    public boolean parseInRep(packHolder pck) {
        if (typ != msgInRep) {
            return true;
        }
        callLoc = pck.msbGetW(2); // call id
        callRem = pck.msbGetW(4); // call id
        resCod = pck.getByte(6); // result code
        errCod = pck.getByte(7); // error code
        winSiz = pck.msbGetW(8); // rx window size
        prcDel = pck.msbGetW(10); // processing delay
        return false;
    }

    /**
     * parse in call reply
     *
     * @param pck packet to use
     */
    public void createInRep(packHolder pck) {
        pck.clear();
        pck.msbPutW(0, 0); // reserved
        pck.msbPutW(2, callLoc); // call id
        pck.msbPutW(4, callRem); // call serial
        pck.putByte(6, resCod); // result code
        pck.putByte(7, errCod); // error code
        pck.msbPutW(8, winSiz); // rx window size
        pck.msbPutW(10, prcDel); // processing delay
        pck.msbPutW(12, 0); // reserved
        pck.putSkip(14);
    }

    /**
     * fill in call connect
     *
     * @param rem remote session
     */
    public void fillInCon(int rem) {
        typ = msgInCon;
        callRem = rem;
        bpsMin = 64000;
        winSiz = 1024;
        prcDel = 0;
        frmCap = 1;
    }

    /**
     * parse in call connect
     *
     * @param pck packet to use
     * @return false on success, true on error
     */
    public boolean parseInCon(packHolder pck) {
        if (typ != msgInCon) {
            return true;
        }
        callRem = pck.msbGetW(2); // call id
        bpsMin = pck.msbGetD(6); // connect speed
        winSiz = pck.msbGetW(10); // rx window size
        prcDel = pck.msbGetW(12); // processing delay
        frmCap = pck.msbGetD(14); // framing capability
        return false;
    }

    /**
     * create in call connect
     *
     * @param pck packet to use
     */
    public void createInCon(packHolder pck) {
        pck.clear();
        pck.msbPutW(0, 0); // reserved
        pck.msbPutW(2, callRem); // call serial
        pck.msbPutW(4, 0); // reserved
        pck.msbPutD(6, bpsMin); // connect speed
        pck.msbPutW(10, winSiz); // rx window size
        pck.msbPutW(12, prcDel); // processing delay
        pck.msbPutD(14, frmCap); // framing capability
        pck.putSkip(18);
    }

    /**
     * fill clear request
     *
     * @param rem remote session
     */
    public void fillClrReq(int rem) {
        typ = msgClrReq;
        callRem = rem;
    }

    /**
     * parse clear request
     *
     * @param pck packet to use
     * @return false on success, true on error
     */
    public boolean parseClrReq(packHolder pck) {
        if (typ != msgClrReq) {
            return true;
        }
        callRem = pck.msbGetW(2); // call id
        return false;
    }

    /**
     * create clear request
     *
     * @param pck packet to use
     */
    public void createClrReq(packHolder pck) {
        pck.clear();
        pck.msbPutW(0, 0); // reserved
        pck.msbPutW(2, callRem); // call serial
        pck.msbPutW(4, 0); // reserved
        pck.putSkip(6);
    }

    /**
     * fill disconnect notify
     *
     * @param rem remote session
     */
    public void fillDscNot(int rem) {
        typ = msgDscNot;
        callRem = rem;
    }

    /**
     * parse disconnect notify
     *
     * @param pck packet to use
     * @return false on success, true on error
     */
    public boolean parseDscNot(packHolder pck) {
        if (typ != msgDscNot) {
            return true;
        }
        callRem = pck.msbGetW(2); // call id
        resCod = pck.getByte(4); // result code
        errCod = pck.getByte(5); // result code
        frmCap = pck.msbGetW(6); // cause code
        hstNam = pck.getAsciiZ(10, 128, 0); // call statistics
        return false;
    }

    /**
     * create disconnect notify
     *
     * @param pck packet to use
     */
    public void createDscNot(packHolder pck) {
        pck.clear();
        pck.msbPutW(0, 0); // reserved
        pck.msbPutW(2, callRem); // call id
        pck.putByte(4, resCod); // result code
        pck.putByte(5, errCod); // result code
        pck.msbPutW(6, frmCap); // cause code
        pck.msbPutW(8, 0); // reserved
        pck.putAsciiZ(10, 128, hstNam, 0); // call statistics
    }

}
