package pack;

/**
 * layer two tunneling protocol v2 (rfc2661) packet
 *
 * @author matecsaba
 */
public class packL2tp2 extends packL2tp {

    /**
     * port number
     */
    public static final int port = 1701;

    /**
     * control message
     */
    public boolean ctrl;

    /**
     * tunnel id value
     */
    public int tunID;

    /**
     * session id value
     */
    public int sesID;

    /**
     * receive sequence number
     */
    public int seqRx = -1;

    /**
     * transmit sequence number
     */
    public int seqTx = -1;

    /**
     * parse header
     *
     * @param pck packet
     * @return false on success, true on error
     */
    public boolean parseHeader(packHolder pck) {
        int flags = pck.msbGetW(0);
        ctrl = (flags & 0x8000) != 0;
        pck.getSkip(2);
        if ((flags & 0xf) != 2) {
            return true;
        }
        if ((flags & 0x4000) != 0) {
            int i = pck.msbGetW(0) - 4;
            pck.getSkip(2);
            if (pck.dataSize() < i) {
                return true;
            }
            pck.setDataSize(i);
        }
        tunID = pck.msbGetW(0);
        sesID = pck.msbGetW(2);
        pck.getSkip(4);
        if ((flags & 0x800) != 0) {
            seqTx = pck.msbGetW(0);
            seqRx = pck.msbGetW(2);
            pck.getSkip(4);
        }
        if ((flags & 0x200) != 0) {
            int i = pck.msbGetW(0);
            pck.getSkip(i + 2);
        }
        return false;
    }

    /**
     * create header
     *
     * @param pck packet
     */
    public void createHeader(packHolder pck) {
        int flags = 2;
        if (ctrl) {
            flags |= 0xc000;
        }
        if ((seqTx >= 0) || (seqRx >= 0)) {
            flags |= 0x800;
        }
        pck.msbPutW(0, flags);
        pck.putSkip(2);
        if ((flags & 0x4000) != 0) {
            pck.msbPutW(0, 0);
            pck.putSkip(2);
        }
        pck.msbPutW(0, tunID);
        pck.msbPutW(2, sesID);
        pck.putSkip(4);
        if ((flags & 0x800) != 0) {
            pck.msbPutW(0, seqTx);
            pck.msbPutW(2, seqRx);
            pck.putSkip(4);
        }
        if ((flags & 0x4000) != 0) {
            int i = pck.headSize();
            pck.msbPutW(2 - i, pck.dataSize() + i);
        }
        pck.merge2beg();
    }

    /**
     * dump header
     *
     * @return packet
     */
    public String dumpHeader() {
        return "tun=" + tunID + " ses=" + sesID + " rx=" + seqRx + " tx=" + seqTx;
    }

    /**
     * patch header for control packet
     *
     * @param tun tunnel id
     * @param rx rx sequence
     * @param tx tx sequence
     */
    public void patchHeader(int tun, int rx, int tx) {
        ctrl = true;
        seqRx = rx;
        seqTx = tx;
        tunID = tun;
    }

    /**
     * create hello packet
     *
     * @return packet created
     */
    public static packL2tp2 createHELLO() {
        packL2tp2 pck = new packL2tp2();
        pck.valMsgTyp = typHELLO;
        return pck;
    }

    /**
     * create cdn packet
     *
     * @param rem remote session id
     * @param loc local session id
     * @return packet created
     */
    public static packL2tp2 createCDN(int rem, int loc) {
        packL2tp2 pck = new packL2tp2();
        pck.valMsgTyp = typCDN;
        pck.sesID = rem;
        pck.valResCod = 1;
        pck.valSesId = loc;
        return pck;
    }

    /**
     * create sccrq packet
     *
     * @param tun tunnel id
     * @param host hostname
     * @param chal challenge
     * @return packet created
     */
    public static packL2tp2 createSCCRQ(int tun, String host, byte[] chal) {
        packL2tp2 pck = new packL2tp2();
        pck.valMsgTyp = typSCCRQ;
        pck.valPrtVer = 0x100;
        pck.valFramCap = 3;
        pck.valBearCap = 3;
        pck.valHstNam = host;
        pck.valChallen = chal;
        pck.valTunId = tun;
        pck.valWinSiz = 1024;
        return pck;
    }

    /**
     * create sccrp packet
     *
     * @param tun tunnel id
     * @param host hostname
     * @param chal challenge
     * @param resp response
     * @return packet created
     */
    public static packL2tp2 createSCCRP(int tun, String host, byte[] chal, byte[] resp) {
        packL2tp2 pck = new packL2tp2();
        pck.valMsgTyp = typSCCRP;
        pck.valPrtVer = 0x100;
        pck.valFramCap = 3;
        pck.valBearCap = 3;
        pck.valHstNam = host;
        pck.valChallen = chal;
        pck.valResponse = resp;
        pck.valTunId = tun;
        pck.valWinSiz = 1024;
        return pck;
    }

    /**
     * create scccn packet
     *
     * @param resp response
     * @return packet created
     */
    public static packL2tp2 createSCCCN(byte[] resp) {
        packL2tp2 pck = new packL2tp2();
        pck.valMsgTyp = typSCCCN;
        pck.valResponse = resp;
        return pck;
    }

    /**
     * create ocrq packet
     *
     * @param loc local session
     * @param called called number
     * @param calling calling number
     * @return created packet
     */
    public static packL2tp2 createOCRQ(int loc, String called, String calling) {
        packL2tp2 pck = new packL2tp2();
        pck.valMsgTyp = typOCRQ;
        pck.valSesId = loc;
        pck.valCallSer = 1;
        pck.valMinBps = 1;
        pck.valMaxBps = 1000000000;
        pck.valBearTyp = 1;
        pck.valFramTyp = 1;
        pck.valCalled = called;
        pck.valCalling = calling;
        return pck;
    }

    /**
     * create ocrp packet
     *
     * @param loc local session
     * @param rem remote session
     * @return created packet
     */
    public static packL2tp2 createOCRP(int loc, int rem) {
        packL2tp2 pck = new packL2tp2();
        pck.sesID = rem;
        pck.valMsgTyp = typOCRP;
        pck.valSesId = loc;
        return pck;
    }

    /**
     * create occn packet
     *
     * @param rem remote session
     * @return created packet
     */
    public static packL2tp2 createOCCN(int rem) {
        packL2tp2 pck = new packL2tp2();
        pck.sesID = rem;
        pck.valMsgTyp = typOCCN;
        pck.valTxSpeed = 64000;
        pck.valRxSpeed = pck.valTxSpeed;
        pck.valFramTyp = 1;
        return pck;
    }

    /**
     * create ocrq packet
     *
     * @param loc local session
     * @param called called number
     * @param calling calling number
     * @return created packet
     */
    public static packL2tp2 createICRQ(int loc, String called, String calling) {
        packL2tp2 pck = new packL2tp2();
        pck.valMsgTyp = typICRQ;
        pck.valSesId = loc;
        pck.valCallSer = 1;
        pck.valBearTyp = 1;
        pck.valCalled = called;
        pck.valCalling = calling;
        return pck;
    }

    /**
     * create icrp packet
     *
     * @param loc local session
     * @param rem remote session
     * @return created packet
     */
    public static packL2tp2 createICRP(int loc, int rem) {
        packL2tp2 pck = new packL2tp2();
        pck.sesID = rem;
        pck.valMsgTyp = typICRP;
        pck.valSesId = loc;
        return pck;
    }

    /**
     * create iccn packet
     *
     * @param rem remote session
     * @return created packet
     */
    public static packL2tp2 createICCN(int rem) {
        packL2tp2 pck = new packL2tp2();
        pck.sesID = rem;
        pck.valMsgTyp = typICCN;
        pck.valTxSpeed = 64000;
        pck.valRxSpeed = pck.valTxSpeed;
        pck.valFramTyp = 1;
        return pck;
    }

}
