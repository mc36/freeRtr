package pack;

import addr.addrIPv4;

/**
 * layer two tunneling protocol v3 (rfc3931) packet
 *
 * @author matecsaba
 */
public class packL2tp3 extends packL2tp {

    /**
     * protocol number
     */
    public static final int prot = 115;

    /**
     * control message
     */
    public boolean ctrl;

    /**
     * connection id
     */
    public int conID = -1;

    /**
     * session id
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
     * cookie size
     */
    public int cookSiz = -1;

    /**
     * cookie value
     */
    public long cookVal = -1;

    /**
     * parse header
     *
     * @param pck packet
     * @return false on success, true on error
     */
    public boolean parseHeader(packHolder pck) {
        sesID = pck.msbGetD(0);
        pck.getSkip(4);
        ctrl = sesID == 0;
        if (!ctrl) {
            switch (cookSiz) {
                case 4:
                    cookVal = pck.msbGetD(0);
                    pck.getSkip(4);
                    break;
                case 8:
                    cookVal = pck.msbGetQ(0);
                    pck.getSkip(8);
                    break;
            }
            if ((seqTx >= 0) || (seqRx >= 0)) {
                seqRx = pck.msbGetD(0) & 0xffffff;
                pck.getSkip(4);
            }
            return false;
        }
        int flags = pck.msbGetW(0);
        pck.getSkip(2);
        if ((flags & 0xf) != 3) {
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
        conID = pck.msbGetD(0);
        pck.getSkip(4);
        if ((flags & 0x800) != 0) {
            seqTx = pck.msbGetW(0);
            seqRx = pck.msbGetW(2);
            pck.getSkip(4);
        }
        return false;
    }

    /**
     * create header
     *
     * @param pck packet
     */
    public void createHeader(packHolder pck) {
        pck.msbPutD(0, sesID);
        pck.putSkip(4);
        if (!ctrl) {
            switch (cookSiz) {
                case 4:
                    pck.msbPutD(0, (int) cookVal);
                    pck.putSkip(4);
                    break;
                case 8:
                    pck.msbPutQ(0, cookVal);
                    pck.putSkip(8);
                    break;
            }
            if ((seqTx >= 0) || (seqRx >= 0)) {
                pck.msbPutD(0, seqTx);
                pck.putByte(0, 0x40);
                pck.putSkip(4);
            }
            pck.merge2beg();
            return;
        }
        int flags = 0xc003;
        if ((seqTx >= 0) || (seqRx >= 0)) {
            flags |= 0x800;
        }
        pck.msbPutW(0, flags);
        pck.putSkip(2);
        if ((flags & 0x4000) != 0) {
            pck.msbPutW(0, 0);
            pck.putSkip(2);
        }
        pck.msbPutD(0, conID);
        pck.putSkip(4);
        if ((flags & 0x800) != 0) {
            pck.msbPutW(0, seqTx);
            pck.msbPutW(2, seqRx);
            pck.putSkip(4);
        }
        if ((flags & 0x4000) != 0) {
            int i = pck.headSize();
            pck.msbPutW(6 - i, pck.dataSize() + i - 4);
        }
        pck.merge2beg();
    }

    /**
     * dump header
     *
     * @return string
     */
    public String dumpHeader() {
        return "con=" + conID + " ses=" + sesID + " rx=" + seqRx + " tx=" + seqTx;
    }

    /**
     * patch header for control packet
     *
     * @param con connection id
     * @param rx rx sequence
     * @param tx tx sequence
     */
    public void patchHeader(int con, int rx, int tx) {
        ctrl = true;
        seqRx = rx;
        seqTx = tx;
        conID = con;
    }

    /**
     * create hello packet
     *
     * @return packet created
     */
    public static packL2tp3 createHELLO() {
        packL2tp3 pck = new packL2tp3();
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
    public static packL2tp3 createCDN(int rem, int loc) {
        packL2tp3 pck = new packL2tp3();
        pck.valMsgTyp = typCDN;
        pck.valLocSesId = loc;
        pck.valRemSesId = rem;
        pck.valResCod = 1;
        return pck;
    }

    /**
     * create sli packet
     *
     * @param rem remote session id
     * @param loc local session id
     * @return packet created
     */
    public static packL2tp3 createSLI(int rem, int loc) {
        packL2tp3 pck = new packL2tp3();
        pck.valMsgTyp = typSLI;
        pck.valLocSesId = loc;
        pck.valRemSesId = rem;
        pck.valCircStat = 1;
        return pck;
    }

    /**
     * create sccrq packet
     *
     * @param con connection id
     * @param rtr router id
     * @param host hostname
     * @param tie tie breaker
     * @param chal challenge
     * @return packet created
     */
    public static packL2tp3 createSCCRQ(int con, addrIPv4 rtr, String host, long tie, byte[] chal) {
        packL2tp3 pck = new packL2tp3();
        pck.valMsgTyp = typSCCRQ;
        pck.valPrtVer = 0x100;
        pck.valFramCap = 3;
        pck.valBearCap = 3;
        pck.valHstNam = host;
        pck.valChallen = chal;
        pck.valTieBrk = tie;
        pck.valConnId = con;
        pck.valRtrID = rtr.copyBytes();
        pck.valWinSiz = 1024;
        pck.valPwCap = new int[4];
        pck.valPwCap[0] = packLdpPwe.pwtEthPort;
        pck.valPwCap[1] = packLdpPwe.pwtEthVlan;
        pck.valPwCap[2] = packLdpPwe.pwtHdlc;
        pck.valPwCap[3] = packLdpPwe.pwtPpp;
        return pck;
    }

    /**
     * create sccrp packet
     *
     * @param con connection id
     * @param rtr router id
     * @param host hostname
     * @param chal challenge
     * @param resp response
     * @return packet created
     */
    public static packL2tp3 createSCCRP(int con, addrIPv4 rtr, String host, byte[] chal, byte[] resp) {
        packL2tp3 pck = new packL2tp3();
        pck.valMsgTyp = typSCCRP;
        pck.valPrtVer = 0x100;
        pck.valFramCap = 3;
        pck.valBearCap = 3;
        pck.valHstNam = host;
        pck.valChallen = chal;
        pck.valResponse = resp;
        pck.valConnId = con;
        pck.valRtrID = rtr.copyBytes();
        pck.valWinSiz = 1024;
        pck.valPwCap = new int[4];
        pck.valPwCap[0] = packLdpPwe.pwtEthPort;
        pck.valPwCap[1] = packLdpPwe.pwtEthVlan;
        pck.valPwCap[2] = packLdpPwe.pwtHdlc;
        pck.valPwCap[3] = packLdpPwe.pwtPpp;
        return pck;
    }

    /**
     * create scccn packet
     *
     * @param resp response
     * @return packet created
     */
    public static packL2tp3 createSCCCN(byte[] resp) {
        packL2tp3 pck = new packL2tp3();
        pck.valMsgTyp = typSCCCN;
        pck.valResponse = resp;
        return pck;
    }

    /**
     * create ocrq packet
     *
     * @param loc local session
     * @param vcid vc id
     * @param pwt pw type
     * @param tie tie breaker
     * @return created packet
     */
    public static packL2tp3 createICRQ(int loc, String vcid, int pwt, long tie) {
        packL2tp3 pck = new packL2tp3();
        pck.valMsgTyp = typICRQ;
        pck.valLocSesId = loc;
        pck.valRemEndId = vcid;
        pck.valPwTyp = pwt;
        pck.valTieBrk = tie;
        pck.valRemSesId = 0;
        pck.valCallSer = 1;
        pck.valCircStat = 1;
        return pck;
    }

    /**
     * create icrp packet
     *
     * @param rem remote session
     * @param loc local session
     * @param vcid vc id
     * @param pwt pw type
     * @return created packet
     */
    public static packL2tp3 createICRP(int rem, int loc, String vcid, int pwt) {
        packL2tp3 pck = new packL2tp3();
        pck.valMsgTyp = typICRP;
        pck.valLocSesId = loc;
        pck.valRemSesId = rem;
        pck.valRemEndId = vcid;
        pck.valPwTyp = pwt;
        pck.valCallSer = 1;
        pck.valCircStat = 1;
        return pck;
    }

    /**
     * create sli packet
     *
     * @param rem remote session id
     * @param loc local session id
     * @return packet created
     */
    public static packL2tp3 createICCN(int rem, int loc) {
        packL2tp3 pck = new packL2tp3();
        pck.valMsgTyp = typICCN;
        pck.valLocSesId = loc;
        pck.valRemSesId = rem;
        pck.valTxSpeed = 64000;
        pck.valRxSpeed = pck.valTxSpeed;
        pck.valCircStat = 1;
        return pck;
    }

}
