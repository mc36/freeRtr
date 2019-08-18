package ifc;

import auth.autherChap;
import auth.autherDoer;
import auth.autherEap;
import auth.autherPap;
import pack.packHolder;
import util.bits;
import util.state;
import util.typLenVal;

/**
 * line control protocol for ppp
 *
 * @author matecsaba
 */
public class ifcPppLcp extends ifcPppNcp {

    /**
     * ethertype
     */
    public final static int ethTyp = -1;

    /**
     * ppp name
     */
    public final static String pppName = "lcp";

    /**
     * ppp data type
     */
    public final static int pppData = -1;

    /**
     * ppp control type
     */
    public final static int pppCtrl = 0xc021;

    /**
     * Maximum-Receive-Unit
     */
    public final static int optionMRU = 1;

    /**
     * Async-Control-Character-Map
     */
    public final static int optionACCM = 2;

    /**
     * Authentication-Protocol
     */
    public final static int optionAuth = 3;

    /**
     * Quality-Protocol
     */
    public final static int optionQuality = 4;

    /**
     * Magic-Number
     */
    public final static int optionMagic = 5;

    /**
     * Protocol-Field-Compression
     */
    public final static int optionPFC = 7;

    /**
     * Address-and-Control-Field-Compression
     */
    public final static int optionACFC = 8;

    /**
     * local authentication type
     */
    public int authLoc;

    /**
     * remote authentication type
     */
    public int authRem;

    /**
     * echoreqs sent
     */
    private int echoesSent;

    /**
     * last echoreq id
     */
    private int lastEchoId;

    /**
     * get name
     *
     * @return name
     */
    public String getPPPname() {
        return pppName;
    }

    /**
     * get control code
     *
     * @return code
     */
    public int getPPPctrl() {
        return pppCtrl;
    }

    /**
     * get data code
     *
     * @return code
     */
    public int getPPPdata() {
        return pppData;
    }

    /**
     * get ethertype
     *
     * @return ethertype
     */
    public int getPPPetyp() {
        return ethTyp;
    }

    /**
     * create lcp handler
     *
     * @param prnt parent
     */
    public ifcPppLcp(ifcPpp prnt) {
        parent = prnt;
    }

    /**
     * clear state
     */
    public void clearState() {
        echoesSent = 0;
        lastEchoId = -1;
        authLoc = 0;
        authRem = 0;
        clearUpperState();
    }

    /**
     * get one config
     *
     * @param txReq send request
     * @return data
     */
    public Object getOneConfig(boolean txReq) {
        ifcPppLcpConf cfg = new ifcPppLcpConf();
        if (!txReq) {
            return cfg;
        }
        cfg.magic = bits.randomD();
        if (parent.authenRem != null) {
            cfg.auth = findAuthen(autherEap.pppCtrl, false);
        }
        cfg.mru = parent.sentMru;
        cfg.accm = parent.sentAccm;
        return cfg;
    }

    /**
     * get next authentication method
     *
     * @param i current value
     * @return next value
     */
    public static int nextAuthen(int i) {
        switch (i) {
            case autherEap.pppCtrl:
                return autherChap.pppCtrl;
            case autherChap.pppCtrl:
                return autherPap.pppCtrl;
            case autherPap.pppCtrl:
                return autherEap.pppCtrl;
            default:
                return -1;
        }
    }

    /**
     * check if authentication method allowed
     *
     * @param i current value
     * @return false if allowed, true if forbidden
     */
    public boolean forbiddenAuthen(int i) {
        switch (i) {
            case autherEap.pppCtrl:
                return parent.refuseEap;
            case autherChap.pppCtrl:
                return parent.refuseChap;
            case autherPap.pppCtrl:
                return parent.refusePap;
            default:
                return true;
        }
    }

    /**
     * find alllowed authentication method
     *
     * @param i current value
     * @param inc first get a next one
     * @return next value
     */
    public int findAuthen(int i, boolean inc) {
        if (inc) {
            i = nextAuthen(i);
        }
        for (int o = 0; o < 16; o++) {
            if (!forbiddenAuthen(i)) {
                return i;
            }
            i = nextAuthen(i);
        }
        return -1;
    }

    /**
     * read options
     *
     * @param config config
     * @param tlv tlv
     * @return false on success, true on error
     */
    public boolean readOption(Object config, typLenVal tlv) {
        ifcPppLcpConf cfg = (ifcPppLcpConf) config;
        switch (tlv.valTyp) {
            case optionMRU:
                cfg.mru = bits.msbGetW(tlv.valDat, 0);
                return false;
            case optionACCM:
                cfg.accm = bits.msbGetD(tlv.valDat, 0);
                return false;
            case optionAuth:
                cfg.auth = bits.msbGetW(tlv.valDat, 0);
                if (nextAuthen(cfg.auth) <= 0) {
                    return true;
                }
                return false;
            case optionMagic:
                cfg.magic = bits.msbGetD(tlv.valDat, 0);
                return false;
            case optionQuality:
                cfg.quality = bits.msbGetW(tlv.valDat, 0);
                return false;
            case optionPFC:
                cfg.pfc = true;
                return false;
            case optionACFC:
                cfg.acfc = true;
                return false;
            default:
                return true;
        }
    }

    /**
     * write options
     *
     * @param pck packet
     * @param data data
     */
    public void writeOptions(packHolder pck, Object data) {
        ifcPppLcpConf dat = (ifcPppLcpConf) data;
        typLenVal tlv = getTlv();
        byte[] buf = new byte[128];
        if (dat.mru > 0) {
            bits.msbPutW(buf, 0, dat.mru);
            tlv.putBytes(pck, optionMRU, 2, buf);
        }
        if (dat.accm != 0) {
            bits.msbPutD(buf, 0, dat.accm);
            tlv.putBytes(pck, optionACCM, 4, buf);
        }
        if (dat.auth > 0) {
            bits.msbPutW(buf, 0, dat.auth);
            int i = 2;
            if (dat.auth == autherChap.pppCtrl) {
                buf[2] = 5;
                i = 3;
            }
            tlv.putBytes(pck, optionAuth, i, buf);
        }
        if (dat.magic != 0) {
            bits.msbPutD(buf, 0, dat.magic);
            tlv.putBytes(pck, optionMagic, 4, buf);
        }
        if (dat.quality > 0) {
            bits.msbPutW(buf, 0, dat.quality);
            tlv.putBytes(pck, optionQuality, 2, buf);
        }
        if (dat.pfc) {
            tlv.putBytes(pck, optionPFC, 0, buf);
        }
        if (dat.acfc) {
            tlv.putBytes(pck, optionACFC, 0, buf);
        }
    }

    /**
     * send echo request
     */
    public void sendEchoReq() {
        if (echoesSent++ > 10) {
            clearState();
            parent.checkPeerState(state.states.up);
            return;
        }
        ifcPppLcpConf cfg = (ifcPppLcpConf) txReq;
        packHolder pck = new packHolder(true, true);
        pck.msbPutD(0, cfg.magic);
        lastEchoId = bits.randomD();
        pck.msbPutD(4, lastEchoId);
        pck.putSkip(8);
        lastEchoId &= 0xff;
        parent.sendNcpCtrl(pck, pppCtrl, codeEchoReq, lastEchoId);
    }

    /**
     * get configure ack
     *
     * @param data data
     * @return false on success, true on error
     */
    public boolean gotConfAck(Object data) {
        ifcPppLcpConf dat = (ifcPppLcpConf) data;
        authRem = dat.auth;
        return false;
    }

    /**
     * get configure request
     *
     * @param data data
     * @param rej rejected
     * @return data
     */
    public Object gotConfReq(Object data, boolean rej) {
        boolean val = false;
        ifcPppLcpConf dat = (ifcPppLcpConf) data;
        ifcPppLcpConf res = new ifcPppLcpConf();
        ifcPppLcpConf cfg = (ifcPppLcpConf) txReq;
        if (dat.acfc) {
            res.acfc = true;
            val = true;
        }
        if (dat.pfc) {
            res.pfc = true;
            val = true;
        }
        if (rej) {
            if (val) {
                return res;
            } else {
                return null;
            }
        }
        authLoc = dat.auth;
        if ((dat.magic == cfg.magic) && (cfg.magic != 0)) {
            res.magic = dat.magic;
            val = true;
        }
        if (dat.auth > 0) {
            if (parent.sentUser == null) {
                res.auth = dat.auth;
                val = true;
            }
            if (forbiddenAuthen(dat.auth)) {
                res.auth = findAuthen(dat.auth, true);
                val = true;
            }
        }
        if (dat.quality > 0) {
            res.quality = dat.quality;
            val = true;
        }
        if (val) {
            return res;
        } else {
            return null;
        }
    }

    /**
     * get configure nak
     *
     * @param data data
     */
    public void gotConfNak(Object data) {
        ifcPppLcpConf dat = (ifcPppLcpConf) data;
        ifcPppLcpConf cfg = (ifcPppLcpConf) txReq;
        if (dat.quality > 0) {
            cfg.quality = 0;
        }
        if (dat.auth > 0) {
            cfg.auth = findAuthen(dat.auth, false);
        }
        if (dat.mru > 0) {
            cfg.mru = dat.mru;
        }
        if (dat.accm != 0) {
            cfg.accm = dat.accm;
        }
    }

    /**
     * get configure reject
     *
     * @param data data
     */
    public void gotConfRej(Object data) {
        ifcPppLcpConf dat = (ifcPppLcpConf) data;
        ifcPppLcpConf cfg = (ifcPppLcpConf) txReq;
        if (dat.acfc) {
            cfg.acfc = false;
        }
        if (dat.pfc) {
            cfg.pfc = false;
        }
        if (dat.accm != 0) {
            cfg.accm = 0;
        }
        if (dat.magic != 0) {
            cfg.magic = 0;
        }
        if (dat.auth > 0) {
            cfg.auth = findAuthen(cfg.auth, true);
        }
        if (dat.mru > 0) {
            cfg.mru = 0;
        }
        if (dat.quality > 0) {
            cfg.quality = 0;
        }
    }

    /**
     * get unknown code
     *
     * @param pck packet
     * @param code code
     * @param id id
     * @return false on success, true on error
     */
    public boolean gotUnknownCode(packHolder pck, int code, int id) {
        ifcPppLcpConf cfg = (ifcPppLcpConf) txReq;
        switch (code) {
            case codeCodeRej:
                break;
            case codeProtRej:
                break;
            case codeEchoReq:
                if (id == lastEchoId) {
                    return false;
                }
                pck.getSkip(4);
                pck.msbPutD(0, cfg.magic);
                pck.putSkip(4);
                parent.sendNcpCtrl(pck, pppCtrl, codeEchoRep, id);
                echoesSent = 0;
                break;
            case codeEchoRep:
                if (id != lastEchoId) {
                    return false;
                }
                echoesSent = 0;
                lastEchoId = -1;
                break;
            case codeDiscReq:
                break;
            default:
                return true;
        }
        return false;
    }

}

class ifcPppLcpConf {

    public int mru = -1;

    public int auth = -1;

    public int quality = -1;

    public int magic = 0;

    public boolean pfc = false;

    public boolean acfc = false;

    public int accm = 0;

    public String toString() {
        return "magic=" + magic + " mru=" + mru + " accm=" + accm + " auth=" + autherDoer.getName(auth) + " quality="
                + quality + " pfc=" + pfc + " acfc=" + acfc;
    }

}
