package ifc;

import addr.addrEui;
import ip.ipIfc6;
import pack.packHolder;
import util.bits;
import util.typLenVal;

/**
 * ipv4 control protocol (rfc2472) for ppp
 *
 * @author matecsaba
 */
public class ifcPppIp6 extends ifcPppNcp {

    /**
     * my address, null=negotiated
     */
    public addrEui locAddrCur;

    /**
     * peer address, null=negotiated
     */
    public addrEui remAddrCur;

    /**
     * ethertype
     */
    public final static int ethTyp = ipIfc6.type;

    /**
     * ppp name
     */
    public final static String pppName = "ipv6cp";

    /**
     * ppp data type
     */
    public final static int pppData = 0x0057;

    /**
     * ppp control type
     */
    public final static int pppCtrl = 0x8057;

    /**
     * Interface-Identifier
     */
    public final static int optionIntId = 1;

    /**
     * IPv6-Compression-Protocol
     */
    public final static int optionCompress = 2;

    /**
     * the constructor
     *
     * @param prnt parent
     */
    public ifcPppIp6(ifcPpp prnt) {
        parent = prnt;
    }

    public String getPPPname() {
        return pppName;
    }

    public int getPPPctrl() {
        return pppCtrl;
    }

    public int getPPPdata() {
        return pppData;
    }

    public int getPPPetyp() {
        return ethTyp;
    }

    public void clearState() {
        if (parent.locIfIdCfg != null) {
            locAddrCur = parent.locIfIdCfg.copyBytes();
        } else {
            locAddrCur = addrEui.getRandom();
        }
        remAddrCur = new addrEui();
        clearUpperState();
    }

    public Object getOneConfig(boolean txReq) {
        ifcPppIp6conf cfg = new ifcPppIp6conf();
        cfg.ifid = null;
        if (!txReq) {
            return cfg;
        }
        cfg.ifid = locAddrCur.copyBytes();
        return cfg;
    }

    public boolean readOption(Object config, typLenVal tlv) {
        ifcPppIp6conf cfg = (ifcPppIp6conf) config;
        switch (tlv.valTyp) {
            case optionIntId:
                cfg.ifid = new addrEui();
                cfg.ifid.fromBuf(tlv.valDat, 0);
                return false;
            case optionCompress:
                cfg.compress = bits.msbGetW(tlv.valDat, 0);
                return false;
            default:
                return true;
        }
    }

    public void writeOptions(packHolder pck, Object data) {
        ifcPppIp6conf dat = (ifcPppIp6conf) data;
        typLenVal tlv = getTlv();
        byte[] buf = new byte[128];
        if (dat.compress > 0) {
            bits.msbPutW(buf, 0, dat.compress);
            tlv.putBytes(pck, optionCompress, 2, buf);
        }
        if (dat.ifid != null) {
            dat.ifid.toBuffer(buf, 0);
            tlv.putBytes(pck, optionIntId, addrEui.size, buf);
        }
    }

    public boolean gotConfAck(Object data) {
        return false;
    }

    public Object gotConfReq(Object data, boolean rej) {
        ifcPppIp6conf dat = (ifcPppIp6conf) data;
        ifcPppIp6conf res = new ifcPppIp6conf();
        if (dat.compress > 0) {
            res.compress = dat.compress;
            return res;
        }
        if (rej) {
            return null;
        }
        if (dat.ifid != null) {
            remAddrCur = dat.ifid;
        }
        return null;
    }

    public void gotConfNak(Object data) {
        ifcPppIp6conf dat = (ifcPppIp6conf) data;
        ifcPppIp6conf cfg = (ifcPppIp6conf) txReq;
        if (dat.compress > 0) {
            cfg.compress = 0;
        }
    }

    public void gotConfRej(Object data) {
        gotConfNak(data);
    }

    public boolean gotUnknownCode(packHolder pck, int code, int id) {
        return true;
    }

}

class ifcPppIp6conf {

    public int compress = -1;

    public addrEui ifid = new addrEui();

    public String toString() {
        return "ifid=" + ifid + " compress=" + compress;
    }

}
