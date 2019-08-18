package ifc;

import addr.addrMac;
import pack.packHolder;
import util.bits;
import util.typLenVal;

/**
 * bridging control protocol (rfc3518) for ppp
 *
 * @author matecsaba
 */
public class ifcPppBrdg extends ifcPppNcp {

    /**
     * ethertype
     */
    public final static int ethTyp = ifcBridge.serialType;

    /**
     * ppp name
     */
    public final static String pppName = "bcp";

    /**
     * ppp data type
     */
    public final static int pppData = 0x0031;

    /**
     * ppp control type
     */
    public final static int pppCtrl = 0x8031;

    /**
     * Bridge-Identification
     */
    public final static int optionBriId = 1;

    /**
     * Line-Identification
     */
    public final static int optionLinId = 2;

    /**
     * MAC-Support
     */
    public final static int optionMacSup = 3;

    /**
     * Tinygram-Compression
     */
    public final static int optionTinCmp = 4;

    /**
     * LAN-Identification
     */
    public final static int optionLanId = 5;

    /**
     * MAC-Address
     */
    public final static int optionMacAdr = 6;

    /**
     * Spanning-Tree-Protocol
     */
    public final static int optionSpnTre = 7;

    /**
     * IEEE 802 Tagged Frame
     */
    public final static int optionTagd = 8;

    /**
     * Management Inline spanning-tree new format
     */
    public final static int optionMgmt = 9;

    /**
     * Bridge Control Packet Indicator bpdu enabled
     */
    public final static int optionCtrPkt = 10;

    /**
     * update received packet
     *
     * @param pck
     */
    public static void patchPackRx(packHolder pck) {
        pck.getSkip(2);
    }

    /**
     * update transmitted packet
     *
     * @param pck
     */
    public static void patchPackTx(packHolder pck) {
        pck.msbPutW(0, 0x0001);
        pck.putSkip(2);
        pck.merge2beg();
    }

    /**
     * the constructor
     *
     * @param prnt parent
     */
    public ifcPppBrdg(ifcPpp prnt) {
        parent = prnt;
    }

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
     * clear state
     */
    public void clearState() {
        clearUpperState();
    }

    /**
     * got config request
     *
     * @param txReq send reply
     * @return decoded config
     */
    public Object getOneConfig(boolean txReq) {
        ifcPppBrdgConf cfg = new ifcPppBrdgConf();
        if (!txReq) {
            return cfg;
        }
        cfg.tagged = true;
        cfg.bridgeCtrl = true;
        cfg.management = true;
        return cfg;
    }

    /**
     * read options
     *
     * @param config configuration
     * @param tlv tlv
     * @return false on success, true on error
     */
    public boolean readOption(Object config, typLenVal tlv) {
        ifcPppBrdgConf cfg = (ifcPppBrdgConf) config;
        switch (tlv.valTyp) {
            case optionBriId:
                cfg.bridgeId = bits.msbGetW(tlv.valDat, 0);
                return false;
            case optionLinId:
                cfg.lineId = bits.msbGetW(tlv.valDat, 0);
                return false;
            case optionMacSup:
                cfg.macType = bits.getByte(tlv.valDat, 0);
                return false;
            case optionTinCmp:
                cfg.compress = (tlv.valDat[0] == 1);
                return false;
            case optionLanId:
                return false;
            case optionMacAdr:
                cfg.macAddr = new addrMac();
                cfg.macAddr.fromBuf(tlv.valDat, 0);
                return false;
            case optionSpnTre:
                cfg.spanTree = bits.getByte(tlv.valDat, 0);
                return false;
            case optionTagd:
                cfg.tagged = (tlv.valDat[0] == 1);
                return false;
            case optionMgmt:
                cfg.management = true;
                return false;
            case optionCtrPkt:
                cfg.bridgeCtrl = true;
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
        ifcPppBrdgConf dat = (ifcPppBrdgConf) data;
        typLenVal tlv = getTlv();
        byte[] buf = new byte[128];
        if (dat.bridgeId > 0) {
            bits.msbPutW(buf, 0, dat.bridgeId);
            tlv.putBytes(pck, optionBriId, 2, buf);
        }
        if (dat.lineId > 0) {
            bits.msbPutW(buf, 0, dat.lineId);
            tlv.putBytes(pck, optionLinId, 2, buf);
        }
        if (dat.macType > 0) {
            bits.putByte(buf, 0, dat.macType);
            tlv.putBytes(pck, optionMacSup, 1, buf);
        }
        if (dat.compress) {
            buf[0] = 1;
            tlv.putBytes(pck, optionTinCmp, 1, buf);
        }
        if (dat.macAddr != null) {
            dat.macAddr.toBuffer(buf, 0);
            tlv.putBytes(pck, optionMacAdr, addrMac.size, buf);
        }
        if (dat.spanTree > 0) {
            bits.putByte(buf, 0, dat.spanTree);
            tlv.putBytes(pck, optionSpnTre, 1, buf);
        }
        if (dat.tagged) {
            buf[0] = 1;
            tlv.putBytes(pck, optionTagd, 1, buf);
        }
        if (dat.management) {
            tlv.putBytes(pck, optionMgmt, 0, buf);
        }
        if (dat.bridgeCtrl) {
            tlv.putBytes(pck, optionCtrPkt, 0, buf);
        }
    }

    /**
     * got configure ack
     *
     * @param data data
     * @return false on success, true on error
     */
    public boolean gotConfAck(Object data) {
        return false;
    }

    /**
     * got configure request
     *
     * @param data data
     * @param rej rejected
     * @return false on success, true on error
     */
    public Object gotConfReq(Object data, boolean rej) {
        ifcPppBrdgConf dat = (ifcPppBrdgConf) data;
        ifcPppBrdgConf res = new ifcPppBrdgConf();
        if (dat.compress) {
            res.compress = true;
            return res;
        }
        if (rej) {
            return null;
        }
        return null;
    }

    /**
     * got configure nak
     *
     * @param data data
     */
    public void gotConfNak(Object data) {
        ifcPppBrdgConf dat = (ifcPppBrdgConf) data;
        ifcPppBrdgConf cfg = (ifcPppBrdgConf) txReq;
        if (dat.compress) {
            cfg.compress = false;
        }
        if (dat.bridgeCtrl) {
            cfg.bridgeCtrl = false;
        }
        if (dat.management) {
            cfg.management = false;
        }
        if (dat.tagged) {
            cfg.tagged = false;
        }
        if (dat.bridgeId > 0) {
            cfg.bridgeId = 0;
        }
        if (dat.lineId > 0) {
            cfg.lineId = 0;
        }
        if (dat.macAddr != null) {
            cfg.macAddr = null;
        }
        if (dat.macType > 0) {
            cfg.macType = 0;
        }
        if (dat.spanTree > 0) {
            cfg.spanTree = 0;
        }
    }

    /**
     * got configure reject
     *
     * @param data data
     */
    public void gotConfRej(Object data) {
        gotConfNak(data);
    }

    /**
     * got unknown code
     *
     * @param pck packet
     * @param code code
     * @param id id
     * @return false on success, true on error
     */
    public boolean gotUnknownCode(packHolder pck, int code, int id) {
        return true;
    }

}

class ifcPppBrdgConf {

    public int bridgeId = -1;

    public int lineId = -1;

    public int macType = -1;

    public boolean compress;

    public addrMac macAddr;

    public int spanTree = -1;

    public boolean tagged;

    public boolean management;

    public boolean bridgeCtrl;

    public String toString() {
        return "brid=" + bridgeId + " linid=" + lineId + " macTyp=" + macType + " compress=" + compress + " addr=" + macAddr
                + " span=" + spanTree + " tag=" + tagged + " mgmt=" + management + " brctr=" + bridgeCtrl;
    }

}
