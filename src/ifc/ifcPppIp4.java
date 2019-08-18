package ifc;

import addr.addrIPv4;
import ip.ipIfc4;
import pack.packHolder;
import util.bits;
import util.typLenVal;

/**
 * ipv4 control protocol (rfc1332) for ppp
 *
 * @author matecsaba
 */
public class ifcPppIp4 extends ifcPppNcp {

    /**
     * ethertype
     */
    public final static int ethTyp = ipIfc4.type;

    /**
     * ppp name
     */
    public final static String pppName = "ipv4cp";

    /**
     * ppp data type
     */
    public final static int pppData = 0x0021;

    /**
     * ppp control type
     */
    public final static int pppCtrl = 0x8021;

    /**
     * my address, null=negotiated
     */
    public addrIPv4 locAddrCur;

    /**
     * peer address, null=negotiated
     */
    public addrIPv4 remAddrCur;

    /**
     * dns1 address
     */
    public addrIPv4 dns1addrCur;

    /**
     * dns2 address
     */
    public addrIPv4 dns2addrCur;

    /**
     * local address required
     */
    public boolean locAddrReq;

    /**
     * IP-Addresses
     */
    public final static int optionAddres = 1;

    /**
     * IP-Compression-Protocol
     */
    public final static int optionCompress = 2;

    /**
     * IP-Address
     */
    public final static int optionAddr = 3;

    /**
     * Mobile-Node-Home-Address
     */
    public final static int optionMobil = 4;

    /**
     * Primary-DNS-Address
     */
    public final static int optionDns1 = 129;

    /**
     * Secondary-DNS-Address
     */
    public final static int optionDns2 = 131;

    /**
     * Primary-NBNS-Address
     */
    public final static int optionNbns1 = 130;

    /**
     * Secondary-NBNS-Address
     */
    public final static int optionNbns2 = 132;

    /**
     * the constructor
     *
     * @param prnt parent
     */
    public ifcPppIp4(ifcPpp prnt) {
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
        if (parent.locAddrCfg == null) {
            locAddrCur = null;
        } else {
            locAddrCur = parent.locAddrCfg.copyBytes();
        }
        if (parent.remAddrCfg == null) {
            remAddrCur = null;
        } else {
            remAddrCur = parent.remAddrCfg.copyBytes();
        }
        if (parent.dns1addrCfg == null) {
            dns1addrCur = null;
        } else {
            dns1addrCur = parent.dns1addrCfg.copyBytes();
        }
        if (parent.dns2addrCfg == null) {
            dns2addrCur = null;
        } else {
            dns2addrCur = parent.dns2addrCfg.copyBytes();
        }
        locAddrReq = parent.locAddrReq;
        clearUpperState();
    }

    /**
     * get one config
     *
     * @param txReq send reply
     * @return data
     */
    public Object getOneConfig(boolean txReq) {
        ifcPppIp4conf cfg = new ifcPppIp4conf();
        if (!txReq) {
            return cfg;
        }
        if (locAddrCur != null) {
            cfg.addr = locAddrCur.copyBytes();
        }
        if (dns1addrCur != null) {
            cfg.dns1 = dns1addrCur.copyBytes();
        }
        if (dns2addrCur != null) {
            cfg.dns2 = dns2addrCur.copyBytes();
        }
        return cfg;
    }

    /**
     * read options
     *
     * @param config config
     * @param tlv tlv
     * @return false on success, true on error
     */
    public boolean readOption(Object config, typLenVal tlv) {
        ifcPppIp4conf cfg = (ifcPppIp4conf) config;
        switch (tlv.valTyp) {
            case optionAddres:
                cfg.addr = new addrIPv4();
                cfg.addr.fromBuf(tlv.valDat, 0);
                return false;
            case optionAddr:
                cfg.addr = new addrIPv4();
                cfg.addr.fromBuf(tlv.valDat, 0);
                return false;
            case optionDns1:
                cfg.dns1 = new addrIPv4();
                cfg.dns1.fromBuf(tlv.valDat, 0);
                return false;
            case optionDns2:
                cfg.dns2 = new addrIPv4();
                cfg.dns2.fromBuf(tlv.valDat, 0);
                return false;
            case optionNbns1:
                cfg.nbns1 = new addrIPv4();
                cfg.nbns1.fromBuf(tlv.valDat, 0);
                return false;
            case optionNbns2:
                cfg.nbns2 = new addrIPv4();
                cfg.nbns2.fromBuf(tlv.valDat, 0);
                return false;
            case optionCompress:
                cfg.compress = bits.msbGetW(tlv.valDat, 0);
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
        ifcPppIp4conf dat = (ifcPppIp4conf) data;
        typLenVal tlv = getTlv();
        byte[] buf = new byte[128];
        if (dat.compress > 0) {
            bits.msbPutW(buf, 0, dat.compress);
            tlv.putBytes(pck, optionCompress, 2, buf);
        }
        if (dat.addr != null) {
            dat.addr.toBuffer(buf, 0);
            tlv.putBytes(pck, optionAddr, addrIPv4.size, buf);
        }
        if (dat.dns1 != null) {
            dat.dns1.toBuffer(buf, 0);
            tlv.putBytes(pck, optionDns1, addrIPv4.size, buf);
        }
        if (dat.dns2 != null) {
            dat.dns2.toBuffer(buf, 0);
            tlv.putBytes(pck, optionDns2, addrIPv4.size, buf);
        }
        if (dat.nbns1 != null) {
            dat.nbns1.toBuffer(buf, 0);
            tlv.putBytes(pck, optionNbns1, addrIPv4.size, buf);
        }
        if (dat.nbns2 != null) {
            dat.nbns2.toBuffer(buf, 0);
            tlv.putBytes(pck, optionNbns2, addrIPv4.size, buf);
        }
    }

    /**
     * got configure ack
     *
     * @param data data
     * @return false on success, true on error
     */
    public boolean gotConfAck(Object data) {
        ifcPppIp4conf dat = (ifcPppIp4conf) data;
        if (locAddrReq) {
            if (dat.addr == null) {
                return true;
            }
            if (dat.addr.isEmpty()) {
                return true;
            }
        }
        return false;
    }

    /**
     * got configure request
     *
     * @param data data
     * @param rej rejected
     * @return config
     */
    public Object gotConfReq(Object data, boolean rej) {
        boolean val = false;
        ifcPppIp4conf dat = (ifcPppIp4conf) data;
        ifcPppIp4conf res = new ifcPppIp4conf();
        if (dat.compress > 0) {
            res.compress = dat.compress;
            val = true;
        }
        if (rej) {
            if (val) {
                return res;
            } else {
                return null;
            }
        }
        if (dat.addr != null) {
            if (parent.remAddrCfg != null) {
                if (dat.addr.compare(dat.addr, parent.remAddrCfg) != 0) {
                    res.addr = remAddrCur.copyBytes();
                    val = true;
                }
            }
            if (remAddrCur == null) {
                remAddrCur = dat.addr;
            } else {
                if (dat.addr.compare(dat.addr, remAddrCur) != 0) {
                    res.addr = remAddrCur.copyBytes();
                    val = true;
                }
            }
        }
        if (dat.dns1 != null) {
            if (dat.dns1.isEmpty() && (parent.dns1addrCfg != null)) {
                res.dns1 = parent.dns1addrCfg.copyBytes();
                val = true;
            }
        }
        if (dat.dns2 != null) {
            if (dat.dns2.isEmpty() && (parent.dns2addrCfg != null)) {
                res.dns2 = parent.dns2addrCfg.copyBytes();
                val = true;
            }
        }
        if (val) {
            return res;
        } else {
            return null;
        }
    }

    /**
     * got configure nak
     *
     * @param data data
     */
    public void gotConfNak(Object data) {
        ifcPppIp4conf dat = (ifcPppIp4conf) data;
        ifcPppIp4conf cfg = (ifcPppIp4conf) txReq;
        if (dat.compress > 0) {
            cfg.compress = 0;
        }
        if ((dat.addr != null) && (locAddrCur != null)) {
            locAddrCur = dat.addr;
            cfg.addr = dat.addr;
        }
        if ((dat.dns1 != null) && (dns1addrCur != null)) {
            dns1addrCur = dat.dns1;
            cfg.dns1 = dat.dns1;
        }
        if ((dat.dns2 != null) && (dns2addrCur != null)) {
            dns2addrCur = dat.dns2;
            cfg.dns2 = dat.dns2;
        }
        if ((dat.nbns1 != null) && (dns1addrCur != null)) {
            cfg.nbns1 = dat.nbns1;
        }
        if ((dat.nbns2 != null) && (dns2addrCur != null)) {
            cfg.nbns2 = dat.nbns2;
        }
    }

    /**
     * got configure reject
     *
     * @param data data
     */
    public void gotConfRej(Object data) {
        ifcPppIp4conf dat = (ifcPppIp4conf) data;
        ifcPppIp4conf cfg = (ifcPppIp4conf) txReq;
        if (dat.compress > 0) {
            cfg.compress = 0;
        }
        if (dat.addr != null) {
            cfg.addr = null;
        }
        if (dat.dns1 != null) {
            cfg.dns1 = null;
        }
        if (dat.dns2 != null) {
            cfg.dns2 = null;
        }
        if (dat.nbns1 != null) {
            cfg.nbns1 = null;
        }
        if (dat.nbns2 != null) {
            cfg.nbns2 = null;
        }
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

class ifcPppIp4conf {

    public addrIPv4 addr = null;

    public addrIPv4 dns1 = null;

    public addrIPv4 dns2 = null;

    public addrIPv4 nbns1 = null;

    public addrIPv4 nbns2 = null;

    public int compress = -1;

    public String toString() {
        return "addr=" + addr + " dns1=" + dns1 + " dns2=" + dns2 + " compress=" + compress + " nbns1=" + nbns1 + " nbns2="
                + nbns2;
    }

}
