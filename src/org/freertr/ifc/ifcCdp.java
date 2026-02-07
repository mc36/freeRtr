package org.freertr.ifc;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrEmpty;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv4;
import org.freertr.addr.addrIPv6;
import org.freertr.addr.addrMac;
import org.freertr.addr.addrType;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgIfc;
import org.freertr.cfg.cfgInit;
import org.freertr.enc.enc7bit;
import org.freertr.ip.ipCor4;
import org.freertr.ip.ipCor6;
import org.freertr.pack.packHolder;
import org.freertr.tab.tabGen;
import org.freertr.util.bits;
import org.freertr.util.counter;
import org.freertr.util.logger;
import org.freertr.util.state;
import org.freertr.enc.encTlv;
import org.freertr.util.version;

/**
 * cisco discovery protocol
 *
 * @author matecsaba
 */
public class ifcCdp implements ifcUp {

    /**
     * snap org id
     */
    public final static int orgid = 0x00000c;

    /**
     * ethertype
     */
    public final static int ethtyp = 0x2000;

    /**
     * time between advertisements
     */
    public int advertiseInterval = 30000;

    /**
     * on demand gateway
     */
    public addrIPv4 odr4;

    /**
     * on demand gateway
     */
    public addrIPv6 odr6;

    /**
     * list of neighbors
     */
    public tabGen<ifcCdpNeigh> neighs = new tabGen<ifcCdpNeigh>();

    /**
     * keepalive
     */
    protected ifcCdpTxAdv keepTimer;

    private cfgIfc cfg;

    private ifcDn lower = new ifcNull();

    private addrType hwadr = new addrEmpty();

    private counter cntr = new counter();

    /**
     * device id
     */
    public final static int ttypDevId = 1;

    /**
     * address
     */
    public final static int ttypAddr = 2;

    /**
     * port id
     */
    public final static int ttypPrtId = 3;

    /**
     * capabilities
     */
    public final static int ttypCapa = 4;

    /**
     * version
     */
    public final static int ttypVer = 5;

    /**
     * platform
     */
    public final static int ttypPlat = 6;

    /**
     * ipv4 prefix
     */
    public final static int ttypIp4prfx = 7;

    /**
     * ipv6 prefix
     */
    public final static int ttypIp6prfx = 8;

    /**
     * vtp domain
     */
    public final static int ttypVtp = 9;

    /**
     * native vlan
     */
    public final static int ttypVlan = 10;

    /**
     * duplex
     */
    public final static int ttypDuplex = 11;

    /**
     * appliance id
     */
    public final static int ttypApplnc = 14;

    /**
     * power consumption
     */
    public final static int ttypPower = 16;

    /**
     * router
     */
    public final static int capaRouter = 0x01;

    /**
     * transparent bridge
     */
    public final static int capaBridge = 0x02;

    /**
     * source routed bridge
     */
    public final static int capaSrcRtBr = 0x04;

    /**
     * switch
     */
    public final static int capaSwitch = 0x08;

    /**
     * host
     */
    public final static int capaHost = 0x10;

    /**
     * igmp capable
     */
    public final static int capaIgmp = 0x20;

    /**
     * repeater
     */
    public final static int capaRepeater = 0x40;

    /**
     * decode capabilities
     *
     * @param i capabilities
     * @return decoded string
     */
    public static String capability2string(int i) {
        return bits.bit2str(i, capaRouter, "rtr") + " " + bits.bit2str(i, capaBridge, "brdg") + " " + bits.bit2str(i, capaSrcRtBr, "srcbrd") + " " + bits.bit2str(i, capaSwitch, "swch") + " " + bits.bit2str(i, capaHost, "host") + " " + bits.bit2str(i, capaIgmp, "igmp") + " " + bits.bit2str(i, capaRepeater, "repeater");
    }

    /**
     * create new instance
     *
     * @param ifc interface to use
     */
    public ifcCdp(cfgIfc ifc) {
        cfg = ifc;
        restartTimer(false);
    }

    public String toString() {
        return "cdp on " + lower;
    }

    private encTlv getTlv() {
        return new encTlv(0, 16, 16, 16, 1, 4, 4, 1, 0, 512, true);
    }

    public void recvPack(packHolder pck) {
        cntr.rx(pck);
        if (pck.msbGetW(0) != ethtyp) {
            cntr.drop(pck, counter.reasons.badEthTyp);
            return;
        }
        pck.getSkip(2);
        if (pck.getByte(0) != 2) {
            cntr.drop(pck, counter.reasons.badVer);
            return;
        }
        ifcCdpNeigh nei = new ifcCdpNeigh();
        nei.peer = pck.ETHsrc.copyBytes();
        nei.hostName = "";
        nei.holdTime = pck.getByte(1) * 1000; // hold time
        pck.getSkip(4);
        encTlv tlv = getTlv();
        for (;;) {
            if (tlv.getBytes(pck)) {
                break;
            }
            switch (tlv.valTyp) {
                case ttypDevId:
                    nei.hostName = tlv.getStr();
                    break;
                case ttypVer:
                    nei.swVer = tlv.getStr();
                    break;
                case ttypPlat:
                    nei.platform = tlv.getStr();
                    break;
                case ttypPrtId:
                    nei.portId = tlv.getStr();
                    break;
                case ttypCapa:
                    nei.capa = bits.msbGetD(tlv.valDat, 0);
                    break;
                case ttypVlan:
                    nei.vlan = bits.msbGetW(tlv.valDat, 0);
                    break;
                case ttypVtp:
                    nei.vtp = tlv.getStr();
                    break;
                case ttypAddr:
                    for (int p = 4; p < tlv.valSiz;) {
                        int tpt = tlv.valDat[p + 0] & 0xff;
                        int pl = tlv.valDat[p + 1] & 0xff;
                        int pt = tlv.valDat[p + 2] & 0xff;
                        p += pl + 2;
                        int al = tlv.valDat[p + 1] & 0xff;
                        p += 2;
                        if (tpt != 1) {
                            pt = -1;
                        }
                        switch (pt) {
                            case ipCor4.protocolNLPID:
                                addrIPv4 a4 = new addrIPv4();
                                a4.fromBuf(tlv.valDat, p);
                                nei.addr4 = new addrIP();
                                nei.addr4.fromIPv4addr(a4);
                                break;
                            case ipCor6.protocolNLPID:
                                addrIPv6 a6 = new addrIPv6();
                                a6.fromBuf(tlv.valDat, p);
                                nei.addr6 = new addrIP();
                                nei.addr6.fromIPv6addr(a6);
                                break;
                        }
                        p += al;
                    }
                    break;
            }
        }
        nei.created = bits.getTime();
        neighs.put(nei);
    }

    public void setParent(ifcDn parent) {
        lower = parent;
        hwadr = lower.getHwAddr();
    }

    public void setState(state.states stat) {
    }

    public void closeUp() {
    }

    public counter getCounter() {
        return cntr;
    }

    /**
     * setup timer thread
     *
     * @param shutdown set true to shut down
     */
    public void restartTimer(boolean shutdown) {
        keepTimer = null;
        if (shutdown) {
            return;
        }
        if (advertiseInterval < 1) {
            return;
        }
        keepTimer = new ifcCdpTxAdv(this);
        keepTimer.start();
    }

    private void putAddr(encTlv tlv, int typ, addrType adr) {
        if (adr == null) {
            return;
        }
        bits.msbPutD(tlv.valDat, 0, bits.msbGetD(tlv.valDat, 0) + 1);
        bits.msbPutW(tlv.valDat, tlv.valSiz + 0, 0x0101);
        bits.putByte(tlv.valDat, tlv.valSiz + 2, typ);
        bits.msbPutW(tlv.valDat, tlv.valSiz + 3, adr.getSize());
        tlv.valSiz += 5;
        adr.toBuffer(tlv.valDat, tlv.valSiz);
        tlv.valSiz += adr.getSize();
    }

    /**
     * send advertisement
     */
    protected void sendAdvert() {
        encTlv tlv = getTlv();
        long tim = bits.getTime();
        for (int i = neighs.size(); i >= 0; i--) {
            ifcCdpNeigh nei = neighs.get(i);
            if (nei == null) {
                continue;
            }
            if ((nei.created + nei.holdTime) > tim) {
                continue;
            }
            neighs.del(nei);
        }
        packHolder pck = new packHolder(true, true);
        pck.ETHtrg.fromString("0100:0ccc:cccc");
        if (hwadr.getSize() == addrMac.size) {
            pck.ETHsrc.fromBuf(hwadr.getBytes(), 0);
        }
        pck.msbPutW(0, ethtyp); // ethertype
        pck.putSkip(2);
        pck.merge2end();
        pck.putByte(0, 2); // version
        pck.putByte(1, (advertiseInterval * 4) / 1000); // hold time
        pck.msbPutW(2, 0); // checksum
        pck.putSkip(4);
        tlv.putStr(pck, ttypDevId, cfgAll.hostName);
        tlv.putStr(pck, ttypVer, cfgInit.versionFull + "\n" + cfgInit.getHWfwd1liner() + "\n" + cfgInit.getVMname() + "\n" + cfgInit.getKernelName());
        tlv.putStr(pck, ttypPlat, version.name);
        tlv.putStr(pck, ttypPrtId, cfg.name);
        bits.msbPutD(tlv.valDat, 0, capaRouter | capaBridge);
        tlv.putBytes(pck, ttypCapa, 4, tlv.valDat);
        tlv.valSiz = 4;
        bits.msbPutD(tlv.valDat, 0, 0);
        putAddr(tlv, ipCor4.protocolNLPID, cfg.addr4);
        putAddr(tlv, ipCor6.protocolNLPID, cfg.addr6);
        tlv.putBytes(pck, ttypAddr, tlv.valSiz, tlv.valDat);
        if (odr4 != null) {
            odr4.toBuffer(tlv.valDat, 0);
            tlv.putBytes(pck, ttypIp4prfx, addrIPv4.size, tlv.valDat);
        }
        if (odr6 != null) {
            odr6.toBuffer(tlv.valDat, 0);
            tlv.putBytes(pck, ttypIp6prfx, addrIPv6.size, tlv.valDat);
        }
        if ((pck.headSize() & 1) != 0) {
            tlv.valDat[0] = 0;
            tlv.putBytes(pck, 0xffff, 1, tlv.valDat);
        }
        int i = pck.headSize();
        pck.lsbPutW(2 - i, 0xffff - pck.putIPsum(-i, i, 0));
        pck.merge2end();
        cntr.tx(pck);
        lower.sendPack(pck);
    }

    /**
     * get show output
     *
     * @param detailed detailed listing
     * @return list of neighbors
     */
    public List<String> getShNeigh(boolean detailed) {
        List<String> l = new ArrayList<String>();
        for (int i = 0; i < neighs.size(); i++) {
            ifcCdpNeigh nei = neighs.get(i);
            if (!detailed) {
                l.add(cfg.name + "|" + nei);
            } else {
                nei.dump(l);
            }
        }
        return l;
    }

}

class ifcCdpTxAdv implements Runnable {

    private ifcCdp lower;

    public ifcCdpTxAdv(ifcCdp parent) {
        lower = parent;
    }

    public void start() {
        new Thread(this).start();
    }

    public void run() {
        try {
            for (;;) {
                if (lower.keepTimer != this) {
                    break;
                }
                lower.sendAdvert();
                bits.sleep(lower.advertiseInterval);
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

}

class ifcCdpNeigh implements Comparable<ifcCdpNeigh> {

    public addrMac peer;

    public int holdTime;

    public long created;

    public String hostName;

    public String swVer;

    public String platform;

    public String portId;

    public addrIP addr4;

    public addrIP addr6;

    public int capa;

    public String vtp;

    public int vlan;

    public String toString() {
        return enc7bit.doOneString(hostName) + "|" + enc7bit.doOneString(portId) + "|" + addr4 + "|" + addr6;
    }

    public void dump(List<String> l) {
        l.add("");
        l.add("peer|" + peer);
        l.add("hostname|" + enc7bit.doOneString(hostName));
        l.add("port id|" + enc7bit.doOneString(portId));
        l.add("platform|" + enc7bit.doOneString(platform));
        l.add("ipv4 addr|" + addr4);
        l.add("ipv6 addr|" + addr6);
        l.add("sw version|" + enc7bit.doOneString(swVer));
        l.add("vlan|" + vlan);
        l.add("vtp|" + enc7bit.doOneString(vtp));
        l.add("hold|" + holdTime);
        l.add("capa|" + ifcCdp.capability2string(capa));
    }

    public int compareTo(ifcCdpNeigh o) {
        return hostName.compareTo(o.hostName);
    }

}
