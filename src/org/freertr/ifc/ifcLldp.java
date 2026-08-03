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
import org.freertr.pack.packHolder;
import org.freertr.tab.tabGen;
import org.freertr.util.bits;
import org.freertr.util.counter;
import org.freertr.util.logger;
import org.freertr.util.state;
import org.freertr.enc.encTlv;

/**
 * link layer discovery (ieee 802.1ab) protocol
 *
 * @author matecsaba
 */
public class ifcLldp implements ifcUp {

    /**
     * ethertype
     */
    public final static int ethtyp = 0x88cc;

    /**
     * time between advertisements
     */
    public int advertiseInterval = 30000;

    /**
     * list of neighbors
     */
    public tabGen<ifcLldpNeigh> neighs = new tabGen<ifcLldpNeigh>();

    /**
     * keepalive
     */
    protected ifcLldpTxAdv keepTimer;

    private cfgIfc cfg;

    private ifcDn lower = new ifcNull();

    private addrType hwadr = new addrEmpty();

    private counter cntr = new counter();

    /**
     * end of pdu
     */
    public final static int ttypEnd = 0;

    /**
     * chassis id
     */
    public final static int ttypChassis = 1;

    /**
     * port id
     */
    public final static int ttypPrtId = 2;

    /**
     * time to live
     */
    public final static int ttypTime = 3;

    /**
     * port description
     */
    public final static int ttypPrtDesc = 4;

    /**
     * system name
     */
    public final static int ttypSysName = 5;

    /**
     * system description
     */
    public final static int ttypSysDesc = 6;

    /**
     * system capabilities
     */
    public final static int ttypSysCapa = 7;

    /**
     * management address
     */
    public final static int ttypMgmtAddr = 8;

    /**
     * other
     */
    public final static int capaOther = 0x01;

    /**
     * repeater
     */
    public final static int capaRepeat = 0x02;

    /**
     * bridge
     */
    public final static int capaBridge = 0x04;

    /**
     * wlan access point
     */
    public final static int capaWlan = 0x08;

    /**
     * router
     */
    public final static int capaRouter = 0x10;

    /**
     * telephone
     */
    public final static int capaTelephone = 0x20;

    /**
     * docsis device
     */
    public final static int capaDocsis = 0x40;

    /**
     * station
     */
    public final static int capaStation = 0x80;

    /**
     * decode capabilities
     *
     * @param i capabilities
     * @return decoded string
     */
    public static String capability2string(int i) {
        return bits.bit2str(i, capaOther, "oth") + " " + bits.bit2str(i, capaRepeat, "rep") + " " + bits.bit2str(i, capaBridge, "brdg") + " " + bits.bit2str(i, capaWlan, "ap") + " " + bits.bit2str(i, capaRouter, "rtr") + " " + bits.bit2str(i, capaTelephone, "tel") + " " + bits.bit2str(i, capaDocsis, "cab") + " " + bits.bit2str(i, capaStation, "stat");
    }

    /**
     * create new instance
     *
     * @param ifc interface to use
     */
    public ifcLldp(cfgIfc ifc) {
        cfg = ifc;
        restartTimer(false);
    }

    public String toString() {
        return "lldp on " + lower;
    }

    private encTlv getTvl() {
        return new encTlv(0, 7, 8, 8, 1, 0, 2, 1, 0, 512, true);
    }

    /**
     * received packet
     *
     * @param pck packet
     */
    public void recvPack(packHolder pck) {
        cntr.rx(pck);
        if (pck.msbGetW(0) != ethtyp) {
            cntr.drop(pck, counter.reasons.badEthTyp);
            return;
        }
        pck.getSkip(2);
        ifcLldpNeigh nei = new ifcLldpNeigh();
        nei.peer = pck.ETHsrc.copyBytes();
        nei.sysName = "";
        addrIPv4 a4 = new addrIPv4();
        addrIPv6 a6 = new addrIPv6();
        encTlv tlv = getTvl();
        for (;;) {
            if (tlv.getBytes(pck)) {
                break;
            }
            switch (tlv.valTyp) {
                case ttypEnd:
                    break;
                case ttypChassis:
                    nei.chassis = new addrMac();
                    nei.chassis.fromBuf(tlv.valDat, 1);
                    break;
                case ttypPrtId:
                    if (tlv.valSiz < 1) {
                        continue;
                    }
                    switch (tlv.valDat[0]) {
                        case 3: // mac
                            addrMac mac = new addrMac();
                            mac.fromBuf(tlv.valDat, 1);
                            nei.portId = "" + mac;
                            break;
                        case 5: // str
                            byte[] buf = new byte[tlv.valSiz - 1];
                            bits.byteCopy(tlv.valDat, 1, buf, 0, buf.length);
                            nei.portId = new String(buf);
                            break;
                        case 7: // loc
                            buf = new byte[tlv.valSiz - 1];
                            bits.byteCopy(tlv.valDat, 1, buf, 0, buf.length);
                            nei.portId = new String(buf);
                            break;
                        default:
                            nei.portId = "unknown:" + tlv.dump();
                            break;
                    }
                    break;
                case ttypTime:
                    nei.ttl = bits.msbGetW(tlv.valDat, 0) * 1000;
                    break;
                case ttypPrtDesc:
                    nei.portDesc = tlv.getStr();
                    break;
                case ttypSysName:
                    nei.sysName = tlv.getStr();
                    break;
                case ttypSysDesc:
                    nei.sysDesc = tlv.getStr();
                    break;
                case ttypSysCapa:
                    nei.capaSys = bits.msbGetW(tlv.valDat, 0);
                    nei.capaCfg = bits.msbGetW(tlv.valDat, 2);
                    break;
                case ttypMgmtAddr:
                    switch (tlv.valDat[1]) {
                        case 1:
                            a4.fromBuf(tlv.valDat, 2);
                            nei.addr4 = new addrIP();
                            nei.addr4.fromIPv4addr(a4);
                            break;
                        case 2:
                            a6.fromBuf(tlv.valDat, 2);
                            nei.addr6 = new addrIP();
                            nei.addr6.fromIPv6addr(a6);
                            break;
                        case 6:
                            nei.addrM = new addrMac();
                            nei.addrM.fromBuf(tlv.valDat, 2);
                            break;
                    }
                    break;
            }
        }
        nei.created = bits.getTime();
        neighs.put(nei);
    }

    /**
     * set parent
     *
     * @param parent parent
     */
    public void setParent(ifcDn parent) {
        lower = parent;
        hwadr = lower.getHwAddr();
    }

    /**
     * set state
     *
     * @param stat state
     */
    public void setState(state.states stat) {
    }

    /**
     * close interface
     */
    public void closeUp() {
    }

    /**
     * get counter
     *
     * @return counter
     */
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
        keepTimer = new ifcLldpTxAdv(this);
        keepTimer.start();
    }

    private void putAddr(packHolder pck, encTlv tlv, int typ, addrType adr) {
        if (adr == null) {
            return;
        }
        int siz = adr.getSize();
        tlv.valDat[0] = (byte) (siz + 1);
        tlv.valDat[1] = (byte) typ;
        adr.toBuffer(tlv.valDat, 2);
        tlv.valDat[siz + 2] = 3; // port number
        tlv.valDat[siz + 3] = 0; // port id
        tlv.valDat[siz + 4] = 0; // port id
        tlv.valDat[siz + 5] = 0; // port id
        tlv.valDat[siz + 6] = 0; // port id
        tlv.valDat[siz + 7] = 0; // oid size
        tlv.putBytes(pck, ttypMgmtAddr, siz + 8, tlv.valDat);
    }

    /**
     * send advertisement
     */
    protected void sendAdvert() {
        encTlv tlv = getTvl();
        long tim = bits.getTime();
        for (int i = neighs.size(); i >= 0; i--) {
            ifcLldpNeigh nei = neighs.get(i);
            if (nei == null) {
                continue;
            }
            if ((nei.created + nei.ttl) > tim) {
                continue;
            }
            neighs.del(nei);
        }
        packHolder pck = new packHolder(true, true);
        pck.ETHtrg.fromString("0180:c200:000e");
        if (hwadr.getSize() == addrMac.size) {
            pck.ETHsrc.fromBuf(hwadr.getBytes(), 0);
        }
        pck.msbPutW(0, ethtyp); // ethertype
        pck.putSkip(2);
        tlv.valDat[0] = 4; // mac address
        hwadr.toBuffer(tlv.valDat, 1);
        tlv.putBytes(pck, ttypChassis, 7, tlv.valDat);
        byte[] buf = cfg.name.getBytes();
        tlv.valDat[0] = 5; // name
        bits.byteCopy(buf, 0, tlv.valDat, 1, buf.length);
        tlv.putBytes(pck, ttypPrtId, buf.length + 1, tlv.valDat);
        bits.msbPutW(tlv.valDat, 0, (advertiseInterval * 4) / 1000);
        tlv.putBytes(pck, ttypTime, 2, tlv.valDat);
        tlv.putStr(pck, ttypPrtDesc, cfg.name);
        tlv.putStr(pck, ttypSysName, cfgAll.hostName);
        tlv.putStr(pck, ttypSysDesc, cfgInit.versionFull + " " + cfgInit.getHWfwd1liner() + " " + cfgInit.getVMname() + " " + cfgInit.getKernelName());
        bits.msbPutW(tlv.valDat, 0, capaRouter | capaBridge); // capabilities
        bits.msbPutW(tlv.valDat, 2, capaRouter | capaBridge); // capabilities
        tlv.putBytes(pck, ttypSysCapa, 4, tlv.valDat);
        putAddr(pck, tlv, 1, cfg.addr4);
        putAddr(pck, tlv, 2, cfg.addr6);
        putAddr(pck, tlv, 6, hwadr);
        tlv.putBytes(pck, ttypEnd, 0, tlv.valDat);
        pck.merge2beg();
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
            ifcLldpNeigh nei = neighs.get(i);
            if (!detailed) {
                l.add(cfg.name + "|" + nei);
            } else {
                nei.dump(l);
            }
        }
        return l;
    }

}

class ifcLldpTxAdv implements Runnable {

    private ifcLldp lower;

    public ifcLldpTxAdv(ifcLldp parent) {
        lower = parent;
    }

    public void start() {
        logger.startThread(this);
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

class ifcLldpNeigh implements Comparable<ifcLldpNeigh> {

    public addrMac peer;

    public addrMac chassis;

    public String sysName;

    public String sysDesc;

    public String portId;

    public String portDesc;

    public addrIP addr4;

    public addrIP addr6;

    public addrMac addrM;

    public int ttl;

    public int capaSys;

    public int capaCfg;

    public long created;

    public String toString() {
        return enc7bit.doOneString(sysName) + "|" + enc7bit.doOneString(portId) + "|" + addr4 + "|" + addr6;
    }

    public void dump(List<String> l) {
        l.add("");
        l.add("peer|" + peer);
        l.add("system name|" + enc7bit.doOneString(sysName));
        l.add("port id|" + enc7bit.doOneString(portId));
        l.add("port desc|" + enc7bit.doOneString(portDesc));
        l.add("ipv4 addr|" + addr4);
        l.add("ipv6 addr|" + addr6);
        l.add("mac addr|" + addrM);
        l.add("system desc|" + enc7bit.doOneString(sysDesc));
        l.add("ttl|" + ttl);
        l.add("sys capa|" + ifcLldp.capability2string(capaSys));
        l.add("cfg capa|" + ifcLldp.capability2string(capaCfg));
    }

    public int compareTo(ifcLldpNeigh o) {
        return sysName.compareTo(o.sysName);
    }

}
