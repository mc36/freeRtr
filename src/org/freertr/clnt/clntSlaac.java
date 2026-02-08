package org.freertr.clnt;

import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv6;
import org.freertr.addr.addrMac;
import org.freertr.cfg.cfgIfc;
import org.freertr.ifc.ifcEthTyp;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipFwdIface;
import org.freertr.ip.ipIcmp6;
import org.freertr.ip.ipIfc6;
import org.freertr.ip.ipPrt;
import org.freertr.pack.packHolder;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.counter;
import org.freertr.util.debugger;
import org.freertr.util.logger;
import org.freertr.util.notifier;
import org.freertr.util.state;
import org.freertr.enc.encTlv;

/**
 * stateless address autoconfiguration client
 *
 * @author matecsaba
 */
public class clntSlaac implements Runnable, ipPrt {

    /**
     * config class
     */
    public cfgIfc cfger;

    /**
     * minimum lease time
     */
    public int leaseMin = 60 * 1000;

    /**
     * maximum lease time
     */
    public int leaseMax = 43200 * 1000;

    private ipFwd lower;

    private ipFwdIface iface;

    private ipIfc6 ipifc;

    private ifcEthTyp ethtyp;

    private boolean working;

    private boolean gotAddr;

    private notifier notif = new notifier();

    private long validFor;

    /**
     * my address
     */
    public addrIPv6 locAddr;

    /**
     * my netmask
     */
    public addrIPv6 locMask;

    /**
     * gw address
     */
    public addrIPv6 gwAddr;

    /**
     * dns1 address
     */
    public addrIPv6 dns1addr;

    /**
     * dns2 address
     */
    public addrIPv6 dns2addr;

    /**
     * create new slaac client on interface
     *
     * @param wrkr ip worker
     * @param ifc forwarder interface
     * @param ipi ip interface
     * @param phy handler of interface
     * @param cfg config interface
     */
    public clntSlaac(ipFwd wrkr, ipFwdIface ifc, ipIfc6 ipi, ifcEthTyp phy, cfgIfc cfg) {
        lower = wrkr;
        iface = ifc;
        ipifc = ipi;
        ethtyp = phy;
        cfger = cfg;
        clearState();
        working = true;
        logger.startThread(this);
    }

    public String toString() {
        return "slaac on " + ethtyp;
    }

    /**
     * stop client
     */
    public void closeClient() {
        working = false;
        notif.wakeup();
    }

    /**
     * clear state
     */
    public void clearState() {
        gotAddr = false;
        locAddr = addrIPv6.getEmpty();
        locMask = addrIPv6.getEmpty();
        gwAddr = addrIPv6.getEmpty();
        dns1addr = addrIPv6.getEmpty();
        dns2addr = addrIPv6.getEmpty();
        notif.wakeup();
    }

    private boolean doWork() {
        if (bits.getTime() > validFor) {
            gotAddr = false;
        }
        if (gotAddr) {
            notif.sleep(10000);
            if (cfger.addr6 == null) {
                return false;
            }
            if (locAddr.compareTo(cfger.addr6) == 0) {
                return false;
            }
            clearState();
        }
        addrMac mac;
        try {
            mac = (addrMac) ethtyp.getHwAddr();
        } catch (Exception e) {
            mac = addrMac.getRandom();
        }
        locMask.fromString("ffff:ffff:ffff:ffff::");
        addrIPv6 ll = ipifc.getLinkLocalAddr().toIPv6();
        if (cfger.addr6.isEmpty()) {
            cfger.addr6changed(ll, locMask, null);
        }
        lower.protoAdd(this, iface, null);
        packHolder pck = new packHolder(true, true);
        for (;;) {
            if (!working) {
                return true;
            }
            if (cfger.addr6 == null) {
                return false;
            }
            if (gotAddr) {
                break;
            }
            if (debugger.clntSlaacTraf) {
                logger.debug("sending solicit");
            }
            pck.clear();
            ((ipIcmp6) lower.icmpCore).createRouterSol(mac, pck, ll);
            iface.lower.sendProto(pck, pck.IPtrg);
            notif.sleep(10000);
        }
        lower.protoDel(this, iface, null);
        ll = ipifc.getLinkLocalAddr().toIPv6();
        addrIPv6 wld = new addrIPv6();
        wld.setNot(locMask);
        ll.setAnd(ll, wld);
        locAddr.setAnd(locAddr, locMask);
        locAddr.setOr(locAddr, ll);
        cfger.addr6changed(locAddr, locMask, gwAddr);
        return false;
    }

    public void run() {
        if (debugger.clntSlaacTraf) {
            logger.debug("started");
        }
        try {
            for (;;) {
                if (doWork()) {
                    break;
                }
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
        if (debugger.clntSlaacTraf) {
            logger.debug("stopped");
        }
    }

    /**
     * get protocol number
     *
     * @return number
     */
    public int getProtoNum() {
        return ipIcmp6.protoNum;
    }

    /**
     * close interface
     *
     * @param iface interface
     */
    public void closeUp(ipFwdIface iface) {
    }

    /**
     * set state
     *
     * @param iface interface
     * @param stat state
     */
    public void setState(ipFwdIface iface, state.states stat) {
    }

    /**
     * received packet
     *
     * @param rxIfc interface
     * @param pck packet
     */
    public void recvPack(ipFwdIface rxIfc, packHolder pck) {
        if (lower.icmpCore.parseICMPheader(pck)) {
            return;
        }
        if (pck.ICMPtc != ipIcmp6.icmpRtrAdv) {
            ((ipIcmp6) lower.icmpCore).recvPack(rxIfc, pck);
            return;
        }
        pck.getSkip(ipIcmp6.size);
        pck.getSkip(8);
        encTlv tlv = ipIcmp6.getTLVreader();
        if (debugger.clntSlaacTraf) {
            logger.debug("got advertisement");
        }
        int pl = -1;
        int lt = -1;
        for (;;) {
            if (tlv.getBytes(pck)) {
                break;
            }
            switch (tlv.valTyp) {
                case 3:
                    lt = bits.msbGetD(tlv.valDat, 6);
                    pl = tlv.valDat[0] & 0xff;
                    locAddr.fromBuf(tlv.valDat, 14);
                    break;
                case 25:
                    dns1addr.fromBuf(tlv.valDat, 6);
                    dns2addr.fromBuf(tlv.valDat, 22);
                    break;
            }
        }
        if (pl < 0) {
            return;
        }
        locMask.fromNetmask(pl);
        gwAddr = pck.IPsrc.toIPv6();
        if (debugger.clntSlaacTraf) {
            logger.debug("addr=" + locAddr + "/" + locMask + " gw=" + gwAddr + " dns1=" + dns1addr + " dns2=" + dns2addr + " valid=" + lt);
        }
        if (locAddr.isLinkLocal()) {
            return;
        }
        lt = lt * 700;
        if (lt > leaseMax) {
            lt = leaseMax;
        }
        if (lt < leaseMin) {
            lt = leaseMin;
        }
        validFor = lt + bits.getTime();
        gotAddr = true;
        notif.wakeup();
    }

    /**
     * alert packet
     *
     * @param rxIfc interface
     * @param pck packet
     * @return false on success, true on error
     */
    public boolean alertPack(ipFwdIface rxIfc, packHolder pck) {
        return true;
    }

    /**
     * error packet
     *
     * @param err error code
     * @param rtr address
     * @param rxIfc interface
     * @param pck packet
     */
    public void errorPack(counter.reasons err, addrIP rtr, ipFwdIface rxIfc, packHolder pck) {
    }

    /**
     * get counter
     *
     * @return counter
     */
    public counter getCounter() {
        return new counter();
    }

    /**
     * get configuration
     *
     * @param l storage
     * @param beg beginning
     * @param cmd command
     */
    public void getConfig(List<String> l, String beg, String cmd) {
        l.add(beg + cmd + "renew-min " + leaseMin);
        l.add(beg + cmd + "renew-max " + leaseMax);
    }

    /**
     * do configuration
     *
     * @param a command
     * @param cmd commands
     * @return result code, true on error, false on success
     */
    public boolean doConfig(String a, cmds cmd) {
        if (a.equals("renew-min")) {
            leaseMin = bits.str2num(cmd.word());
            return false;
        }
        if (a.equals("renew-max")) {
            leaseMax = bits.str2num(cmd.word());
            return false;
        }
        return true;
    }

}
