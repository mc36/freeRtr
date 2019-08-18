package clnt;

import addr.addrIP;
import addr.addrIPv6;
import addr.addrMac;
import cfg.cfgIfc;
import ifc.ifcEthTyp;
import ip.ipFwd;
import ip.ipFwdIface;
import ip.ipIcmp6;
import ip.ipIfc6;
import ip.ipPrt;
import pack.packHolder;
import util.counter;
import util.debugger;
import util.logger;
import util.notifier;
import util.state;
import util.typLenVal;

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

    private ipFwd lower;

    private ipFwdIface iface;

    private ifcEthTyp ethtyp;

    private boolean working;

    private boolean gotAddr;

    private notifier notif = new notifier();

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
     * @param phy handler of interface
     * @param cfg config interface
     */
    public clntSlaac(ipFwd wrkr, ipFwdIface ifc, ifcEthTyp phy, cfgIfc cfg) {
        lower = wrkr;
        iface = ifc;
        ethtyp = phy;
        cfger = cfg;
        locAddr = addrIPv6.getEmpty();
        locMask = addrIPv6.getEmpty();
        gwAddr = addrIPv6.getEmpty();
        dns1addr = addrIPv6.getEmpty();
        dns2addr = addrIPv6.getEmpty();
        working = true;
        new Thread(this).start();
        if (debugger.clntSlaacTraf) {
            logger.debug("started");
        }
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

    public void run() {
        addrMac mac;
        try {
            mac = (addrMac) ethtyp.getHwAddr();
        } catch (Exception e) {
            mac = addrMac.getRandom();
        }
        locMask.fromString("ffff:ffff:ffff:ffff::");
        addrIPv6 ll = addrIPv6.genLinkLocal(mac);
        cfger.addr6changed(ll, locMask, null);
        lower.protoAdd(this, iface, null);
        packHolder pck = new packHolder(true, true);
        for (;;) {
            if (!working) {
                break;
            }
            if (debugger.clntSlaacTraf) {
                logger.debug("sending solicit");
            }
            pck.clear();
            ((ipIcmp6) lower.icmpCore).createRouterSol(mac, pck, cfger.addr6);
            ((ipIfc6) iface.lower).sendProto(pck, pck.IPtrg);
            notif.sleep(10000);
        }
        lower.protoDel(this, iface, null);
        if (debugger.clntSlaacTraf) {
            logger.debug("stopped");
        }
        if (!gotAddr) {
            return;
        }
        addrIPv6 wld = new addrIPv6();
        wld.setNot(locMask);
        ll.setAnd(ll, wld);
        locAddr.setAnd(locAddr, locMask);
        locAddr.setOr(locAddr, ll);
        cfger.addr6changed(locAddr, locMask, gwAddr);
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
            return;
        }
        pck.getSkip(ipIcmp6.size);
        pck.getSkip(8);
        typLenVal tlv = ipIcmp6.getTLVreader();
        if (debugger.clntSlaacTraf) {
            logger.debug("got advertisement");
        }
        int pl = -1;
        for (;;) {
            if (tlv.getBytes(pck)) {
                break;
            }
            switch (tlv.valTyp) {
                case 3:
                    pl = tlv.valDat[0];
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
            logger.debug("addr=" + locAddr + "/" + locMask + " gw=" + gwAddr + " dns1=" + dns1addr + " dns2=" + dns2addr);
        }
        working = false;
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

}
