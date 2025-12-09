package org.freertr.serv;

import org.freertr.addr.addrMac;
import org.freertr.addr.addrType;
import org.freertr.cfg.cfgIfc;
import org.freertr.ifc.ifcDn;
import org.freertr.ifc.ifcEthTyp;
import org.freertr.ifc.ifcNull;
import org.freertr.ifc.ifcUp;
import org.freertr.ip.ipIfc4;
import org.freertr.ip.ipIfc4arp;
import org.freertr.pack.packHolder;
import org.freertr.util.bits;
import org.freertr.util.counter;
import org.freertr.util.logger;
import org.freertr.util.state;

/**
 * one stack interface
 *
 * @author matecsaba
 */
public class servStackIfc implements Comparable<servStackIfc>, ifcUp {

    private final static int magic1 = 0x00010000 | ipIfc4.type;

    private final static int magic2 = 0x06040bad;

    /**
     * interface id
     */
    protected cfgIfc pi;

    /**
     * id
     */
    protected int id;

    private final servStackFwd lower;

    private final counter cntr = new counter();

    /**
     * metric
     */
    protected int metric = 10;

    /**
     * random id
     */
    protected int randId;

    /**
     * last timestamp
     */
    protected long lastTime;

    /**
     * last forwarder
     */
    protected servStackFwd lastFwdr;

    /**
     * last portid
     */
    protected servStackIfc lastPort;

    /**
     * ready to use
     */
    protected boolean ready;

    /**
     * interface handler
     */
    protected ifcDn parent = new ifcNull();

    /**
     * ethertype handler
     */
    protected ifcEthTyp ifc;

    /**
     * create instance
     *
     * @param prnt parent
     * @param ifc interface
     */
    protected servStackIfc(servStackFwd prnt, cfgIfc ifc) {
        pi = ifc;
        lower = prnt;
        randId = bits.randomD();
    }

    /**
     * get hardware forwarder info
     *
     * @return offload info
     */
    protected String getShGenOneLiner() {
        if (pi == null) {
            return "n/a,";
        }
        return pi.name + ",";
    }

    public String toString() {
        return "" + pi;
    }

    public int compareTo(servStackIfc o) {
        return pi.compareTo(o.pi);
    }

    /**
     * get source address
     *
     * @return mac address
     */
    protected addrMac getMac() {
        addrType adr = ifc.getHwAddr();
        if (adr.getSize() != addrMac.size) {
            return new addrMac();
        }
        return (addrMac) adr;
    }

    /**
     * send keepalive packet
     */
    protected void sendHello() {
        packHolder pck = new packHolder(true, true);
        pck.msbPutW(0, ipIfc4arp.type);
        pck.putSkip(2);
        pck.putFill(0, ipIfc4arp.size, 0);
        pck.msbPutD(0, magic1);
        pck.msbPutD(4, magic2);
        pck.msbPutD(8, lower.lower.randId);
        pck.msbPutD(12, lower.id);
        pck.msbPutD(16, id);
        pck.msbPutD(20, randId);
        pck.putSkip(ipIfc4arp.size);
        pck.merge2beg();
        pck.ETHsrc.setAddr(getMac());
        pck.ETHtrg.setAddr(addrMac.getBroadcast());
        pck.ETHtype = ipIfc4arp.type;
        parent.sendPack(pck);
    }

    public void recvPack(packHolder pck) {
        int typ = pck.msbGetW(0);
        if (typ != ipIfc4arp.type) {
            logger.info("got invalid (" + bits.toHexW(typ) + ") packet on " + ifc);
            return;
        }
        pck.getSkip(2);
        if (pck.dataSize() < ipIfc4arp.size) {
            logger.info("got truncated packet on " + ifc);
            return;
        }
        if (pck.msbGetD(0) != magic1) {
            logger.info("got invalid magic on " + ifc);
            return;
        }
        if (pck.msbGetD(4) != magic2) {
            logger.info("got invalid magic on " + ifc);
            return;
        }
        if (pck.msbGetD(8) != lower.lower.randId) {
            logger.info("got invalid cluster on " + ifc);
            return;
        }
        int i = pck.msbGetD(12);
        if (i == lower.id) {
            logger.info("got looping packet on " + ifc);
            return;
        }
        if ((i < 0) || (i >= lower.lower.fwds.size())) {
            logger.info("got invalid forwarder id on " + ifc);
            return;
        }
        lastFwdr = lower.lower.fwds.get(i);
        i = pck.msbGetD(16);
        if ((i < 0) || (i >= lastFwdr.ifaces.size())) {
            logger.info("got invalid interface id on " + ifc);
            return;
        }
        lastPort = lastFwdr.ifaces.get(i);
        int lastRand = pck.msbGetD(20);
        if (lastPort.randId != lastRand) {
            logger.info("got invalid random id on " + ifc);
            return;
        }
        lastTime = bits.getTime();
    }

    public void setParent(ifcDn lower) {
        parent = lower;
    }

    public void setState(state.states stat) {
    }

    public void closeUp() {
    }

    public counter getCounter() {
        return cntr;
    }

}
