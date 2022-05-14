package net.freertr.serv;

import java.util.Comparator;
import net.freertr.addr.addrMac;
import net.freertr.addr.addrType;
import net.freertr.ifc.ifcDn;
import net.freertr.ifc.ifcEthTyp;
import net.freertr.ifc.ifcNull;
import net.freertr.ifc.ifcUp;
import net.freertr.ip.ipIfc4;
import net.freertr.ip.ipIfc4arp;
import net.freertr.pack.packHolder;
import net.freertr.util.bits;
import net.freertr.util.counter;
import net.freertr.util.logger;
import net.freertr.util.state;

/**
 * one p4lang backplane interface
 *
 * @author matecsaba
 */
public class servP4langBkpl implements Comparator<servP4langBkpl>, ifcUp {

    private final static int magic1 = 0x00010000 | ipIfc4.type;

    private final static int magic2 = 0x06040bad;

    /**
     * interface id
     */
    protected final int id;

    private final servP4langCfg lower;

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
    protected int lastFwdr;

    /**
     * last portid
     */
    protected int lastPort;

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
     * @param num interface id
     */
    protected servP4langBkpl(servP4langCfg prnt, int num) {
        id = num;
        lower = prnt;
        randId = bits.randomD();
    }

    public int compare(servP4langBkpl o1, servP4langBkpl o2) {
        if (o1.id < o2.id) {
            return -1;
        }
        if (o1.id > o2.id) {
            return +1;
        }
        return 0;
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
        pck.msbPutD(8, lower.parent.dscvry.randId);
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
        if (pck.msbGetD(8) != lower.parent.dscvry.randId) {
            logger.info("got invalid cluster on " + ifc);
            return;
        }
        lastFwdr = pck.msbGetD(12);
        if (lastFwdr == lower.id) {
            logger.info("got looping packet on " + ifc);
            return;
        }
        if ((lastFwdr < 0) || (lastFwdr >= lower.parent.fwds.size())) {
            logger.info("got invalid forwarder id on " + ifc);
            return;
        }
        servP4langCfg peer = lower.parent.fwds.get(lastFwdr);
        lastPort = pck.msbGetD(16);
        servP4langBkpl pif = peer.backPlanes.find(new servP4langBkpl(lower, lastPort));
        if (pif == null) {
            logger.info("got invalid interface id on " + ifc);
            return;
        }
        int lastRand = pck.msbGetD(20);
        if (pif.randId != lastRand) {
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
