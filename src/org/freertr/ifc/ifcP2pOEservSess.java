package org.freertr.ifc;

import org.freertr.addr.addrMac;
import org.freertr.addr.addrType;
import org.freertr.cfg.cfgIfc;
import org.freertr.pack.packHolder;
import org.freertr.pack.packPppOE;
import org.freertr.util.counter;
import org.freertr.util.debugger;
import org.freertr.util.logger;
import org.freertr.util.state;

/**
 * ppp over ethernet (rfc2516) protocol server client
 *
 * @author matecsaba
 */
public class ifcP2pOEservSess implements ifcDn, Comparable<ifcP2pOEservSess> {

    private ifcP2pOEserv lower;

    private counter cntr = new counter();

    private ifcUp upper = new ifcNull();

    /**
     * interface
     */
    protected cfgIfc ifc;

    /**
     * client address
     */
    protected addrMac mac;

    /**
     * session id
     */
    protected int sessid;

    public int compareTo(ifcP2pOEservSess o) {
        return mac.compareTo(o.mac);
    }

    /**
     * create instance
     *
     * @param parent parent
     * @param addr address
     */
    public ifcP2pOEservSess(ifcP2pOEserv parent, addrMac addr) {
        lower = parent;
        mac = addr.copyBytes();
    }

    public String toString() {
        return "pppoeS with " + mac;
    }

    /**
     * start upper layer
     */
    public void startUpper() {
        upper = new ifcNull();
        ifc = lower.clnIfc.cloneStart(this);
    }

    public counter getCounter() {
        return cntr;
    }

    public int getMTUsize() {
        return lower.lower.getMTUsize() - packPppOE.size;
    }

    public long getBandwidth() {
        return lower.lower.getBandwidth();
    }

    public state.states getState() {
        return state.states.up;
    }

    public void closeDn() {
        lower.delClient(this);
        upper.closeUp();
        if (ifc != null) {
            ifc.cloneStop();
        }
        if (debugger.ifcP2pOEserv) {
            logger.debug("tx padt");
        }
        packHolder pck = new packHolder(true, true);
        pck.clear();
        pck.putStart();
        pck.ETHtrg.setAddr(mac);
        pck.ETHsrc.setAddr(lower.hwaddr);
        packPppOE.updateHeader(pck, packPppOE.codePadT, sessid);
        lower.lower.sendPack(pck);
    }

    public void flapped() {
        closeDn();
    }

    public void setUpper(ifcUp server) {
        upper = server;
        upper.setParent(this);
    }

    public addrType getHwAddr() {
        return lower.hwaddr.copyBytes();
    }

    public void setFilter(boolean promisc) {
    }

    /**
     * send to upper layer
     *
     * @param pck packet to send
     */
    public void send2upper(packHolder pck) {
        cntr.rx(pck);
        pck.putStart();
        pck.msbPutW(0, ifcPpp.preamble);
        pck.putSkip(2);
        pck.merge2beg();
        upper.recvPack(pck);
    }

    public void sendPack(packHolder pck) {
        cntr.tx(pck);
        pck.merge2beg();
        pck.getSkip(2);
        pck.ETHtrg.setAddr(mac);
        pck.ETHsrc.setAddr(lower.hwaddr);
        packPppOE.updateHeader(pck, packPppOE.codeData, sessid);
        lower.lower.sendPack(pck);
    }

    /**
     * get session info
     *
     * @param clnt mac address
     * @return session id, negative if down
     */
    public int getSession(addrMac clnt) {
        clnt.setAddr(mac);
        return sessid;
    }

    /**
     * get lower info
     *
     * @param srvr mac address
     * @return lower handler
     */
    public cfgIfc getLower(addrMac srvr) {
        srvr.setAddr(lower.hwaddr);
        return lower.pktIfc;
    }

}
