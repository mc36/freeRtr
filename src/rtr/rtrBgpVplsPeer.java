package rtr;

import addr.addrEmpty;
import addr.addrIP;
import addr.addrType;
import clnt.clntMplsPwe;
import ifc.ifcBridgeIfc;
import ifc.ifcDn;
import ifc.ifcNull;
import ifc.ifcUp;
import ip.ipMpls;
import java.util.Comparator;
import pack.packHolder;
import tab.tabRtrmapN;
import util.counter;
import util.state;

/**
 * bgp4 vpls peer
 *
 * @author matecsaba
 */
public class rtrBgpVplsPeer implements ifcDn, Comparator<rtrBgpVplsPeer> {

    /**
     * peer address
     */
    protected addrIP peer;

    /**
     * needed
     */
    protected boolean needed = false;

    /**
     * pwe client
     */
    protected clntMplsPwe clnt;

    /**
     * bridge interface
     */
    protected ifcBridgeIfc brdg;

    /**
     * ve id
     */
    protected int veId;

    /**
     * ve label
     */
    protected int veLab;

    /**
     * counter
     */
    public counter cntr = new counter();

    private ifcUp upper = new ifcNull();

    private rtrBgp parentB;

    private rtrBgpVpls parentV;

    /**
     * create new instance
     *
     * @param pb parent to use
     * @param pv parent to use
     */
    protected rtrBgpVplsPeer(rtrBgp pb, rtrBgpVpls pv) {
        parentV = pv;
        parentB = pb;
    }

    public int compare(rtrBgpVplsPeer o1, rtrBgpVplsPeer o2) {
        return o1.peer.compare(o1.peer, o2.peer);
    }

    public String toString() {
        return "vpls " + peer + " " + tabRtrmapN.rd2string(parentV.id);
    }

    public addrType getHwAddr() {
        return new addrEmpty();
    }

    public void setFilter(boolean promisc) {
    }

    public state.states getState() {
        return state.states.up;
    }

    public void closeDn() {
    }

    public void flapped() {
    }

    public void setUpper(ifcUp server) {
        upper = server;
        upper.setParent(this);
    }

    public counter getCounter() {
        return cntr;
    }

    public int getMTUsize() {
        return 1500;
    }

    public long getBandwidth() {
        return 8000000;
    }

    public void sendPack(packHolder pck) {
        pck.merge2beg();
        ipMpls.beginMPLSfields(pck, false);
        pck.MPLSlabel = veLab + parentV.veId - 1;
        ipMpls.createMPLSheader(pck);
        parentB.fwdCore.mplsTxPack(peer, pck, false);
    }

}
