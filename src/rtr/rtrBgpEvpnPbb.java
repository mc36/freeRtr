package rtr;

import addr.addrMac;
import ifc.ifcDn;
import ifc.ifcDot1ah;
import ifc.ifcEther;
import ifc.ifcUp;
import pack.packHolder;
import util.counter;
import util.state;

/**
 * bgp4 evpn/pbb receiver
 *
 * @author matecsaba
 */
public class rtrBgpEvpnPbb implements ifcUp {

    private rtrBgp parent;

    /**
     * counter
     */
    public counter cntr = new counter();

    /**
     * create new instance
     *
     * @param p parent to use
     */
    public rtrBgpEvpnPbb(rtrBgp p) {
        parent = p;
    }

    /**
     * set parent
     *
     * @param parent parent
     */
    public void setParent(ifcDn parent) {
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
     * received packet
     *
     * @param pck
     */
    public void recvPack(packHolder pck) {
        pck.getSkip(4);
        ifcEther.parseETHheader(pck, false);
        addrMac src = pck.ETHsrc.copyBytes();
        if (ifcDot1ah.parseHeader(pck)) {
            return;
        }
        pck.getSkip(ifcDot1ah.size);
        rtrBgpEvpn evpn = new rtrBgpEvpn(null);
        evpn.id = pck.ETHvlan;
        evpn = parent.evpn.find(evpn);
        if (evpn == null) {
            return;
        }
        if (src.compare(src, evpn.bbmac) == 0) {
            return;
        }
        rtrBgpEvpnPeer peer = new rtrBgpEvpnPeer(null);
        peer.bbmac = src;
        peer = evpn.peers.find(peer);
        if (peer == null) {
            return;
        }
        peer.brdg.recvPack(pck);
    }

}
