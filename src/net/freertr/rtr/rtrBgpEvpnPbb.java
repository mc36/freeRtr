package net.freertr.rtr;

import net.freertr.addr.addrMac;
import net.freertr.ifc.ifcDn;
import net.freertr.ifc.ifcDot1ah;
import net.freertr.ifc.ifcEther;
import net.freertr.ifc.ifcUp;
import net.freertr.pack.packHolder;
import net.freertr.util.counter;
import net.freertr.util.state;

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
     * @param pck packet
     */
    public void recvPack(packHolder pck) {
        pck.getSkip(4);
        ifcEther.parseETHheader(pck, false);
        addrMac src = pck.ETHsrc.copyBytes();
        if (new ifcDot1ah().parseHeader(pck)) {
            cntr.drop(pck, counter.reasons.badHdr);
            return;
        }
        rtrBgpEvpn evpn = new rtrBgpEvpn(null);
        evpn.id = pck.ETHvlan;
        evpn = parent.evpn.find(evpn);
        if (evpn == null) {
            cntr.drop(pck, counter.reasons.badAddr);
            return;
        }
        if (src.compare(src, evpn.bbmac) == 0) {
            cntr.drop(pck, counter.reasons.badAddr);
            return;
        }
        rtrBgpEvpnPeer peer = new rtrBgpEvpnPeer(null);
        peer.bbmac = src;
        peer = evpn.peers.find(peer);
        if (peer == null) {
            cntr.drop(pck, counter.reasons.badAddr);
            return;
        }
        peer.recvPack(pck);
    }

}
