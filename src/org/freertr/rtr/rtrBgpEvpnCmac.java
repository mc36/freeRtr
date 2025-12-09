package org.freertr.rtr;

import org.freertr.ifc.ifcDn;
import org.freertr.ifc.ifcEther;
import org.freertr.ifc.ifcUp;
import org.freertr.pack.packHolder;
import org.freertr.util.counter;
import org.freertr.util.state;

/**
 * bgp4 evpn/cmac receiver
 *
 * @author matecsaba
 */
public class rtrBgpEvpnCmac implements ifcUp {

    private final rtrBgpEvpn parent;

    /**
     * counter
     */
    public counter cntr = new counter();

    /**
     * create new instance
     *
     * @param p parent to use
     */
    public rtrBgpEvpnCmac(rtrBgpEvpn p) {
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
        ifcEther.parseETHheader(pck, false);
        rtrBgpEvpnPeer peer = new rtrBgpEvpnPeer(null);
        peer.bbmac = pck.ETHsrc;
        peer = parent.peers.find(peer);
        if (peer == null) {
            cntr.drop(pck, counter.reasons.badAddr);
            return;
        }
        peer.recvPack(pck);
    }

}
