package rtr;

import ifc.ifcDn;
import ifc.ifcEther;
import ifc.ifcUp;
import pack.packHolder;
import util.counter;
import util.state;

/**
 * bgp4 evpn/cmac receiver
 *
 * @author matecsaba
 */
public class rtrBgpEvpnCmac implements ifcUp {

    private rtrBgpEvpn parent;

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
            return;
        }
        peer.brdg.recvPack(pck);
    }

}
