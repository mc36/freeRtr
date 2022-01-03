package net.freertr.prt;

import net.freertr.addr.addrEmpty;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrPrefix;
import net.freertr.addr.addrType;
import net.freertr.ifc.ifcDn;
import net.freertr.ifc.ifcEthTyp;
import net.freertr.ifc.ifcNull;
import net.freertr.ifc.ifcUp;
import net.freertr.ip.ipCor6;
import net.freertr.ip.ipFwd;
import net.freertr.ip.ipIfc6;
import net.freertr.ip.ipMpls;
import net.freertr.pack.packHolder;
import net.freertr.util.bits;
import net.freertr.util.counter;
import net.freertr.util.state;

/**
 * srv6 protocol translation
 *
 * @author matecsaba
 */
public class prtSrv6 implements ifcDn {

    private counter cntr = new counter();

    private ifcUp upper = new ifcNull();

    private ifcEthTyp ethtyp;

    private final ipCor6 ip6;

    private ipFwd fwd4;

    private ipFwd fwd6;

    private addrPrefix<addrIP> prf;

    /**
     * get counter
     *
     * @return counter
     */
    public counter getCounter() {
        return cntr;
    }

    /**
     * initialize context
     *
     * @param adr address to translate
     * @param et forwarder
     * @param f4 forwarder
     * @param f6 forwarder
     */
    public prtSrv6(addrIP adr, ifcEthTyp et, ipFwd f4, ipFwd f6) {
        prf = new addrPrefix<addrIP>(adr, addrIP.size * 4);
        ethtyp = et;
        fwd4 = f4;
        fwd6 = f6;
        ip6 = new ipCor6();
    }

    /**
     * close interface
     */
    public void closeDn() {
    }

    /**
     * flap interface
     */
    public void flapped() {
    }

    /**
     * set upper layer
     *
     * @param server upper layer
     */
    public void setUpper(ifcUp server) {
        upper = server;
        upper.setParent(this);
    }

    /**
     * get state
     *
     * @return state
     */
    public state.states getState() {
        return state.states.up;
    }

    /**
     * set filter
     *
     * @param promisc promiscous mode
     */
    public void setFilter(boolean promisc) {
    }

    /**
     * get hw address
     *
     * @return hw address
     */
    public addrType getHwAddr() {
        return new addrEmpty();
    }

    /**
     * get mtu size
     *
     * @return mtu size
     */
    public int getMTUsize() {
        return 1500;
    }

    /**
     * get bandwidth
     *
     * @return bandwidth
     */
    public long getBandwidth() {
        return 8000000;
    }

    /**
     * send packet
     *
     * @param pck packet
     */
    public void sendPack(packHolder pck) {
        cntr.tx(pck);
        if (pck.msbGetW(0) != ipIfc6.type) {
            cntr.drop(pck, counter.reasons.badCod);
            return;
        }
        pck.getSkip(2);
        if (ip6.parseIPheader(pck, true)) {
            cntr.drop(pck, counter.reasons.badHdr);
            return;
        }
        pck.getSkip(pck.IPsiz);
        if (!prf.matches(pck.IPtrg)) {
            cntr.drop(pck, counter.reasons.badNet);
            return;
        }
        ipMpls.beginMPLSfields(pck, true);
        pck.MPLSlabel = bits.msbGetD(pck.IPtrg.getBytes(), 12);
        ipMpls.createMPLSheader(pck);
        ipMpls.gotMplsPack(fwd4, fwd6, ethtyp, false, pck);
    }

}
