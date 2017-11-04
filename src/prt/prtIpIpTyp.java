package prt;

import addr.addrEmpty;
import addr.addrIP;
import addr.addrType;
import ifc.ifcDn;
import ifc.ifcEther;
import ifc.ifcNull;
import ifc.ifcUp;
import ip.ipFwd;
import ip.ipFwdIface;
import ip.ipIfc4;
import ip.ipIfc6;
import pack.packHolder;
import util.counter;
import util.state;

/**
 * handle ipip ethertyped packets
 *
 * @author matecsaba
 */
public class prtIpIpTyp implements ifcDn, ifcUp {

    private counter cntr = new counter();

    private prtIpIp ip4;

    private prtIpIp ip6;

    private ifcUp upper = new ifcNull();

    private state.states lastState = state.states.up;

    public counter getCounter() {
        return cntr;
    }

    /**
     * initialize context
     *
     * @param parent forwarder of encapsulated packets
     */
    public prtIpIpTyp(ipFwd parent) {
        ip4 = new prtIpIp(parent, 4);
        ip6 = new prtIpIp(parent, 6);
        ip4.setUpper(this);
        ip6.setUpper(this);
    }

    /**
     * set target of tunnel
     *
     * @param ifc interface to source from
     * @param trg ip address of remote
     * @return false if successful, true if error happened
     */
    public boolean setEndpoints(ipFwdIface ifc, addrIP trg) {
        boolean b = false;
        b |= ip4.setEndpoints(ifc, trg);
        b |= ip6.setEndpoints(ifc, trg);
        return b;
    }

    public void closeDn() {
        ip4.closeDn();
        ip6.closeDn();
    }

    public void flapped() {
    }

    public void closeUp() {
        upper.closeUp();
    }

    public void setUpper(ifcUp server) {
        upper = server;
        upper.setParent(this);
    }

    public state.states getState() {
        return lastState;
    }

    public void setFilter(boolean promisc) {
    }

    public void setParent(ifcDn parent) {
    }

    public void setState(state.states stat) {
        lastState = stat;
        cntr.stateChange(stat);
        upper.setState(stat);
    }

    public addrType getHwAddr() {
        return new addrEmpty();
    }

    public int getMTUsize() {
        int i = ip4.getMTUsize();
        int o = ip6.getMTUsize();
        if (o < i) {
            i = o;
        }
        return i;
    }

    public long getBandwidth() {
        long i = ip4.getBandwidth();
        long o = ip4.getBandwidth();
        if (o < i) {
            i = o;
        }
        return i;
    }

    /**
     * set sending tos value
     *
     * @param i tos value
     */
    public void setTxTOS(int i) {
        ip4.sendingTOS = i;
        ip6.sendingTOS = i;
    }

    /**
     * set sending ttl value
     *
     * @param i ttl value
     */
    public void setTxTTL(int i) {
        ip4.sendingTTL = i;
        ip6.sendingTTL = i;
    }

    public void sendPack(packHolder pck) {
        cntr.tx(pck);
        int i = pck.msbGetW(0);
        pck.getSkip(2);
        switch (i) {
            case ipIfc4.type:
                ip4.sendPack(pck);
                break;
            case ipIfc6.type:
                ip6.sendPack(pck);
                break;
            default:
                cntr.drop(pck, counter.reasons.badProto);
                return;
        }
    }

    public void recvPack(packHolder pck) {
        cntr.rx(pck);
        int i = ifcEther.guessEtherType(pck);
        if (i < 0) {
            cntr.drop(pck, counter.reasons.badVer);
            return;
        }
        pck.msbPutW(0, i);
        i = pck.headSize();
        pck.putSkip(2);
        pck.mergeHeader(-1, i);
        upper.recvPack(pck);
    }

}
