package prt;

import addr.addrIP;
import addr.addrMac;
import addr.addrType;
import ifc.ifcDn;
import ifc.ifcNull;
import ifc.ifcUp;
import ip.ipFwd;
import ip.ipFwdIface;
import ip.ipIfc4;
import ip.ipIfc6;
import ip.ipMpls;
import ip.ipPrt;
import pack.packHolder;
import util.counter;
import util.state;

/**
 * mpls in ip (rfc4023) client
 *
 * @author matecsaba
 */
public class prtMplsIp implements ipPrt, ifcDn {

    /**
     * protocol number
     */
    public static final int prot = 137;

    /**
     * upper layer
     */
    public ifcUp upper = new ifcNull();

    /**
     * sending ttl value, -1 means maps out
     */
    public int sendingTTL = 255;

    /**
     * sending tos value, -1 means maps out
     */
    public int sendingTOS = -1;

    /**
     * counter
     */
    public counter cntr = new counter();

    private ipFwd lower;

    private addrIP remote;

    private ipFwdIface sendingIfc;

    /**
     * initialize context
     *
     * @param parent forwarder of encapsulated packets
     */
    public prtMplsIp(ipFwd parent) {
        lower = parent;
    }

    /**
     * set target of tunnel
     *
     * @param ifc interface to source from
     * @param trg ip address of remote
     * @param reg register to lower layer
     * @return false if successful, true if error happened
     */
    public boolean setEndpoints(ipFwdIface ifc, addrIP trg, boolean reg) {
        if (!reg) {
            remote = trg;
            sendingIfc = ifc;
            return false;
        }
        if (sendingIfc != null) {
            lower.protoDel(this, sendingIfc, remote);
        }
        remote = trg;
        sendingIfc = ifc;
        return lower.protoAdd(this, sendingIfc, remote);
    }

    public String toString() {
        return "mplsip to " + remote;
    }

    /**
     * guess label from ethertype
     *
     * @param pck packet to parse
     * @return true on error, false on success
     */
    public static boolean ethtyp2mpls(packHolder pck) {
        int i = pck.msbGetW(0); // ethertype
        pck.getSkip(2);
        switch (i) {
            case ipMpls.typeU:
            case ipMpls.typeM:
                return false;
            case ipIfc4.type:
                pck.MPLSlabel = ipMpls.labelExp4;
                ipMpls.beginMPLSfields(pck, false);
                ipMpls.createMPLSheader(pck);
                return false;
            case ipIfc6.type:
                pck.MPLSlabel = ipMpls.labelExp6;
                ipMpls.beginMPLSfields(pck, false);
                ipMpls.createMPLSheader(pck);
                return false;
            default:
                return true;
        }
    }

    /**
     * guess ethertype from label
     *
     * @param pck packet to parse
     * @return true on error, false on success
     */
    public static boolean mpls2ethtyp(packHolder pck) {
        if (ipMpls.parseMPLSheader(pck)) {
            return true;
        }
        int i;
        switch (pck.MPLSlabel) {
            case ipMpls.labelExp4:
                i = ipIfc4.type;
                break;
            case ipMpls.labelExp6:
                i = ipIfc6.type;
                break;
            default:
                pck.getSkip(-ipMpls.sizeL);
                i = ipMpls.typeU;
                break;
        }
        pck.msbPutW(0, i); // ethertype
        pck.putSkip(2);
        pck.merge2beg();
        return false;
    }

    /**
     * get hw address
     *
     * @return hw address
     */
    public addrType getHwAddr() {
        return addrMac.getRandom();
    }

    /**
     * set filter
     *
     * @param promisc promiscous mode
     */
    public void setFilter(boolean promisc) {
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
     * close interface
     */
    public void closeDn() {
        if (sendingIfc != null) {
            lower.protoDel(this, sendingIfc, remote);
        }
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
     * get counter
     *
     * @return counter
     */
    public counter getCounter() {
        return cntr;
    }

    /**
     * get mtu size
     *
     * @return mtu size
     */
    public int getMTUsize() {
        return sendingIfc.mtu;
    }

    /**
     * get bandwidth
     *
     * @return bandwidth
     */
    public long getBandwidth() {
        return sendingIfc.bandwidth;
    }

    /**
     * send packet
     *
     * @param pck packet
     */
    public void sendPack(packHolder pck) {
        pck.merge2beg();
        if (sendingIfc == null) {
            return;
        }
        if (ethtyp2mpls(pck)) {
            return;
        }
        cntr.tx(pck);
        pck.putDefaults();
        if (sendingTTL >= 0) {
            pck.IPttl = sendingTTL;
        }
        if (sendingTOS >= 0) {
            pck.IPtos = sendingTOS;
        }
        pck.IPprt = prot;
        pck.IPsrc.setAddr(sendingIfc.addr);
        pck.IPtrg.setAddr(remote);
        lower.protoPack(sendingIfc, pck);
    }

    /**
     * get protocol number
     *
     * @return number
     */
    public int getProtoNum() {
        return prot;
    }

    /**
     * close interface
     *
     * @param iface interface
     */
    public void closeUp(ipFwdIface iface) {
        upper.closeUp();
    }

    /**
     * set state
     *
     * @param iface interface
     * @param stat state
     */
    public void setState(ipFwdIface iface, state.states stat) {
        if (iface.ifwNum != sendingIfc.ifwNum) {
            return;
        }
        upper.setState(stat);
    }

    /**
     * received packet
     *
     * @param rxIfc interface
     * @param pck packet
     */
    public void recvPack(ipFwdIface rxIfc, packHolder pck) {
        if (mpls2ethtyp(pck)) {
            return;
        }
        cntr.rx(pck);
        upper.recvPack(pck);
    }

    /**
     * alert packet
     *
     * @param rxIfc interface
     * @param pck packet
     * @return false if success, true if error
     */
    public boolean alertPack(ipFwdIface rxIfc, packHolder pck) {
        return true;
    }

    /**
     * error packet
     *
     * @param err error code
     * @param rtr address
     * @param rxIfc interface
     * @param pck packet
     */
    public void errorPack(counter.reasons err, addrIP rtr, ipFwdIface rxIfc, packHolder pck) {
    }

}
