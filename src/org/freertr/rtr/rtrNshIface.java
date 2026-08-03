package org.freertr.rtr;

import org.freertr.addr.addrIP;
import org.freertr.ifc.ifcNshFwd;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipFwdIface;
import org.freertr.ip.ipMpls;
import org.freertr.ip.ipPrt;
import org.freertr.pack.packHolder;
import org.freertr.util.counter;
import org.freertr.util.debugger;
import org.freertr.util.logger;
import org.freertr.util.state;

/**
 * network service header (rfc9491) handler
 *
 * @author matecsaba
 */
public class rtrNshIface implements ipPrt {

    /**
     * protocol
     */
    public final static int protoNum = 145;

    private final ipFwd fwdCore;

    private final ipFwdIface iface;

    private counter cntr = new counter();

    /**
     * create new instance
     *
     * @param fwd forwarder
     * @param ifc interface
     */
    public rtrNshIface(ipFwd fwd, ipFwdIface ifc) {
        fwdCore = fwd;
        iface = ifc;
    }

    /**
     * unregister from ip
     */
    public void unregister2ip() {
        if (debugger.rtrNshTraf) {
            logger.debug("unregister " + iface);
        }
        fwdCore.protoDel(this, iface, null);
    }

    /**
     * register to ip
     */
    public void register2ip() {
        if (debugger.rtrNshTraf) {
            logger.debug("register " + iface);
        }
        fwdCore.protoAdd(this, iface, null);
    }

    public String toString() {
        return "nsh";
    }

    /**
     * get protocol number
     *
     * @return number
     */
    public int getProtoNum() {
        return protoNum;
    }

    /**
     * close interface
     *
     * @param iface interface
     */
    public void closeUp(ipFwdIface iface) {
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
     * set state
     *
     * @param iface interface
     * @param stat state
     */
    public void setState(ipFwdIface iface, state.states stat) {
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

    /**
     * received packet
     *
     * @param rxIfc interface
     * @param pck packet
     */
    public void recvPack(ipFwdIface rxIfc, packHolder pck) {
        if (ifcNshFwd.parseNSHheader(pck)) {
            cntr.drop(pck, counter.reasons.badHdr);
            return;
        }
        cntr.rx(pck);
        ipMpls.gotNshPack(pck);
    }

}
