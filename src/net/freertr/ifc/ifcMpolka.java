package net.freertr.ifc;

import net.freertr.addr.addrIP;
import net.freertr.cry.cryHashCrc16;
import net.freertr.cry.cryPoly;
import net.freertr.ip.ipFwd;
import net.freertr.ip.ipMpls;
import net.freertr.pack.packHolder;
import net.freertr.tab.tabGen;
import net.freertr.tab.tabIndex;
import net.freertr.util.bits;
import net.freertr.util.counter;
import net.freertr.util.debugger;
import net.freertr.util.logger;
import net.freertr.util.state;

/**
 * polka encapsulation handler
 *
 * @author matecsaba
 */
public class ifcMpolka implements ifcUp {

    /**
     * ethertype
     */
    public final static int type = 0x8841;

    /**
     * size
     */
    public final static int size = 20;

    /**
     * counter of this interface
     */
    public counter cntr = new counter();

    /**
     * server that sends our packets
     */
    public ifcDn lower = new ifcNull();

    /**
     * local id
     */
    public final int localId;

    /**
     * base of crc values
     */
    public final int crcBase;

    /**
     * base of crc values
     */
    public final int crcMax;

    /**
     * crc calculator
     */
    public final cryHashCrc16 hasher;

    /**
     * crc calculator
     */
    public final cryPoly[] coeffs;

    /**
     * ipv4 forwarder
     */
    public ipFwd fwd4;

    /**
     * ipv6 forwarder
     */
    public ipFwd fwd6;

    /**
     * create instance
     *
     * @param id local id
     * @param bas crc base
     * @param max crc max
     */
    public ifcMpolka(int id, int bas, int max) {
        localId = id;
        crcBase = bas;
        crcMax = max;
        coeffs = ifcPolka.generatePolynomial(bas, max);
        hasher = new cryHashCrc16(coeffs[id].intCoeff(), 0, 0, false);
    }

    /**
     * decode routeid
     *
     * @param pck packet to decode
     * @return next node id
     */
    public int decodeRouteId(packHolder pck) {
        return ifcPolka.decodeRouteIdCrc(new cryHashCrc16(hasher), pck.NSHmdv);
    }

    /**
     * parse polka header
     *
     * @param pck packet to parse
     * @return false on success, true on error
     */
    public static boolean parseMpolkaHeader(packHolder pck) {
        if (pck.dataSize() < size) {
            return true;
        }
        if (pck.getByte(0) != 0) { // version
            return true;
        }
        pck.NSHttl = pck.getByte(1); // ttl
        pck.IPprt = pck.msbGetW(2); // protocol
        pck.NSHmdv = new byte[16];
        pck.getCopy(pck.NSHmdv, 0, 4, pck.NSHmdv.length);
        pck.getSkip(size);
        return false;
    }

    /**
     * create polka header
     *
     * @param pck packet to parse
     */
    public static void createMpolkaHeader(packHolder pck) {
        pck.putByte(0, 0); // version
        pck.putByte(1, pck.NSHttl); // ttl
        pck.msbPutW(2, pck.IPprt); // protocol
        pck.putCopy(pck.NSHmdv, 0, 4, pck.NSHmdv.length);
        pck.putSkip(size);
        pck.merge2beg();
    }

    public void setParent(ifcDn parent) {
        lower = parent;
    }

    public counter getCounter() {
        return cntr;
    }

    public void closeUp() {
    }

    public void setState(state.states stat) {
    }

    public String toString() {
        return "mpolka on " + lower;
    }

    public void recvPack(packHolder pck) {
        cntr.rx(pck);
        if (pck.msbGetW(0) != type) {
            return;
        }
        pck.getSkip(2);
        if (ifcPolka.parsePolkaHeader(pck)) {
            cntr.drop(pck, counter.reasons.badHdr);
            return;
        }
        if (debugger.ifcPolkaEvnt) {
            logger.debug("rx ttl=" + pck.NSHttl + " proto=" + pck.IPprt + " route=" + bits.byteDump(pck.NSHmdv, 0, -1));
        }
        ipMpls.gotMpolkaPack(this, fwd4, fwd6, pck);
    }

    /**
     * encode routeid polynomial
     *
     * @param s switch coefficients
     * @param o switches to visit
     * @return routeid
     */
    public static byte[] encodeRouteId(cryPoly[] s, tabGen<tabIndex<addrIP>> o) {
        cryPoly pm = new cryPoly(1);
        for (int i = 0; i < o.size(); i++) {
            pm = pm.mul(s[o.get(i).index]);
        }
        cryPoly r = new cryPoly(0);
        for (int i = 0; i < o.size(); i++) {
            tabIndex<addrIP> c = o.get(i);
            cryPoly soi = s[c.index];
            cryPoly mi = pm.div(soi)[0];
            cryPoly ni = mi.modInv(soi)[0];
            r = r.add(new cryPoly(c.bitmap).mul(mi).mul(ni));
        }
        r = r.div(pm)[1];
        byte[] b = r.getCoeff().toByteArray();
        byte[] p = new byte[16 - b.length];
        return bits.byteConcat(p, b);
    }

    /**
     * send one packet
     *
     * @param pck packet to send
     */
    public void send2eth(packHolder pck) {
        if (debugger.ifcMpolkaEvnt) {
            logger.debug("tx ttl=" + pck.NSHttl + " proto=" + pck.IPprt + " route=" + bits.byteDump(pck.NSHmdv, 0, -1));
        }
        lower.sendPack(pck);
    }

}
