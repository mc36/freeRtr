package org.freertr.ifc;

import java.math.BigInteger;
import org.freertr.addr.addrIP;
import org.freertr.cry.cryHashCrc16;
import org.freertr.cry.cryHashCrcPoly;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipMpls;
import org.freertr.pack.packHolder;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabIndex;
import org.freertr.user.userFormat;
import org.freertr.util.bits;
import org.freertr.util.counter;
import org.freertr.util.debugger;
import org.freertr.util.logger;
import org.freertr.util.state;

/**
 * polka encapsulation handler
 *
 * @author matecsaba
 */
public class ifcPolka implements ifcUp {

    /**
     * ethertype
     */
    public final static int type = 0x8842;

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
    public final cryHashCrcPoly[] coeffs;

    /**
     * ipv4 forwarder
     */
    public ipFwd fwd4;

    /**
     * ipv6 forwarder
     */
    public ipFwd fwd6;

    /**
     * ethtyp forwarder
     */
    public final ifcEthTyp fwdE;

    /**
     * mpls forwarder
     */
    public ipMpls fwdM;

    /**
     * create instance
     *
     * @param e ethtyp
     * @param id local id
     * @param bas crc base
     * @param max crc max
     */
    public ifcPolka(ifcEthTyp e, int id, int bas, int max) {
        fwdE = e;
        localId = id;
        crcBase = bas;
        crcMax = max;
        coeffs = generatePolynomial(bas, max);
        hasher = new cryHashCrc16(coeffs[id].intCoeff(), 0, 0, false);
    }

    private static boolean checkPolynomial(cryHashCrcPoly[] s, int o, cryHashCrcPoly f) {
        for (int i = 0; i < o; i++) {
            cryHashCrcPoly[] r = s[i].modInv(f);
            if (r == null) {
                return true;
            }
            if (r[0] == null) {
                return true;
            }
            if (r[1].intCoeff() > 1) {
                return true;
            }
        }
        return false;
    }

    /**
     * generate polynominals
     *
     * @param f first polynominal
     * @param n number of entries
     * @return entries generated
     */
    public static cryHashCrcPoly[] generatePolynomial(int f, int n) {
        cryHashCrcPoly[] s = new cryHashCrcPoly[n];
        s[0] = new cryHashCrcPoly(f);
        for (int i = 1; i < s.length; i++) {
            cryHashCrcPoly p;
            for (;;) {
                f++;
                p = new cryHashCrcPoly(f);
                if (checkPolynomial(s, i, p)) {
                    continue;
                }
                break;
            }
            s[i] = p;
        }
        return s;
    }

    /**
     * get next value from a route
     *
     * @param o switches to visit
     * @param i switch number
     * @return next value
     */
    public static int routeNextValue(int[] o, int i) {
        if (i < (o.length - 1)) {
            return o[i + 1];
        }
        return 0;
    }

    /**
     * encode routeid polynomial
     *
     * @param s switch coefficients
     * @param o switches to visit
     * @return routeid
     */
    public static byte[] encodeRouteIdUni(cryHashCrcPoly[] s, int[] o) {
        cryHashCrcPoly pm = new cryHashCrcPoly(1);
        for (int i = 0; i < o.length; i++) {
            pm = pm.mul(s[o[i]]);
        }
        cryHashCrcPoly r = new cryHashCrcPoly(0);
        for (int i = 0; i < o.length; i++) {
            cryHashCrcPoly soi = s[o[i]];
            cryHashCrcPoly mi = pm.div(soi)[0];
            cryHashCrcPoly ni = mi.modInv(soi)[0];
            int p = routeNextValue(o, i);
            r = r.add(new cryHashCrcPoly(p).mul(mi).mul(ni));
        }
        r = r.div(pm)[1];
        byte[] b = r.getCoeff().toByteArray();
        byte[] p = new byte[16 - b.length];
        return bits.byteConcat(p, b);
    }

    /**
     * encode routeid polynomial
     *
     * @param s switch coefficients
     * @param o switches to visit
     * @return routeid
     */
    public static byte[] encodeRouteIdMul(cryHashCrcPoly[] s, tabGen<tabIndex<addrIP>> o) {
        cryHashCrcPoly pm = new cryHashCrcPoly(1);
        for (int i = 0; i < o.size(); i++) {
            pm = pm.mul(s[o.get(i).index]);
        }
        cryHashCrcPoly r = new cryHashCrcPoly(0);
        for (int i = 0; i < o.size(); i++) {
            tabIndex<addrIP> c = o.get(i);
            cryHashCrcPoly soi = s[c.index];
            cryHashCrcPoly mi = pm.div(soi)[0];
            cryHashCrcPoly ni = mi.modInv(soi)[0];
            r = r.add(new cryHashCrcPoly(c.bitmap).mul(mi).mul(ni));
        }
        r = r.div(pm)[1];
        byte[] b = r.getCoeff().toByteArray();
        byte[] p = new byte[16 - b.length];
        return bits.byteConcat(p, b);
    }

    /**
     * decode routeid polynomial
     *
     * @param s switch coefficients
     * @param v routeid
     * @return next node id
     */
    public static int[] decodeRouteIdPoly(cryHashCrcPoly[] s, byte[] v) {
        cryHashCrcPoly r = new cryHashCrcPoly(new BigInteger(v));
        int[] o = new int[s.length];
        for (int i = 0; i < s.length; i++) {
            cryHashCrcPoly c = r.div(s[i])[1];
            o[i] = c.intCoeff();
        }
        return o;
    }

    /**
     * decode routeid with crc
     *
     * @param h hasher
     * @param v routeid
     * @return next node id
     */
    public static int decodeRouteIdCrc(cryHashCrc16 h, byte[] v) {
        h.init();
        h.update(v, 0, v.length - 2);
        return bits.msbGetW(v, v.length - 2) ^ h.getCrc();
    }

    /**
     * decode routeid with crc
     *
     * @param s switch coefficients
     * @param v routeid
     * @return next node id
     */
    public static int[] decodeRouteIdCrc(cryHashCrcPoly[] s, byte[] v) {
        int[] o = new int[s.length];
        for (int i = 0; i < s.length; i++) {
            cryHashCrc16 h = new cryHashCrc16(s[i].intCoeff(), 0, 0, false);
            o[i] = decodeRouteIdCrc(h, v);
        }
        return o;
    }

    /**
     * decode routeid
     *
     * @param pck packet to decode
     * @return next node id
     */
    public int decodeRouteId(packHolder pck) {
        return decodeRouteIdCrc(new cryHashCrc16(hasher), pck.NSHmdv);
    }

    /**
     * parse polka header
     *
     * @param pck packet to parse
     * @return false on success, true on error
     */
    public static boolean parsePolkaHeader(packHolder pck) {
        if (pck.dataSize() < size) {
            return true;
        }
        pck.NSHmdt = pck.getByte(0); // version
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
    public static void createPolkaHeader(packHolder pck) {
        pck.putByte(0, pck.NSHmdt); // version
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
        return "polka on " + lower;
    }

    public void recvPack(packHolder pck) {
        cntr.rx(pck);
        if (pck.msbGetW(0) != type) {
            return;
        }
        pck.getSkip(2);
        if (parsePolkaHeader(pck)) {
            cntr.drop(pck, counter.reasons.badHdr);
            return;
        }
        if (debugger.ifcPolkaEvnt) {
            logger.debug("rx ttl=" + pck.NSHttl + " proto=" + pck.IPprt + " route=" + bits.byteDump(pck.NSHmdv, 0, -1));
        }
        ipMpls.gotPolkaPack(this, fwd4, fwd6, fwdE, fwdM, pck);
    }

    /**
     * send one packet
     *
     * @param pck packet to send
     */
    public void send2eth(packHolder pck) {
        if (debugger.ifcPolkaEvnt) {
            logger.debug("tx ttl=" + pck.NSHttl + " proto=" + pck.IPprt + " route=" + bits.byteDump(pck.NSHmdv, 0, -1));
        }
        lower.sendPack(pck);
    }

    /**
     * get show
     *
     * @param coeffs coefficients
     * @return show
     */
    public static userFormat getShow(cryHashCrcPoly[] coeffs) {
        userFormat l = new userFormat("|", "index|deg|coeff|poly");
        for (int i = 0; i < coeffs.length; i++) {
            l.add(i + "|" + coeffs[i].getDegree() + "|" + bits.toHexD(coeffs[i].intCoeff()) + "|" + coeffs[i]);
        }
        return l;
    }

}
