package org.freertr.tab;

import org.freertr.addr.addrIP;
import org.freertr.cfg.cfgAll;
import org.freertr.pack.packHolder;
import org.freertr.prt.prtDccp;
import org.freertr.prt.prtLudp;
import org.freertr.prt.prtSctp;
import org.freertr.prt.prtTcp;
import org.freertr.prt.prtUdp;
import org.freertr.util.bits;
import org.freertr.util.counter;

/**
 * represents one nat entry (source/target, orig/new)
 *
 * @author matecsaba
 */
public class tabNatTraN implements Comparable<tabNatTraN> {

    /**
     * create instance
     */
    public tabNatTraN() {
    }

    /**
     * matching protocol
     */
    public int protocol = -1;

    /**
     * original source address
     */
    public addrIP origSrcAddr;

    /**
     * original target address
     */
    public addrIP origTrgAddr;

    /**
     * original source port
     */
    public int origSrcPort = -1;

    /**
     * original target port
     */
    public int origTrgPort = -1;

    /**
     * new source address
     */
    public addrIP newSrcAddr;

    /**
     * new target address
     */
    public addrIP newTrgAddr;

    /**
     * new source port
     */
    public int newSrcPort = -1;

    /**
     * new target port
     */
    public int newTrgPort = -1;

    /**
     * time when last used
     */
    public long lastUsed;

    /**
     * time when created
     */
    public long created;

    /**
     * timeout on this entry
     */
    public int timeout;

    /**
     * reverse translation
     */
    public tabNatTraN reverse;

    /**
     * counter
     */
    public counter cntr = new counter();

    /**
     * hardware counter
     */
    public counter hwCntr;

    /**
     * log translations
     */
    public boolean logEnd;

    /**
     * reverse entry
     *
     * @return entry reversed
     */
    public tabNatTraN reverseEntry() {
        tabNatTraN n = new tabNatTraN();
        n.lastUsed = lastUsed;
        n.created = created;
        n.timeout = timeout;
        n.protocol = protocol;
        n.origTrgAddr = newSrcAddr.copyBytes();
        n.origSrcAddr = newTrgAddr.copyBytes();
        n.origTrgPort = newSrcPort;
        n.origSrcPort = newTrgPort;
        n.newTrgAddr = origSrcAddr.copyBytes();
        n.newSrcAddr = origTrgAddr.copyBytes();
        n.newTrgPort = origSrcPort;
        n.newSrcPort = origTrgPort;
        n.reverse = this;
        reverse = n;
        return n;
    }

    /**
     * pick random source port
     *
     * @param n reverse sentry
     */
    public void pickRandomSrcPort(tabNatTraN n) {
        newSrcPort = bits.random(cfgAll.tcpRangeMin, cfgAll.tcpRangeMax);
        n.origTrgPort = newSrcPort;
    }

    /**
     * check if duplicate check needed
     *
     * @return true if yes, false if not
     */
    public boolean needDuplicateCheck() {
        switch (protocol) {
            case prtTcp.protoNum:
            case prtUdp.protoNum:
            case prtLudp.protoNum:
            case prtSctp.protoNum:
            case prtDccp.protoNum:
                return true;
            default:
                return false;
        }
    }

    public String toString() {
        String a = "";
        String s = "";
        if (hwCntr != null) {
            a = "+" + hwCntr.packRx;
            s = "+" + hwCntr.byteRx;
        }
        return protocol + "|" + origSrcAddr + " " + origSrcPort + "|" + origTrgAddr + " " + origTrgPort + "|" + newSrcAddr + " " + newSrcPort + "|" + newTrgAddr + " " + newTrgPort + "|" + bits.timePast(created) + "|" + bits.timePast(lastUsed) + "|" + bits.timeDump(timeout / 1000) + "|" + cntr.packRx + a + "|" + cntr.byteRx + s;
    }

    public int compareTo(tabNatTraN o) {
        if (protocol < o.protocol) {
            return -1;
        }
        if (protocol > o.protocol) {
            return +1;
        }
        if (origSrcPort < o.origSrcPort) {
            return -1;
        }
        if (origSrcPort > o.origSrcPort) {
            return +1;
        }
        if (origTrgPort < o.origTrgPort) {
            return -1;
        }
        if (origTrgPort > o.origTrgPort) {
            return +1;
        }
        int i = origSrcAddr.compareTo(o.origSrcAddr);
        if (i != 0) {
            return i;
        }
        return origTrgAddr.compareTo(o.origTrgAddr);
    }

    /**
     * generate entry
     *
     * @param pck packet to parse
     * @return entry generated
     */
    public static tabNatTraN fromPack(packHolder pck) {
        tabNatTraN n = new tabNatTraN();
        n.lastUsed = bits.getTime();
        n.created = n.lastUsed;
        n.protocol = pck.IPprt;
        n.origSrcPort = pck.UDPsrc;
        n.origTrgPort = pck.UDPtrg;
        n.origSrcAddr = pck.IPsrc;
        n.origTrgAddr = pck.IPtrg;
        return n;
    }

    /**
     * generate entry
     *
     * @param pck packet to parse
     * @return entry generated
     */
    public static tabNatTraN fromError(packHolder pck) {
        tabNatTraN n = new tabNatTraN();
        n.lastUsed = bits.getTime();
        n.created = n.lastUsed;
        n.protocol = pck.IPprt;
        n.origSrcPort = pck.UDPtrg;
        n.origTrgPort = pck.UDPsrc;
        n.origSrcAddr = pck.IPtrg;
        n.origTrgAddr = pck.IPsrc;
        return n;
    }

    /**
     * update one packet
     *
     * @param pck packet to update
     */
    public void updatePack(packHolder pck) {
        cntr.rx(pck);
        pck.IPsrc.fromBuf(newSrcAddr.getBytes(), 0);
        pck.IPtrg.fromBuf(newTrgAddr.getBytes(), 0);
        pck.UDPsrc = newSrcPort;
        pck.UDPtrg = newTrgPort;
        pck.INTupper = -1;
    }

    /**
     * update one packet
     *
     * @param pck packet to update
     */
    public void updateError(packHolder pck) {
        cntr.rx(pck);
        pck.IPtrg.fromBuf(newSrcAddr.getBytes(), 0);
        pck.IPsrc.fromBuf(newTrgAddr.getBytes(), 0);
        pck.UDPtrg = newSrcPort;
        pck.UDPsrc = newTrgPort;
        pck.INTupper = -1;
    }

}
