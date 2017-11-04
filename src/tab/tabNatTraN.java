package tab;

import java.util.Comparator;

import pack.packHolder;
import util.bits;
import addr.addrIP;

/**
 * represents one nat entry (source/target, orig/new)
 *
 * @author matecsaba
 */
public class tabNatTraN implements Comparator<tabNatTraN> {

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
     * reverse translation
     */
    public tabNatTraN reverse;

    /**
     * reverse entry
     *
     * @return entry reversed
     */
    public tabNatTraN reverseEntry() {
        tabNatTraN n = new tabNatTraN();
        n.lastUsed = lastUsed;
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

    public String toString() {
        return protocol + "|" + origSrcAddr + " " + origSrcPort + "|" + origTrgAddr + " " + origTrgPort + "|" + newSrcAddr + " " + newSrcPort + "|" + newTrgAddr + " " + newTrgPort;
    }

    public int compare(tabNatTraN o1, tabNatTraN o2) {
        if (o1.protocol < o2.protocol) {
            return -1;
        }
        if (o1.protocol > o2.protocol) {
            return +1;
        }
        if (o1.origSrcPort < o2.origSrcPort) {
            return -1;
        }
        if (o1.origSrcPort > o2.origSrcPort) {
            return +1;
        }
        if (o1.origTrgPort < o2.origTrgPort) {
            return -1;
        }
        if (o1.origTrgPort > o2.origTrgPort) {
            return +1;
        }
        int i = o1.origSrcAddr.compare(o1.origSrcAddr, o2.origSrcAddr);
        if (i != 0) {
            return i;
        }
        return o1.origTrgAddr.compare(o1.origTrgAddr, o2.origTrgAddr);
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
        pck.IPtrg.fromBuf(newSrcAddr.getBytes(), 0);
        pck.IPsrc.fromBuf(newTrgAddr.getBytes(), 0);
        pck.UDPtrg = newSrcPort;
        pck.UDPsrc = newTrgPort;
        pck.INTupper = -1;
    }

}
