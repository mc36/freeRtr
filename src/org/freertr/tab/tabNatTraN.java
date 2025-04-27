package org.freertr.tab;

import org.freertr.addr.addrIP;
import org.freertr.pack.packHolder;
import org.freertr.prt.prtTcp;
import org.freertr.prt.prtUdp;
import org.freertr.util.bits;
import org.freertr.util.counter;
import org.freertr.util.logger;
import org.freertr.util.debugger;

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
     * Indicates if this is a reverse entry (for external connections) Used to
     * prevent port pool creation for external IPs
     */
    public boolean isReverseEntry = false;

    /**
     * NAT configuration for this translation
     */
    public tabNatCfgN natCfg;

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
        n.isReverseEntry = true;
        n.natCfg = this.natCfg;
        reverse = n;
        return n;
    }

    public String toString() {
        String packStr = "" + cntr.packRx;
        String byteStr = "" + cntr.byteRx;

        if (hwCntr != null) {
            packStr += "+" + hwCntr.packRx;
            byteStr += "+" + hwCntr.byteRx;
        }

        // Don't display sequence number from natCfg as part of the target anymore
        return protocol + "|" + origSrcAddr + " " + origSrcPort + "|" + origTrgAddr + " " + origTrgPort + "|"
                + newSrcAddr + " " + newSrcPort + "|" + newTrgAddr + " " + newTrgPort + "|"
                + bits.timePast(created) + "|" + bits.timePast(lastUsed) + "|"
                + bits.timeDump(timeout / 1000) + "|" + packStr + "|" + byteStr;
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

    /**
     * release resources when the NAT translation is removed This is called when
     * a NAT translation expires or is manually cleared
     */
    public void releaseResources() {
        // Release allocated ports back to the pool if needed
        if (protocol == prtTcp.protoNum || protocol == prtUdp.protoNum) {
            tabNatPortPoolManager poolManager = tabNatPortPoolManager.getInstance();

            // Detailed debug information for troubleshooting
            if (debugger.tabNatDebug) {
                logger.info("DEBUG-RELEASE: Starting resource cleanup for NAT entry: "
                        + "protocol=" + protocol
                        + ", origSrcAddr=" + origSrcAddr
                        + ", origSrcPort=" + origSrcPort
                        + ", origTrgAddr=" + origTrgAddr
                        + ", origTrgPort=" + origTrgPort
                        + ", newSrcAddr=" + newSrcAddr
                        + ", newSrcPort=" + newSrcPort);
            }

            // 1. Check and release newSrcPort in the pool of newSrcAddr by default
            if (newSrcAddr != null && newSrcPort > 0 && poolManager.hasSubPool(newSrcAddr)) {
                if (debugger.tabNatDebug) {
                    logger.info("DEBUG-RELEASE: Checking if need to release newSrcPort=" + newSrcPort
                            + " for newSrcAddr=" + newSrcAddr);
                }

                // Check if the port is actually marked in the pool
                if (poolManager.isPortInUse(newSrcAddr, newSrcPort, protocol)) {
                    poolManager.releasePort(newSrcAddr, newSrcPort, protocol);

                    if (debugger.tabNatDebug) {
                        logger.info("DEBUG-RELEASE: Released port " + newSrcPort
                                + " for " + newSrcAddr + " (protocol: " + protocol + ")");
                    }
                }
            }

            // 2. For trgport rules, the source port (origSrcPort) in the pool of the destination address (origTrgAddr) must be released
            if (origTrgAddr != null && origSrcPort > 0 && poolManager.hasSubPool(origTrgAddr)) {
                if (debugger.tabNatDebug) {
                    logger.info("DEBUG-RELEASE: Checking if need to release origSrcPort=" + origSrcPort
                            + " for origTrgAddr=" + origTrgAddr + " (potential trgport rule)");
                }

                // Check if the port is actually marked in the pool
                if (poolManager.isPortInUse(origTrgAddr, origSrcPort, protocol)) {
                    poolManager.releasePort(origTrgAddr, origSrcPort, protocol);

                    if (debugger.tabNatDebug) {
                        logger.info("DEBUG-RELEASE: Released port " + origSrcPort
                                + " for " + origTrgAddr + " (trgport rule, protocol: " + protocol + ")");
                    }
                }
            }
        }
    }

    /**
     * Get combined stats for both software and hardware counters
     *
     * @return Array with [sw_packets, sw_bytes, hw_packets, hw_bytes,
     * total_packets, total_bytes]
     */
    public long[] getCombinedStats() {
        long[] stats = new long[6];

        // Software counters
        stats[0] = cntr.packRx;
        stats[1] = cntr.byteRx;

        // Hardware counters
        if (hwCntr != null) {
            stats[2] = hwCntr.packRx;
            stats[3] = hwCntr.byteRx;
        }

        // Totals
        stats[4] = stats[0] + stats[2];
        stats[5] = stats[1] + stats[3];

        return stats;
    }

}
