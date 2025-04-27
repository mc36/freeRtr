package org.freertr.tab;

import java.util.ArrayList;
import java.util.List;
import java.util.TreeMap;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv4;
import org.freertr.addr.addrIPv6;
import org.freertr.addr.addrPrefix;
import org.freertr.cfg.cfgAceslst;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgIfc;
import org.freertr.cfg.cfgPlymp;
import org.freertr.cfg.cfgPool;
import org.freertr.ip.ipIcmp;
import org.freertr.pack.packHolder;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.logger;
import org.freertr.util.debugger;

/**
 * represents one nat config (source/target, orig/new)
 *
 * @author matecsaba
 */
public class tabNatCfgN extends tabListingEntry<addrIP> {

    /**
     * matching protocol
     */
    public int protocol = -1;

    /**
     * matching mask
     */
    public addrIP mask;

    /**
     * negated mask
     */
    public addrIP maskNot;

    /**
     * original source address
     */
    public addrIP origSrcAddr;

    /**
     * source access list
     */
    public tabListing<tabAceslstN<addrIP>, addrIP> origSrcList;

    /**
     * original source interface
     */
    public cfgIfc origTrgIface;

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
     * new target interface
     */
    public cfgIfc newSrcIface;

    /**
     * new target pool
     */
    public cfgPool<addrIPv4> newSrcPool4;

    /**
     * new target pool
     */
    public cfgPool<addrIPv6> newSrcPool6;

    /**
     * new source port
     */
    public int newSrcPort = -1;

    /**
     * new target port
     */
    public int newTrgPort = -1;

    /**
     * port range
     */
    public int rangeMin = -1;

    /**
     * port range
     */
    public int rangeMax = -1;

    /**
     * session limit on this entry
     */
    public int maxSess = 0;

    /**
     * maximum rate of sessions
     */
    public tabQos maxRate = null;

    /**
     * log translations
     */
    public boolean logTrans = false;

   /**
     * randomization method
     */
    public enum randomizeMethod {
        /**
         * random port pool based with collision avoidance
         */
        RandomPortAllocation,
        /**
         * port pool based using the first free available port
         */
        SequentialPortAllocation,
        /**
         * preserve original port if possible, otherwise use sequential allocation
         */
        PreserveOriginalThenSequential,
        /**
         * preserve original port if possible, otherwise use random allocation
         */
        PreserveOriginalThenRandom
    } 

    /**
     * randomization method
     */
    public randomizeMethod randMethod = randomizeMethod.PreserveOriginalThenRandom;

    /**
     * create instance
     */
    public tabNatCfgN() {
        timeout = 300 * 1000;
    }

    /**
     * convert string to address
     *
     * @param p protocol version
     * @param s string to convert
     * @param neg parse negated
     * @return 0=ok, 1=error, 2=time, 3=range, 4=log, 5=limit, 6=rate
     */
    public int fromString(int p, String s, boolean neg) {
        cmds cmd = new cmds("", s);
        s = cmd.word();
        if (s.equals("sequence")) {
            sequence = bits.str2num(cmd.word());
            s = cmd.word();
        }
        if (s.equals("timeout")) {
            timeout = bits.str2num(cmd.word());
            return 2;
        }
        if (s.equals("rate")) {
            if (neg) {
                maxRate = null;
                return 6;
            }
            cfgPlymp plc = cfgAll.plmpFind(cmd.word(), false);
            if (plc == null) {
                cmd.error("no such policy map");
                return 1;
            }
            maxRate = tabQos.convertPolicy(plc.plcmap);
            return 6;
        }
        if (s.equals("sessions")) {
            if (neg) {
                maxSess = 0;
            } else {
                maxSess = bits.str2num(cmd.word());
            }
            return 5;
        }
        if (s.equals("randomize")) {
            if (neg) {
                rangeMin = -1;
                rangeMax = -1;
                randMethod = randomizeMethod.PreserveOriginalThenRandom;
            } else {
                String nextWord = cmd.word();
                if (nextWord.equals("sequential-port-allocation")) {
                rangeMin = bits.str2num(cmd.word());
                rangeMax = bits.str2num(cmd.word());
                    randMethod = randomizeMethod.SequentialPortAllocation;
                } else if (nextWord.equals("preserve-original-then-sequential")) {
                    rangeMin = bits.str2num(cmd.word());
                    rangeMax = bits.str2num(cmd.word());
                    randMethod = randomizeMethod.PreserveOriginalThenSequential;
                } else if (nextWord.equals("preserve-original-then-random")) {
                    rangeMin = bits.str2num(cmd.word());
                    rangeMax = bits.str2num(cmd.word());
                    randMethod = randomizeMethod.PreserveOriginalThenRandom;
                } else if (nextWord.equals("random-port-allocation")) {
                    rangeMin = bits.str2num(cmd.word());
                    rangeMax = bits.str2num(cmd.word());
                    randMethod = randomizeMethod.RandomPortAllocation;
                } else {
                    // Case when only "randomize <min> <max>" was entered
                    rangeMin = bits.str2num(nextWord);
                    rangeMax = bits.str2num(cmd.word());
                    randMethod = randomizeMethod.RandomPortAllocation;
                }
            }
            return 3;
        }
        if (s.equals("log-translations")) {
            logTrans = !neg;
            return 4;
        }
        int what = 0; // 1=source, 2=target
        if (s.equals("source")) {
            what = 1;
        }
        if (s.equals("target")) {
            what = 2;
        }
        if (s.equals("srcport")) {
            protocol = bits.str2num(cmd.word());
            what = 1;
        }
        if (s.equals("trgport")) {
            protocol = bits.str2num(cmd.word());
            what = 2;
        }
        if (s.equals("srclist")) {
            what = 3;
        }
        if (s.equals("srcpref")) {
            what = 1;
            mask = new addrIP();
        }
        if (s.equals("trgpref")) {
            what = 2;
            mask = new addrIP();
        }
        if (what < 1) {
            return 1;
        }
        addrIP orgA = new addrIP();
        addrIP newA = new addrIP();
        int orgP = -1;
        int newP = -1;
        if (what == 3) {
            cfgAceslst acl = cfgAll.aclsFind(cmd.word(), false);
            if (acl == null) {
                return 1;
            }
            origSrcList = acl.aceslst;
            orgA = null;
        } else {
            s = cmd.word();
            if (s.equals("interface")) {
                origTrgIface = cfgAll.ifcFind(cmd.word(), 0);
                if (origTrgIface == null) {
                    cmd.error("no such interface");
                    return 1;
                }
            } else if (orgA.fromString(s)) {
                return 1;
            }
        }
        if (protocol >= 0) {
            orgP = bits.str2num(cmd.word());
        }
        s = cmd.word();
        if (s.equals("interface")) {
            newSrcIface = cfgAll.ifcFind(cmd.word(), 0);
            if (newSrcIface == null) {
                cmd.error("no such interface");
                return 1;
            }
        } else if (s.equals("pool")) {
            s = cmd.word();
            if (p == 4) {
                newSrcPool4 = cfgAll.poolFind(cfgAll.ip4pool, s, false);
                if (newSrcPool4 == null) {
                    cmd.error("no such pool");
                    return 1;
                }
            } else {
                newSrcPool6 = cfgAll.poolFind(cfgAll.ip6pool, s, false);
                if (newSrcPool6 == null) {
                    cmd.error("no such pool");
                    return 1;
                }
            }
        } else {
            if (newA.fromString(s)) {
                return 1;
            }
        }
        if (protocol >= 0) {
            newP = bits.str2num(cmd.word());
        }
        if (mask != null) {
            if (mask.fromString(cmd.word())) {
                return 1;
            }
            orgA.setAnd(orgA, mask);
            newA.setAnd(newA, mask);
            maskNot = new addrIP();
            maskNot.setNot(mask);
        }
        if (what == 2) {
            origTrgAddr = orgA;
            origTrgPort = orgP;
            newTrgAddr = newA;
            newTrgPort = newP;
        } else {
            origSrcAddr = orgA;
            origSrcPort = orgP;
            newSrcAddr = newA;
            newSrcPort = newP;
        }
        return 0;
    }

    public String toString() {
        int what = 0;
        addrIP orgA = new addrIP();
        addrIP newA = new addrIP();
        int orgP = -1;
        int newP = -1;
        if (origSrcAddr != null) {
            what = 1;
            orgA = origSrcAddr;
            orgP = origSrcPort;
            newA = newSrcAddr;
            newP = newSrcPort;
        } else {
            what = 2;
            orgA = origTrgAddr;
            orgP = origTrgPort;
            newA = newTrgAddr;
            newP = newTrgPort;
        }
        if (origSrcList != null) {
            what = 3;
            newA = newSrcAddr;
        }
        if (protocol >= 0) {
            what |= 4;
        }
        if (mask != null) {
            what |= 8;
        }
        String s;
        switch (what) {
            case 1:
                s = "source";
                break;
            case 2:
                s = "target";
                break;
            case 3:
                s = "srclist";
                break;
            case 5:
                s = "srcport";
                break;
            case 6:
                s = "trgport";
                break;
            case 9:
                s = "srcpref";
                break;
            case 10:
                s = "trgpref";
                break;
            default:
                s = "unknown";
                break;
        }
        if ((what & 4) != 0) {
            s = s + " " + protocol;
        }
        if (what == 3) {
            s = s + " " + origSrcList.listName;
        } else {
            if (origTrgIface == null) {
                s = s + " " + orgA;
            } else {
                s = s + " interface " + origTrgIface.name;
            }
        }
        if ((what & 4) != 0) {
            s = s + " " + orgP;
        }
        if (newSrcIface != null) {
            s = s + " interface " + newSrcIface.name;
        } else if (newSrcPool4 != null) {
            s = s + " pool " + newSrcPool4.name;
        } else if (newSrcPool6 != null) {
            s = s + " pool " + newSrcPool6.name;
        } else {
            s = s + " " + newA;
        }
        if ((what & 4) != 0) {
            s = s + " " + newP;
        }
        if ((what & 8) != 0) {
            s = s + " " + mask;
        }
        return s;
    }

    /**
     * convert to string
     *
     * @param beg beginning
     * @param filter filter mode
     * @return string
     */
    public List<String> usrString(String beg, int filter) {
        List<String> l = new ArrayList<String>();
        l.add(beg + "sequence " + sequence + " " + this);
        String s = beg + "sequence " + sequence;
        l.add(s + " timeout " + timeout);
        l.add(s + " sessions " + maxSess);
        if (maxRate != null) {
            l.add(s + " rate " + maxRate);
        }
        if (rangeMin > 0) {
            switch (randMethod) {
                case RandomPortAllocation:
            l.add(s + " randomize " + rangeMin + " " + rangeMax);
                    break;
                case SequentialPortAllocation:
                    l.add(s + " randomize sequential-port-allocation " + rangeMin + " " + rangeMax);
                    break;
                case PreserveOriginalThenSequential:
                    l.add(s + " randomize preserve-original-then-sequential " + rangeMin + " " + rangeMax);
                    break;
                case PreserveOriginalThenRandom:
                    l.add(s + " randomize preserve-original-then-random " + rangeMin + " " + rangeMax);
                    break;
            }
        }
        if (logTrans) {
            l.add(s + " log-translations");
        }
        return l;
    }

    /**
     * test if matches
     *
     * @param afi address family
     * @param asn as number
     * @param net network
     * @return false on success, true on error
     */
    public boolean matches(int afi, int asn, addrPrefix<addrIP> net) {
        return false;
    }

    /**
     * test if matches
     *
     * @param afi address family
     * @param asn as number
     * @param net network
     * @return false on success, true on error
     */
    public boolean matches(int afi, int asn, tabRouteEntry<addrIP> net) {
        return false;
    }

    /**
     * test if matches
     *
     * @param pck packet
     * @return false on success, true on error
     */
    public boolean matches(packHolder pck) {
        if ((protocol >= 0) && (pck.IPprt != protocol)) {
            return false;
        }
        if ((origSrcPort >= 0) && (pck.UDPsrc != origSrcPort)) {
            return false;
        }
        if ((origTrgPort >= 0) && (pck.UDPtrg != origTrgPort)) {
            return false;
        }
        if (origSrcList != null) {
            if (!origSrcList.matches(false, false, pck)) {
                return false;
            }
        }
        if (mask != null) {
            addrIP adr = new addrIP();
            if (origSrcAddr != null) {
                adr.setAnd(pck.IPsrc, mask);
                if (origSrcAddr.compareTo(adr) != 0) {
                    return false;
                }
            }
            if (origTrgAddr != null) {
                adr.setAnd(pck.IPtrg, mask);
                if (origTrgAddr.compareTo(adr) != 0) {
                    return false;
                }
            }
            // Logging only for successful match
            if (debugger.tabNatDebug) {
                StringBuilder logMessage = new StringBuilder();
                logMessage.append("NAT-MATCH-DEBUG - Sequence: ").append(sequence).append(" | ");
                
                // NAT rule type (srcport or trgport) determination
                String natType = "unknown";
                if (origSrcPort >= 0 && protocol >= 0) {
                    natType = "srcport " + protocol;
                } else if (origTrgPort >= 0 && protocol >= 0) {
                    natType = "trgport " + protocol;
                } else if (origSrcAddr != null && origTrgAddr == null) {
                    natType = "source";
                } else if (origSrcAddr == null && origTrgAddr != null) {
                    natType = "target";
                } else if (origSrcList != null) {
                    natType = "srclist";
                }
                
                logMessage.append("NAT-Type: ").append(natType).append(" | ");
                logMessage.append("Packet: ");
                logMessage.append("Protocol=").append(pck.IPprt).append(", ");
                logMessage.append("SrcIP=").append(pck.IPsrc).append(", ");
                logMessage.append("DstIP=").append(pck.IPtrg).append(", ");
                logMessage.append("SrcPort=").append(pck.UDPsrc).append(", ");
                logMessage.append("DstPort=").append(pck.UDPtrg).append(" | ");
                
                logMessage.append("NAT-Rule: ");
                logMessage.append("Protocol=").append(protocol).append(", ");
                if (origSrcAddr != null) logMessage.append("OrigSrcIP=").append(origSrcAddr).append(", ");
                if (origTrgAddr != null) logMessage.append("OrigDstIP=").append(origTrgAddr).append(", ");
                if (origSrcPort >= 0) logMessage.append("OrigSrcPort=").append(origSrcPort).append(", ");
                if (origTrgPort >= 0) logMessage.append("OrigDstPort=").append(origTrgPort).append(", ");
                if (newSrcAddr != null) logMessage.append("NewSrcIP=").append(newSrcAddr).append(", ");
                if (newTrgAddr != null) logMessage.append("NewDstIP=").append(newTrgAddr).append(", ");
                if (newSrcPort >= 0) logMessage.append("NewSrcPort=").append(newSrcPort).append(", ");
                if (newTrgPort >= 0) logMessage.append("NewDstPort=").append(newTrgPort).append(", ");
                if (origTrgIface != null) logMessage.append("OrigInterface=").append(origTrgIface.name).append(", ");
                if (newSrcIface != null) logMessage.append("NewInterface=").append(newSrcIface.name);
                
                logger.info(logMessage.toString());
            }
            return true;
        }
        if (origSrcAddr != null) {
            if (!usableAddr(origSrcAddr)) {
                return false;
            }
            if (origSrcAddr.compareTo(pck.IPsrc) != 0) {
                return false;
            }
        }
        if (origTrgAddr != null) {
            if (!usableAddr(origTrgAddr)) {
                return false;
            }
            if (origTrgAddr.compareTo(pck.IPtrg) != 0) {
                return false;
            }
        }
        if (newSrcAddr != null) {
            if (!usableAddr(newSrcAddr)) {
                return false;
            }
        }
        
        // Logging only for successful match
        if (debugger.tabNatDebug){
            StringBuilder logMessage = new StringBuilder();
            logMessage.append("NAT-MATCH-DEBUG - Sequence: ").append(sequence).append(" | ");
        
            // NAT rule type (srcport or trgport) determination
            String natType = "unknown";
            if (origSrcPort >= 0 && protocol >= 0) {
                natType = "srcport " + protocol;
            } else if (origTrgPort >= 0 && protocol >= 0) {
                natType = "trgport " + protocol;
            } else if (origSrcAddr != null && origTrgAddr == null) {
                natType = "source";
            } else if (origSrcAddr == null && origTrgAddr != null) {
                natType = "target";
            } else if (origSrcList != null) {
                natType = "srclist";
            }
        
            logMessage.append("NAT-Type: ").append(natType).append(" | ");
            logMessage.append("Packet: ");
            logMessage.append("Protocol=").append(pck.IPprt).append(", ");
            logMessage.append("SrcIP=").append(pck.IPsrc).append(", ");
            logMessage.append("DstIP=").append(pck.IPtrg).append(", ");
            logMessage.append("SrcPort=").append(pck.UDPsrc).append(", ");
            logMessage.append("DstPort=").append(pck.UDPtrg).append(" | ");
        
            logMessage.append("NAT-Rule: ");
            logMessage.append("Protocol=").append(protocol).append(", ");
            if (origSrcAddr != null) logMessage.append("OrigSrcIP=").append(origSrcAddr).append(", ");
            if (origTrgAddr != null) logMessage.append("OrigDstIP=").append(origTrgAddr).append(", ");
            if (origSrcPort >= 0) logMessage.append("OrigSrcPort=").append(origSrcPort).append(", ");
            if (origTrgPort >= 0) logMessage.append("OrigDstPort=").append(origTrgPort).append(", ");
            if (newSrcAddr != null) logMessage.append("NewSrcIP=").append(newSrcAddr).append(", ");
            if (newTrgAddr != null) logMessage.append("NewDstIP=").append(newTrgAddr).append(", ");
            if (newSrcPort >= 0) logMessage.append("NewSrcPort=").append(newSrcPort).append(", ");
            if (newTrgPort >= 0) logMessage.append("NewDstPort=").append(newTrgPort).append(", ");
            if (origTrgIface != null) logMessage.append("OrigInterface=").append(origTrgIface.name).append(", ");
            if (newSrcIface != null) logMessage.append("NewInterface=").append(newSrcIface.name);
        
            logger.info(logMessage.toString());
        }
        return true;
    }

    private boolean usableAddr(addrIP addr) {
        if (addr.isIPv4()) {
            addrIPv4 adr = addr.toIPv4();
            if (adr.isFilled(0)) {
                return false;
            }
            if (!adr.isUnicast()) {
                return false;
            }
        } else {
            addrIPv6 adr = addr.toIPv6();
            if (adr.isFilled(0)) {
                return false;
            }
            if (!adr.isUnicast()) {
                return false;
            }
            if (adr.isLinkLocal()) {
                return false;
            }
        }
        return true;
    }

    /**
     * Adds a new port range for a specific source address
     *
     * @param srcAddr The source address
     * @param minPort The minimum port number (inclusive)
     * @param maxPort The maximum port number (inclusive)
     */
    public void addPortRange(addrIP srcAddr, int minPort, int maxPort) {
        tabNatPortPoolManager.getInstance().createSubPool(srcAddr, minPort, maxPort, sequence);
        
        if (debugger.tabNatDebug) {
            logger.debug("Created port pool for " + srcAddr + " with range " + minPort + "-" + maxPort);
        }
    }

    /**
     * Allocates a port from the pool for a specific source address
     *
     * @param srcAddr The source address
     * @return The allocated port number, or -1 if no ports are available
     */
    public int allocatePort(addrIP srcAddr) {
        boolean useRandomAllocation = (randMethod == randomizeMethod.RandomPortAllocation);
        
        // Use the sequence-specific method so that ports are correctly displayed in the pools
        int port = tabNatPortPoolManager.getInstance().allocatePortFromSequence(srcAddr, protocol, sequence, useRandomAllocation);
        
        if (port < 0) {
            if (debugger.tabNatDebug) {
                logger.error("Port allocation failed for " + srcAddr + 
                           " (protocol: " + protocol + ", sequence: " + sequence + ")");
            }
        } else if (debugger.tabNatDebug) {
            logger.debug("Allocated port " + port + " for " + srcAddr + 
                       " (protocol: " + protocol + ", sequence: " + sequence + ")");
        }
        
        return port;
    }
    
    /**
     * Releases a previously allocated port back to the pool for a specific source address
     * 
     * @param srcAddr The source address
     * @param port The port number to release
     * @param protocolNum The protocol number (TCP=6, UDP=17, -1=both)
     */
    public void releasePort(addrIP srcAddr, int port, int protocolNum) {
        tabNatPortPoolManager.getInstance().releasePort(srcAddr, port, protocolNum);
        
        if (debugger.tabNatDebug) {
            logger.debug("Released port " + port + " for " + srcAddr + 
                      " (protocol: " + protocolNum + ")");
        }
    }
    
    /**
     * Releases a previously allocated port back to the pool for a specific source address
     * using the class protocol variable
     * 
     * @param srcAddr The source address
     * @param port The port number to release
     */
    public void releasePort(addrIP srcAddr, int port) {
        releasePort(srcAddr, port, protocol);
    }

    /**
     * Removes the port range for a specific source address
     * 
     * @param srcAddr The source address
     */
    public void removePortRange(addrIP srcAddr) {
        tabNatPortPoolManager.getInstance().removeSubPool(srcAddr);
    }
    
    /**
     * Check if there are available ports for a given address
     * @param addr The address to check
     * @param protocolNum The protocol number
     * @return true if ports are available, false otherwise
     */
    public boolean hasAvailablePorts(addrIP addr, int protocolNum) {
        return tabNatPortPoolManager.getInstance().hasSubPool(addr);
    }
    
    /**
     * Check if there are available ports for a given address (using class protocol variable)
     * @param addr The address to check
     * @return true if ports are available, false otherwise
     */
    public boolean hasAvailablePorts(addrIP addr) {
        return hasAvailablePorts(addr, protocol);
    }

    /**
     * Check if a port is already in use in the port pool
     * @param addr The address to check
     * @param port The port to check
     * @param protocolNum The protocol number
     * @return true if port is in use, false otherwise
     */
    private boolean isPortInUse(addrIP addr, int port, int protocolNum) {
        return tabNatPortPoolManager.getInstance().isPortInUse(addr, port, protocolNum);
    }
    
    /**
     * Check if a port pool exists for the given address
     * @param addr The address to check
     * @return true if pool exists, false otherwise
     */
    public boolean hasPortPool(addrIP addr) {
        return tabNatPortPoolManager.getInstance().hasSubPool(addr);
    }


    /**
     * update entry
     *
     * @param afi address family
     * @param asn as number
     * @param net network
     */
    public void update(int afi, int asn, tabRouteEntry<addrIP> net) {
    }

    /**
     * Allocate a port from a specific pool with the provided sequence number
     *
     * @param srcAddr The source address
     * @param sequenceNum The sequence number of the pool to use
     * @param protocol The protocol number
     * @param useRandomAllocation Whether to use random allocation
     * @return The allocated port number, or -1 if no ports are available
     */
    private int allocatePortFromSequence(addrIP srcAddr, int sequenceNum, int protocol, boolean useRandomAllocation) {
        // First check if a sub-pool with the specified sequence number exists
        tabNatPortPoolManager manager = tabNatPortPoolManager.getInstance();
        if (!manager.hasSubPool(srcAddr)) {
            if (debugger.tabNatDebug) {
                logger.error("No sub-pools exist for " + srcAddr);
            }
            return -1;
        }
        
        // Use the new method that allocates directly from the pool with the specified sequence number
        int port = manager.allocatePortFromSequence(srcAddr, protocol, sequenceNum, useRandomAllocation);
        
        if (port < 0) {
            if (debugger.tabNatDebug) {
                logger.error("Port allocation failed for " + srcAddr + 
                           " (protocol: " + protocol + ", sequence: " + sequenceNum + ")");
            }
        } else if (debugger.tabNatDebug) {
            logger.debug("Allocated port " + port + " for " + srcAddr + 
                       " (protocol: " + protocol + ", sequence: " + sequenceNum + ")");
        }
        
        return port;
    }

    /**
     * Allocates a port using the PreserveOriginalThenSequential strategy
     * Tries to keep original port if possible, otherwise allocates next available
     * 
     * @param n NAT translation entry
     * @return allocated port number or -1 if failed
     */
    private int allocatePreserveOriginalThenSequentialPort(tabNatTraN n) {
        if (debugger.tabNatDebug) {
            logger.info("DEBUG-PRESERVE-ORIGINAL: Starting port allocation with PreserveOriginalThenSequential");
            logger.info("DEBUG-PRESERVE-ORIGINAL: Original port is " + n.origSrcPort + 
                      ", valid range is " + (rangeMin > 0 ? rangeMin : "1") + "-" + 
                      (rangeMax > 0 ? rangeMax : "65535"));
        }
        
        // First check if the original port is in the valid range
        int effectiveRangeMin = (rangeMin > 0) ? rangeMin : 1;
        int effectiveRangeMax = (rangeMax > 0) ? rangeMax : 65535;
        
        boolean portInRange = (n.origSrcPort >= effectiveRangeMin && n.origSrcPort <= effectiveRangeMax);
        if (debugger.tabNatDebug) {
            if (!portInRange) {
                logger.info("DEBUG-PRESERVE-ORIGINAL: Original port " + n.origSrcPort + 
                          " is outside the valid range " + effectiveRangeMin + "-" + effectiveRangeMax);
            } else {
                logger.info("DEBUG-PRESERVE-ORIGINAL: Original port " + n.origSrcPort + 
                          " is within the valid range " + effectiveRangeMin + "-" + effectiveRangeMax);
            }
        }
        
        // Then check if the original port is already in use
        boolean portIsUsed = isPortInUse(n.newSrcAddr, n.origSrcPort, n.protocol);
        if (debugger.tabNatDebug) {
            if (portIsUsed) {
                logger.info("DEBUG-PRESERVE-ORIGINAL: Original port " + n.origSrcPort + 
                          " is already in use for " + n.newSrcAddr);
            } else {
                logger.info("DEBUG-PRESERVE-ORIGINAL: Original port " + n.origSrcPort + 
                          " is available for " + n.newSrcAddr);
            }
        }
        
        // Keep the original port if it is in the valid range and not in use
        if (portInRange && !portIsUsed) {
            // Mark the original port as used
            tabNatPortPoolManager.getInstance().markPortAsUsed(n.newSrcAddr, n.origSrcPort, n.protocol);
            
            if (debugger.tabNatDebug) {
                logger.info("DEBUG-PRESERVE-ORIGINAL: SUCCESS - Keeping original port " + n.origSrcPort + 
                          " for " + n.newSrcAddr);
                logger.debug("Kept original port " + n.origSrcPort + " for " + n.newSrcAddr + 
                           " (preserve original then sequential, sequence: " + sequence + ")");
            }
        
            return n.origSrcPort;
        }
        
        // If we get here, we cannot use the original port
        if (debugger.tabNatDebug) {
            logger.info("DEBUG-PRESERVE-ORIGINAL: Cannot use original port " + n.origSrcPort + 
                      ", falling back to sequential allocation");
        }
        
        // Sequential port allocation as fallback
        int allocatedPort = allocatePortFromSequence(n.newSrcAddr, sequence, n.protocol, false);
        
        if (debugger.tabNatDebug) {
            if (allocatedPort < 0) {
                logger.error("DEBUG-PRESERVE-ORIGINAL: Sequential allocation FAILED for " + n.newSrcAddr);
            } else {
                logger.info("DEBUG-PRESERVE-ORIGINAL: Successfully allocated sequential port " + 
                          allocatedPort + " for " + n.newSrcAddr);
            }
        }
        
        return allocatedPort;
    }

    /**
     * Allocates a port using the PreserveOriginalThenRandom strategy
     * Tries to keep original port if possible, otherwise allocates random port
     * 
     * @param n NAT translation entry
     * @return allocated port number or -1 if failed
     */
    private int allocatePreserveOriginalThenRandomPort(tabNatTraN n) {
        if (debugger.tabNatDebug) {
            logger.info("DEBUG-PRESERVE-ORIGINAL: Starting port allocation with PreserveOriginalThenRandom");
            logger.info("DEBUG-PRESERVE-ORIGINAL: Original port is " + n.origSrcPort + 
                      ", valid range is " + (rangeMin > 0 ? rangeMin : "1") + "-" + 
                      (rangeMax > 0 ? rangeMax : "65535"));
        }
        
        // First check if the original port is in the valid range
        int effectiveRangeMin = (rangeMin > 0) ? rangeMin : 1;
        int effectiveRangeMax = (rangeMax > 0) ? rangeMax : 65535;
        
        boolean portInRange = (n.origSrcPort >= effectiveRangeMin && n.origSrcPort <= effectiveRangeMax);
        if (debugger.tabNatDebug) {
            if (!portInRange) {
                logger.info("DEBUG-PRESERVE-ORIGINAL: Original port " + n.origSrcPort + 
                          " is outside the valid range " + effectiveRangeMin + "-" + effectiveRangeMax);
            } else {
                logger.info("DEBUG-PRESERVE-ORIGINAL: Original port " + n.origSrcPort + 
                          " is within the valid range " + effectiveRangeMin + "-" + effectiveRangeMax);
            }
        }
        
        // Then check if the original port is already in use
        boolean portIsUsed = isPortInUse(n.newSrcAddr, n.origSrcPort, n.protocol);
        if (debugger.tabNatDebug) {
            if (portIsUsed) {
                logger.info("DEBUG-PRESERVE-ORIGINAL: Original port " + n.origSrcPort + 
                          " is already in use for " + n.newSrcAddr);
            } else {
                logger.info("DEBUG-PRESERVE-ORIGINAL: Original port " + n.origSrcPort + 
                          " is available for " + n.newSrcAddr);
            }
        }
        
        // Keep the original port if it is in the valid range and not in use
        if (portInRange && !portIsUsed) {
            // Mark the original port as used
            tabNatPortPoolManager.getInstance().markPortAsUsed(n.newSrcAddr, n.origSrcPort, n.protocol);
            
            if (debugger.tabNatDebug) {
                logger.info("DEBUG-PRESERVE-ORIGINAL: SUCCESS - Keeping original port " + n.origSrcPort + 
                          " for " + n.newSrcAddr);
            }
            
            if (debugger.tabNatDebug) {
                logger.debug("Kept original port " + n.origSrcPort + " for " + n.newSrcAddr + 
                           " (preserve original then random, sequence: " + sequence + ")");
            }
            return n.origSrcPort;
        }
        
        // If we get here, we cannot use the original port
        if (debugger.tabNatDebug) {
            logger.info("DEBUG-PRESERVE-ORIGINAL: Cannot use original port " + n.origSrcPort + 
                      ", falling back to random allocation");
        }
        
        // Random port allocation as fallback
        int allocatedPort = allocatePortFromSequence(n.newSrcAddr, sequence, n.protocol, true);
        
        if (debugger.tabNatDebug) {
            if (allocatedPort < 0) {
                logger.error("DEBUG-PRESERVE-ORIGINAL: Random allocation FAILED for " + n.newSrcAddr);
            } else {
                logger.info("DEBUG-PRESERVE-ORIGINAL: Successfully allocated random port " + 
                          allocatedPort + " for " + n.newSrcAddr);
            }
        }
        
        return allocatedPort;
    }

    /**
     * Adds a new port range for a specific sequence number
     * 
     * @param srcAddr The source address
     * @param sequenceNum The sequence number of the pool
     */
    private void addPortRangeForSequence(addrIP srcAddr, int sequenceNum) {
        if (debugger.tabNatDebug) {
            logger.info("DEBUG-NAT-POOL: Adding port range for " + srcAddr + " with sequence " + sequenceNum);
        }
        
        tabNatPortPoolManager manager = tabNatPortPoolManager.getInstance();
        
        // If rangeMin and rangeMax are configured, we use these values
        if (rangeMin > 0 && rangeMax > 0) {
            if (debugger.tabNatDebug) {
                logger.info("DEBUG-NAT-POOL: Using configured range " + rangeMin + "-" + rangeMax);
            }
            
            // Create pool with the specified sequence number
            manager.createSubPool(srcAddr, rangeMin, rangeMax, sequenceNum);
            
        } else {
            // If no ranges are configured, we use the default ranges
            int defaultMinPort = 1;
            int defaultMaxPort = 65535;

            if (debugger.tabNatDebug) {
                logger.info("DEBUG-NAT-POOL: Using default range " + defaultMinPort + "-" + defaultMaxPort);
            }
            
            // Create pool with the specified sequence number
            manager.createSubPool(srcAddr, defaultMinPort, defaultMaxPort, sequenceNum);
            
        }
        
        // Debug log to verify if the pool was created
        if (debugger.tabNatDebug) {
            logger.info("DEBUG-NAT-POOL: Pool creation complete, checking if pool exists: " + 
            manager.hasSubPool(srcAddr));
        }
    }

    /**
     * create entry
     *
     * @param pck packet to use
     * @param icc icmp core
     * @return newly created entry
     */
    public tabNatTraN createEntry(packHolder pck, ipIcmp icc) {
        tabNatTraN n = new tabNatTraN();
        n.lastUsed = bits.getTime();
        n.created = n.lastUsed;
        n.timeout = timeout;
        n.protocol = pck.IPprt;
        n.origSrcAddr = pck.IPsrc.copyBytes();
        n.origTrgAddr = pck.IPtrg.copyBytes();
        n.origSrcPort = pck.UDPsrc;
        n.origTrgPort = pck.UDPtrg;
        n.newSrcAddr = pck.IPsrc.copyBytes();
        n.newTrgAddr = pck.IPtrg.copyBytes();
        n.newSrcPort = pck.UDPsrc;
        n.newTrgPort = pck.UDPtrg;
        
        // Set reference to this NAT configuration to make sequence number available
        n.natCfg = this;
        
        // Check if this is a trgport rule
        boolean isTrgportRule = (origTrgPort >= 0 && protocol >= 0 && origSrcPort < 0);
        
        if (mask == null) {
            if (newSrcAddr != null) {
                n.newSrcAddr = newSrcAddr.copyBytes();
            }
            if (newTrgAddr != null) {
                n.newTrgAddr = newTrgAddr.copyBytes();
            }
        } else {
            addrIP adr = new addrIP();
            if (newSrcAddr != null) {
                adr.setAnd(pck.IPsrc, maskNot);
                adr.setOr(adr, newSrcAddr);
                n.newSrcAddr = adr.copyBytes();
            }
            if (newTrgAddr != null) {
                adr.setAnd(pck.IPtrg, maskNot);
                adr.setOr(adr, newTrgAddr);
                n.newTrgAddr = adr.copyBytes();
            }
        }
        if (newSrcPort >= 0) {
            n.newSrcPort = newSrcPort;
        }
        if (newTrgPort >= 0) {
            n.newTrgPort = newTrgPort;
        }
        
        // For trgport rules, create pool for the original destination IP (OrigDstIP)
        if (isTrgportRule) {
            // For trgport rules, we work with the origTrgAddr (public router IP)
            if (n.protocol != icc.getProtoNum() && n.origSrcPort > 0 && origTrgAddr != null) {
                // Create sub-pool for OrigDstIP with the specific sequence number
                // If specific port ranges are configured, we use these
                // Otherwise, we use the entire port range
                int trgportMinPort = 1;  // For trgport rules, we use the full range
                int trgportMaxPort = 65535;
                
                if (logTrans) {
                    logger.debug("Creating port pool for original destination IP " + origTrgAddr + 
                               " (trgport rule, sequence: " + sequence + ") with range " + 
                               trgportMinPort + "-" + trgportMaxPort);
                }
                
                tabNatPortPoolManager.getInstance().createSubPool(origTrgAddr, trgportMinPort, trgportMaxPort, sequence);
                
                // Check if the source port is already in use
                if (!tabNatPortPoolManager.getInstance().isPortInUse(origTrgAddr, n.origSrcPort, n.protocol)) {
                    // Mark the source port as used
                    tabNatPortPoolManager.getInstance().markPortAsUsed(origTrgAddr, n.origSrcPort, n.protocol);
                    
                    if (logTrans) {
                        logger.info("Marked source port " + n.origSrcPort + " as used for original destination IP " + 
                                  origTrgAddr + " (trgport rule, sequence: " + sequence + ")");
                    }
                } else if (logTrans) {
                    logger.info("Source port " + n.origSrcPort + " already in use for original destination IP " + 
                              origTrgAddr + " (trgport rule, sequence: " + sequence + ")");
                }
            }
            
            n.logEnd = logTrans;
            if (logTrans) {
                logger.info("creating translation " + n);
            }
            return n;
        }
        
        // Skip ICMP protocol processing
        if (n.protocol == icc.getProtoNum()) {
            n.logEnd = logTrans;
            if (logTrans) {
                logger.info("creating icmp translation " + n);
            }
            return n;
        }
        
        // Add debug logs to understand why no port allocation is taking place
        if (debugger.tabNatDebug) {
            logger.info("DEBUG-NAT: Checking port allocation conditions: protocol=" + n.protocol + 
                      ", newSrcAddr=" + (n.newSrcAddr != null ? n.newSrcAddr.toString() : "null") +
                      ", isReverseEntry=" + n.isReverseEntry + 
                      ", newSrcPort=" + n.newSrcPort + 
                      ", randMethod=" + randMethod);
        }
        
        // Port allocation based on selected randomization method
        if (n.protocol != icc.getProtoNum() && n.newSrcAddr != null && !n.isReverseEntry) {
            // Log of the configured port range, important for debugging
            if (debugger.tabNatDebug) {
                logger.info("DEBUG-NAT: Configured port range: " + 
                          (rangeMin > 0 ? rangeMin : "default") + "-" + 
                          (rangeMax > 0 ? rangeMax : "default"));
            }
            
            // Determine the actual range to be used
            int minPort = (rangeMin > 0) ? rangeMin : 1;
            int maxPort = (rangeMax > 0) ? rangeMax : 65535;
            
            if (debugger.tabNatDebug) {
                logger.info("DEBUG-NAT: Creating pool for " + n.newSrcAddr + " with sequence " + sequence + 
                          " and range " + minPort + "-" + maxPort);
            }
            
            // Create pool with current sequence number and configured port limits
            tabNatPortPoolManager.getInstance().createSubPool(n.newSrcAddr, minPort, maxPort, sequence);
            
            // Save the original port for logging purposes and possible fallbacks
            int originalPort = n.newSrcPort;
            if (debugger.tabNatDebug) {
                logger.info("DEBUG-NAT: Original port value: " + originalPort);
            }
            
            // For all methods, always execute the correct allocation logic,
            // regardless of whether the port is already set
            
            if (debugger.tabNatDebug) {
                logger.info("DEBUG-NAT: Applying port allocation strategy: " + randMethod);
            }
            
            // Choose the corresponding allocation path depending on the method
            if (randMethod == randomizeMethod.SequentialPortAllocation) {
                if (debugger.tabNatDebug) {
                    logger.info("DEBUG-NAT: Using SequentialPortAllocation strategy");
                }
                
                // Allocate port sequentially
                n.newSrcPort = tabNatPortPoolManager.getInstance().allocatePortFromSequence(
                    n.newSrcAddr, n.protocol, sequence, false);
            }
            else if (randMethod == randomizeMethod.RandomPortAllocation) {
                if (debugger.tabNatDebug) {
                    logger.info("DEBUG-NAT: Using RandomPortAllocation strategy");
                }
                
                // Randomly allocate port
                n.newSrcPort = tabNatPortPoolManager.getInstance().allocatePortFromSequence(
                    n.newSrcAddr, n.protocol, sequence, true);
            }
            else if (randMethod == randomizeMethod.PreserveOriginalThenSequential) {
                if (debugger.tabNatDebug) {
                    logger.info("DEBUG-NAT: Using PreserveOriginalThenSequential strategy");
                }
                
                // Always use the specialized method
                n.newSrcPort = allocatePreserveOriginalThenSequentialPort(n);
            }
            else if (randMethod == randomizeMethod.PreserveOriginalThenRandom) {
                if (debugger.tabNatDebug) {
                    logger.info("DEBUG-NAT: Using PreserveOriginalThenRandom strategy");
                }
                
                // Always use the specialized method
                n.newSrcPort = allocatePreserveOriginalThenRandomPort(n);
            }
            
            // Check if the port allocation was successful
            if (n.newSrcPort < 0) {
                if (debugger.tabNatDebug) {
                    logger.error("DEBUG-NAT: Port allocation FAILED for " + n.newSrcAddr + 
                               " using " + randMethod + " strategy");
                }
                
                // No fallback required - all methods have their own logic
                if (debugger.tabNatDebug) {
                    logger.info("DEBUG-NAT: No fallback attempted for " + randMethod + " - port allocation has failed");
                }
            } else {
                if (debugger.tabNatDebug) {
                    logger.info("DEBUG-NAT: Successfully allocated port " + n.newSrcPort + 
                              " for " + n.newSrcAddr + " using " + randMethod + " strategy" + 
                              (originalPort != n.newSrcPort ? " (original port was " + originalPort + ")" : ""));
                }
            }
        } else if (debugger.tabNatDebug && n.isReverseEntry) {
            logger.debug("Skipping port pool creation for reverse entry with IP " + n.newSrcAddr);
        } else if (debugger.tabNatDebug) {
            logger.info("DEBUG-NAT: Port allocation skipped: protocol match=" + (n.protocol == icc.getProtoNum()) + 
                      ", newSrcAddr null=" + (n.newSrcAddr == null) + 
                      ", isReverseEntry=" + n.isReverseEntry);
        }

        n.logEnd = logTrans;
        if (logTrans) {
            logger.info("creating translation " + n);
        }
        return n;
    }

}
