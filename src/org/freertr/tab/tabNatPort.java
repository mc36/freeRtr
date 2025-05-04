package org.freertr.tab;

import java.util.ArrayList;
import java.util.BitSet;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import org.freertr.addr.addrIP;
import org.freertr.prt.prtTcp;
import org.freertr.util.bits;
import org.freertr.util.logger;
import org.freertr.util.debugger;

/**
 * central management of NAT port pools Each IP address has its own independent
 * port pool
 *
 * @author takalele
 */
public class tabNatPort {

    // Map of IP addresses to their independent master pools
    private final Map<String, MasterPortPool> masterPools;

    // Map of composite keys (IP+sequence) to sub-pools
    private final Map<String, SubPortPool> subPools;

    // Map of IP addresses to a sorted map of sequence numbers to sub-pools
    // Used to quickly find all sub-pools for an IP and determine the highest priority one
    private final Map<String, TreeMap<Integer, SubPortPool>> ipToSubPoolsMap;

    /**
     * create instance
     */
    public tabNatPort() {
        masterPools = new HashMap<>();
        subPools = new HashMap<>();
        ipToSubPoolsMap = new HashMap<>();
    }

    /**
     * Class for the master port pool that manages the entire range for a
     * specific IP
     */
    private class MasterPortPool {

        private final BitSet tcpPortBitmap;
        private final BitSet udpPortBitmap;
        private final int minPort;
        private final int maxPort;
        private final addrIP ipAddress;

        public MasterPortPool(addrIP ipAddress, int minPort, int maxPort) {
            this.ipAddress = ipAddress;
            this.minPort = minPort;
            this.maxPort = maxPort;
            int size = maxPort - minPort + 1;
            this.tcpPortBitmap = new BitSet(size);
            this.udpPortBitmap = new BitSet(size);
            if (debugger.tabNatDebug) {
                logger.info("Master port pool created for " + ipAddress + " with range " + minPort + "-" + maxPort);
            }
        }

        /**
         * Check if a port is already in use in the master pool
         */
        public boolean isPortInUse(int port, int protocol) {
            BitSet bitmap = (protocol == prtTcp.protoNum) ? tcpPortBitmap : udpPortBitmap;
            int index = port - minPort;
            if (index < 0 || index >= bitmap.size()) {
                return false;
            }
            return bitmap.get(index);
        }

        /**
         * Mark a port as used in the master pool
         */
        public void markPortAsUsed(int port, int protocol) {
            BitSet bitmap = (protocol == prtTcp.protoNum) ? tcpPortBitmap : udpPortBitmap;
            int index = port - minPort;
            if (index >= 0 && index < bitmap.size()) {
                bitmap.set(index);
                if (debugger.tabNatDebug) {
                    logger.debug("Port " + port + " marked in master pool for " + ipAddress + " (protocol: " + protocol + ")");
                }
            }
        }

        /**
         * Release a port in the master pool
         */
        public void releasePort(int port, int protocol) {
            BitSet bitmap = (protocol == prtTcp.protoNum) ? tcpPortBitmap : udpPortBitmap;
            int index = port - minPort;
            if (index >= 0 && index < bitmap.size()) {
                bitmap.clear(index);
                if (debugger.tabNatDebug) {
                    logger.debug("Port " + port + " released in master pool for " + ipAddress + " (protocol: " + protocol + ")");
                }
            }
        }

        /**
         * Get usage statistics for the master pool
         */
        public String getUsageStatistics() {
            int totalPorts = maxPort - minPort + 1;
            int tcpUsed = tcpPortBitmap.cardinality();
            int udpUsed = udpPortBitmap.cardinality();
            int totalUsed = tcpUsed + udpUsed;
            int tcpFree = totalPorts - tcpUsed;
            int udpFree = totalPorts - udpUsed;

            double tcpUsagePercent = ((double) tcpUsed / totalPorts) * 100;
            double udpUsagePercent = ((double) udpUsed / totalPorts) * 100;

            // Format with enough space for 5-digit numbers
            return String.format("Port range: %d-%d | TCP ports: %6d used, %6d free (%.1f%% used) | UDP ports: %6d used, %6d free (%.1f%% used) | Total: %d ports",
                    minPort, maxPort,
                    tcpUsed, tcpFree, tcpUsagePercent,
                    udpUsed, udpFree, udpUsagePercent,
                    totalUsed);
        }
    }

    /**
     * Class for sub-pools that can only use a portion of the master pool
     */
    private class SubPortPool {

        private final BitSet tcpPortBitmap;
        private final BitSet udpPortBitmap;
        private final int minPort;
        private final int maxPort;
        private int lastAllocatedTcpPort;
        private int lastAllocatedUdpPort;
        private final addrIP owner;
        private final int sequence;

        public SubPortPool(addrIP owner, int minPort, int maxPort, int sequence) {
            this.owner = owner;
            this.minPort = minPort;
            this.maxPort = maxPort;
            this.sequence = sequence;
            int size = maxPort - minPort + 1;
            this.tcpPortBitmap = new BitSet(size);
            this.udpPortBitmap = new BitSet(size);
            this.lastAllocatedTcpPort = minPort - 1;
            this.lastAllocatedUdpPort = minPort - 1;
            if (debugger.tabNatDebug) {
                logger.info("Sub-pool created for " + owner + " with range " + minPort + "-" + maxPort + " (sequence: " + sequence + ")");
            }
        }

        /**
         * Get the sequence number of this sub-pool
         */
        public int getSequence() {
            return sequence;
        }

        /**
         * Allocate a new port from the sub-pool using sequential allocation
         */
        public int allocatePort(int protocol) {
            // Clearly indicate that sequential port allocation is being used
            if (debugger.tabNatDebug) {
                logger.info("DEBUG-SUBPOOL: SEQUENTIAL PORT ALLOCATION ACTIVE for " + owner
                        + " (protocol: " + protocol + ", sequence: " + sequence
                        + ", range: " + minPort + "-" + maxPort + ")");
            }

            // Ensure we have a master pool for this IP
            MasterPortPool masterPool = getMasterPool(owner);

            BitSet bitmap = (protocol == prtTcp.protoNum) ? tcpPortBitmap : udpPortBitmap;
            int lastAllocated = (protocol == prtTcp.protoNum) ? lastAllocatedTcpPort : lastAllocatedUdpPort;

            if (debugger.tabNatDebug) {
                logger.info("DEBUG-SUBPOOL: Starting sequential search from port " + (lastAllocated + 1));
            }

            // First try from last allocated port
            for (int i = lastAllocated + 1 - minPort; i < bitmap.size(); i++) {
                int port = i + minPort;
                if (!bitmap.get(i) && !masterPool.isPortInUse(port, protocol)) {
                    bitmap.set(i);
                    masterPool.markPortAsUsed(port, protocol);
                    if (protocol == prtTcp.protoNum) {
                        lastAllocatedTcpPort = port;
                    } else {
                        lastAllocatedUdpPort = port;
                    }

                    if (debugger.tabNatDebug) {
                        logger.info("DEBUG-SUBPOOL: SEQUENTIAL ALLOCATION SUCCESSFUL - Allocated port " + port
                                + " (sequence: " + sequence + ")");
                    }

                    return port;
                }
            }

            // If we reached the end, search from the beginning
            if (debugger.tabNatDebug) {
                logger.info("DEBUG-SUBPOOL: Reached end of range, restarting from beginning");
            }

            for (int i = 0; i < lastAllocated + 1 - minPort; i++) {
                int port = i + minPort;
                if (!bitmap.get(i) && !masterPool.isPortInUse(port, protocol)) {
                    bitmap.set(i);
                    masterPool.markPortAsUsed(port, protocol);
                    if (protocol == prtTcp.protoNum) {
                        lastAllocatedTcpPort = port;
                    } else {
                        lastAllocatedUdpPort = port;
                    }

                    if (debugger.tabNatDebug) {
                        logger.info("DEBUG-SUBPOOL: SEQUENTIAL ALLOCATION SUCCESSFUL - Allocated port " + port
                                + " (sequence: " + sequence + ")");
                    }

                    return port;
                }
            }

            // No free ports found
            if (debugger.tabNatDebug) {
                logger.error("DEBUG-SUBPOOL: SEQUENTIAL ALLOCATION FAILED - No free ports in range "
                        + minPort + "-" + maxPort);
            }

            return -1;
        }

        /**
         * Allocate a random port from the sub-pool
         */
        public int allocateRandomPort(int protocol) {
            // Clearly indicate that random port allocation is being used
            if (debugger.tabNatDebug) {
                logger.info("DEBUG-SUBPOOL: RANDOM PORT ALLOCATION ACTIVE for " + owner
                        + " (protocol: " + protocol + ", sequence: " + sequence
                        + ", range: " + minPort + "-" + maxPort + ")");
            }

            // Ensure we have a master pool for this IP
            MasterPortPool masterPool = getMasterPool(owner);

            BitSet bitmap = (protocol == prtTcp.protoNum) ? tcpPortBitmap : udpPortBitmap;
            int available = 0;

            // Count available ports
            for (int i = 0; i < bitmap.size(); i++) {
                int port = i + minPort;
                if (!bitmap.get(i) && !masterPool.isPortInUse(port, protocol)) {
                    available++;
                }
            }

            if (available == 0) {
                if (debugger.tabNatDebug) {
                    logger.error("DEBUG-SUBPOOL: RANDOM ALLOCATION FAILED - No free ports in range "
                            + minPort + "-" + maxPort);
                }
                return -1;
            }

            if (debugger.tabNatDebug) {
                logger.info("DEBUG-SUBPOOL: Found " + available + " available ports for random selection");
            }

            // Choose a random available port
            int target = bits.random(0, available - 1);
            int found = 0;

            for (int i = 0; i < bitmap.size(); i++) {
                int port = i + minPort;
                if (!bitmap.get(i) && !masterPool.isPortInUse(port, protocol)) {
                    if (found == target) {
                        bitmap.set(i);
                        masterPool.markPortAsUsed(port, protocol);
                        if (protocol == prtTcp.protoNum) {
                            lastAllocatedTcpPort = port;
                        } else {
                            lastAllocatedUdpPort = port;
                        }

                        if (debugger.tabNatDebug) {
                            logger.info("DEBUG-SUBPOOL: RANDOM ALLOCATION SUCCESSFUL - Allocated port " + port
                                    + " (sequence: " + sequence + ")");
                        }

                        return port;
                    }
                    found++;
                }
            }

            // Should never reach here
            if (debugger.tabNatDebug) {
                logger.error("DEBUG-SUBPOOL: RANDOM ALLOCATION FAILED - Logic error in allocation algorithm");
            }
            return -1;
        }

        /**
         * Release a port in the sub-pool
         */
        public void releasePort(int port, int protocol) {
            if (port < minPort || port > maxPort) {
                return;
            }

            // Get master pool for this IP
            MasterPortPool masterPool = getMasterPool(owner);

            BitSet bitmap = (protocol == prtTcp.protoNum) ? tcpPortBitmap : udpPortBitmap;
            int index = port - minPort;

            if (index >= 0 && index < bitmap.size()) {
                bitmap.clear(index);
                masterPool.releasePort(port, protocol);
                if (debugger.tabNatDebug) {
                    logger.debug("Port " + port + " released for " + owner
                            + " (protocol: " + protocol + ", seq: " + sequence + ")");
                }
            }
        }

        /**
         * Get usage statistics for the sub-pool
         */
        public String getUsageStatistics() {
            int totalPorts = maxPort - minPort + 1;
            int tcpUsed = tcpPortBitmap.cardinality();
            int udpUsed = udpPortBitmap.cardinality();
            int totalUsed = tcpUsed + udpUsed;
            int tcpFree = totalPorts - tcpUsed;
            int udpFree = totalPorts - udpUsed;

            double tcpUsagePercent = ((double) tcpUsed / totalPorts) * 100;
            double udpUsagePercent = ((double) udpUsed / totalPorts) * 100;

            String tcpPosition = (lastAllocatedTcpPort >= minPort)
                    ? String.valueOf(lastAllocatedTcpPort) : "-1";
            String udpPosition = (lastAllocatedUdpPort >= minPort)
                    ? String.valueOf(lastAllocatedUdpPort) : "-1";

            // Formatting with pipe separators for direct use in userShow.java
            return sequence + "|"
                    + minPort + "-" + maxPort + "|"
                    + tcpUsed + " used, " + tcpFree + " free (" + String.format("%.1f", tcpUsagePercent) + "%)" + "|"
                    + udpUsed + " used, " + udpFree + " free (" + String.format("%.1f", udpUsagePercent) + "%)" + "|"
                    + tcpPosition + "|"
                    + udpPosition + "|"
                    + totalUsed + " total";
        }
    }

    /**
     * Get or create a master pool for an IP address
     *
     * @param addr The IP address
     * @return The master pool for the IP address
     */
    private MasterPortPool getMasterPool(addrIP addr) {
        String key = "" + addr;
        MasterPortPool pool = masterPools.get(key);
        if (pool == null) {
            // Create a new master pool for this IP
            pool = new MasterPortPool(addr, 1, 65535);
            masterPools.put(key, pool);
        }
        return pool;
    }

    /**
     * Create a sub-pool for an IP address with a specific port range
     *
     * @param addr The IP address
     * @param minPort minimum port
     * @param maxPort maximum port
     * @param sequence sequence
     */
    public synchronized void createSubPool(addrIP addr, int minPort, int maxPort, int sequence) {
        String ipKey = "" + addr;
        String compositeKey = ipKey + "-" + sequence;

        // Ensure we have a master pool for this IP
        getMasterPool(addr);

        // Check if this specific sub-pool already exists
        if (subPools.containsKey(compositeKey)) {
            if (debugger.tabNatDebug) {
                logger.debug("Sub-pool for " + ipKey + " with sequence " + sequence + " already exists");
            }
            return;
        }

        // Create the TreeMap for this IP if it doesn't exist
        TreeMap<Integer, SubPortPool> subPoolsForIp = ipToSubPoolsMap.get(ipKey);
        if (subPoolsForIp == null) {
            subPoolsForIp = new TreeMap<>();
            ipToSubPoolsMap.put(ipKey, subPoolsForIp);
        }

        // Create new sub-pool
        SubPortPool subPool = new SubPortPool(addr, minPort, maxPort, sequence);
        subPools.put(compositeKey, subPool);
        subPoolsForIp.put(sequence, subPool);

        if (debugger.tabNatDebug) {
            logger.debug("Created sub-pool for " + ipKey + " with range "
                    + minPort + "-" + maxPort + " (sequence: " + sequence + ")");
        }
    }

    /**
     * Get the highest priority (lowest sequence) sub-pool for an IP
     *
     * @param ipAddr IP address
     * @return Highest priority sub-pool, or null if none exists
     */
    private SubPortPool getHighestPrioritySubPool(String ipAddr) {
        TreeMap<Integer, SubPortPool> subPoolsForIp = ipToSubPoolsMap.get(ipAddr);
        if (subPoolsForIp == null || subPoolsForIp.isEmpty()) {
            return null;
        }
        // First entry has the lowest key (sequence number), which is highest priority
        return subPoolsForIp.firstEntry().getValue();
    }

    /**
     * Allocate a port from the highest priority sub-pool
     *
     * @param addr IP address
     * @param protocol IP protocol
     * @param useRandomAllocation port allocation method
     * @return port number, -1 on error
     */
    public synchronized int allocatePort(addrIP addr, int protocol, boolean useRandomAllocation) {
        String ipKey = "" + addr;
        SubPortPool highestPriorityPool = getHighestPrioritySubPool(ipKey);

        if (highestPriorityPool == null) {
            if (debugger.tabNatDebug) {
                logger.error("No sub-pool found for " + ipKey);
            }
            return -1;
        }

        int port;
        if (useRandomAllocation) {
            port = highestPriorityPool.allocateRandomPort(protocol);
        } else {
            port = highestPriorityPool.allocatePort(protocol);
        }

        if (port < 0) {
            if (debugger.tabNatDebug) {
                logger.error("Failed to allocate port for " + ipKey
                        + " (protocol: " + protocol + ")");
            }
            return -1;
        }

        if (debugger.tabNatDebug) {
            logger.debug("Allocated port " + port + " for " + ipKey
                    + " (protocol: " + protocol + ", seq: " + highestPriorityPool.getSequence() + ")");
        }

        return port;
    }

    /**
     * Release a port in all sub-pools for an IP address
     *
     * @param addr IP address
     * @param port port number
     * @param protocol IP protocol
     */
    public synchronized void releasePort(addrIP addr, int port, int protocol) {
        String ipKey = "" + addr;
        TreeMap<Integer, SubPortPool> subPoolsForIp = ipToSubPoolsMap.get(ipKey);

        if (subPoolsForIp != null) {
            // Release in all sub-pools covering this port
            for (SubPortPool pool : subPoolsForIp.values()) {
                if (port >= pool.minPort && port <= pool.maxPort) {
                    pool.releasePort(port, protocol);
                }
            }
        }

        // Also ensure the master pool has the port released
        MasterPortPool masterPool = masterPools.get(ipKey);
        if (masterPool != null) {
            masterPool.releasePort(port, protocol);
        }
    }

    /**
     * Check if a specific port is already in use
     *
     * @param addr IP address
     * @param port port number
     * @param protocol IP protocol
     * @return true if yes, false if not
     */
    public synchronized boolean isPortInUse(addrIP addr, int port, int protocol) {
        String ipKey = "" + addr;

        // Get master pool for this IP
        MasterPortPool masterPool = getMasterPool(addr);

        // Check in master pool
        return masterPool.isPortInUse(port, protocol);
    }

    /**
     * Check if any sub-pool exists for an address
     *
     * @param addr IP address
     * @return true if yes, false if not
     */
    public synchronized boolean hasSubPool(addrIP addr) {
        String ipKey = "" + addr;
        TreeMap<Integer, SubPortPool> subPoolsForIp = ipToSubPoolsMap.get(ipKey);
        return subPoolsForIp != null && !subPoolsForIp.isEmpty();
    }

    /**
     * Get master defined sub-pools
     *
     * @return statistics
     */
    public synchronized List<String> getMasterPoolUsages() {
        List<String> result = new ArrayList<String>();
        for (Map.Entry<String, MasterPortPool> entry : masterPools.entrySet()) {
            result.add(entry.getKey() + "|" + entry.getValue().getUsageStatistics());
        }
        return result;
    }

    /**
     * Get all defined sub-pools
     *
     * @return statistics
     */
    public synchronized List<String> getAllPoolUsages() {
        List<String> result = new ArrayList<String>();
        for (Map.Entry<String, TreeMap<Integer, SubPortPool>> entry : ipToSubPoolsMap.entrySet()) {
            String ipKey = entry.getKey();
            TreeMap<Integer, SubPortPool> subPoolsForIp = entry.getValue();
            for (SubPortPool pool : subPoolsForIp.values()) {
                result.add(ipKey + "|" + pool.getUsageStatistics());
            }
        }
        return result;
    }

    /**
     * Get a random available port from the highest priority sub-pool
     *
     * @param addr The address
     * @param protocol The protocol (TCP/UDP)
     * @return A random available port, or -1 if none available
     */
    public int getRandomAvailablePort(addrIP addr, int protocol) {
        return allocatePort(addr, protocol, true);
    }

    /**
     * Get the next available port from the highest priority sub-pool
     *
     * @param addr The address
     * @param protocol The protocol (TCP/UDP)
     * @return The next available port, or -1 if none available
     */
    public int getNextAvailablePort(addrIP addr, int protocol) {
        return allocatePort(addr, protocol, false);
    }

    /**
     * Mark a port as used in all sub-pools that cover it
     *
     * @param addr The address
     * @param port The port to mark
     * @param protocol The protocol (TCP/UDP)
     */
    public synchronized void markPortAsUsed(addrIP addr, int port, int protocol) {
        String ipKey = "" + addr;

        // Debug-Log at the beginning of the method
        if (debugger.tabNatDebug) {
            logger.info("DEBUG-PORT-MARK: Marking port " + port + " as used for " + ipKey
                    + " (protocol: " + protocol + ")");
        }

        TreeMap<Integer, SubPortPool> subPoolsForIp = ipToSubPoolsMap.get(ipKey);

        if (subPoolsForIp == null) {
            if (debugger.tabNatDebug) {
                logger.error("DEBUG-PORT-MARK: No sub-pools exist for " + addr);
            }
            return;
        }

        // Mark port as used in any sub-pool where it falls within range
        boolean markedInAnyPool = false;
        for (SubPortPool pool : subPoolsForIp.values()) {
            if (port >= pool.minPort && port <= pool.maxPort) {
                BitSet bitmap = (protocol == prtTcp.protoNum) ? pool.tcpPortBitmap : pool.udpPortBitmap;
                int index = port - pool.minPort;
                bitmap.set(index);
                markedInAnyPool = true;

                // Debug log for each pool where the port is marked
                if (debugger.tabNatDebug) {
                    logger.info("DEBUG-PORT-MARK: Marked port " + port + " in pool with sequence "
                            + pool.getSequence() + " for " + ipKey);
                }
            }
        }

        // Always mark in the master pool
        MasterPortPool masterPool = getMasterPool(addr);
        masterPool.markPortAsUsed(port, protocol);

        if (!markedInAnyPool && debugger.tabNatDebug) {
            logger.warn("DEBUG-PORT-MARK: Port " + port + " does not fall within any sub-pool range for " + addr);
        }

        // Debug-Log at the end of the method
        if (debugger.tabNatDebug) {
            logger.info("DEBUG-PORT-MARK: Port marking complete for " + port + " on " + ipKey);
        }
    }

    /**
     * Get a specific sub-pool for an IP by sequence number
     *
     * @param ipAddr IP address string
     * @param sequence Sequence number
     * @return Sub-pool with the specified sequence, or null if none exists
     */
    public SubPortPool getSubPoolBySequence(String ipAddr, int sequence) {
        TreeMap<Integer, SubPortPool> subPoolsForIp = ipToSubPoolsMap.get(ipAddr);
        if (subPoolsForIp == null || subPoolsForIp.isEmpty()) {
            return null;
        }
        // Get the specific sub-pool for this sequence
        return subPoolsForIp.get(sequence);
    }

    /**
     * Allocate a port from a specific sub-pool by sequence number
     *
     * @param addr IP address
     * @param protocol Protocol number
     * @param sequence Sequence number of the sub-pool to use
     * @param useRandomAllocation Whether to use random allocation
     * @return Allocated port, or -1 if failed
     */
    public synchronized int allocatePortFromSequence(addrIP addr, int protocol, int sequence, boolean useRandomAllocation) {
        String ipKey = "" + addr;

        // Clearer debug output of the used strategy
        if (debugger.tabNatDebug) {
            logger.info("DEBUG-PORT-ALLOC: Using " + (useRandomAllocation ? "RANDOM" : "SEQUENTIAL")
                    + " allocation for " + ipKey + " (protocol: " + protocol + ", sequence: " + sequence + ")");
        }

        SubPortPool specificPool = getSubPoolBySequence(ipKey, sequence);

        // If no pool exists with this sequence number, we create one with the full port range
        if (specificPool == null) {
            if (debugger.tabNatDebug) {
                logger.info("DEBUG-PORT-ALLOC: No pool found with sequence " + sequence
                        + " for " + ipKey + ", creating new pool");
            }

            // Full port range as default value
            int defaultMinPort = 1;
            int defaultMaxPort = 65535;

            createSubPool(addr, defaultMinPort, defaultMaxPort, sequence);
            specificPool = getSubPoolBySequence(ipKey, sequence);

            if (specificPool == null) {
                if (debugger.tabNatDebug) {
                    logger.error("DEBUG-PORT-ALLOC: Failed to create pool for " + ipKey);
                }
                return -1;
            }
        }

        if (debugger.tabNatDebug) {
            logger.info("DEBUG-PORT-ALLOC: Found pool with range " + specificPool.minPort + "-"
                    + specificPool.maxPort + ", last allocated TCP port: " + specificPool.lastAllocatedTcpPort
                    + ", UDP port: " + specificPool.lastAllocatedUdpPort);
        }

        // Call the correct allocation method based on useRandomAllocation
        int port;
        if (useRandomAllocation) {
            if (debugger.tabNatDebug) {
                logger.info("DEBUG-PORT-ALLOC: Calling randomPort allocation method");
            }
            port = specificPool.allocateRandomPort(protocol);
        } else {
            if (debugger.tabNatDebug) {
                logger.info("DEBUG-PORT-ALLOC: Calling sequential allocation method");
            }
            port = specificPool.allocatePort(protocol);
        }

        if (port < 0) {
            if (debugger.tabNatDebug) {
                logger.error("DEBUG-PORT-ALLOC: Port allocation FAILED for " + ipKey);
            }
            return -1;
        }

        if (debugger.tabNatDebug) {
            logger.info("DEBUG-PORT-ALLOC: Successfully allocated port " + port + " using "
                    + (useRandomAllocation ? "RANDOM" : "SEQUENTIAL") + " allocation (sequence: " + sequence + ")");
        }

        return port;
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
    private int allocatePortFromSequence(tabNatPort prt, addrIP srcAddr, int sequenceNum, int protocol, boolean useRandomAllocation) {
        // First check if a sub-pool with the specified sequence number exists
        if (!prt.hasSubPool(srcAddr)) {
            if (debugger.tabNatDebug) {
                logger.error("No sub-pools exist for " + srcAddr);
            }
            return -1;
        }

        // Use the new method that allocates directly from the pool with the specified sequence number
        int port = prt.allocatePortFromSequence(srcAddr, protocol, sequenceNum, useRandomAllocation);

        if (port < 0) {
            if (debugger.tabNatDebug) {
                logger.error("Port allocation failed for " + srcAddr
                        + " (protocol: " + protocol + ", sequence: " + sequenceNum + ")");
            }
        } else if (debugger.tabNatDebug) {
            logger.debug("Allocated port " + port + " for " + srcAddr
                    + " (protocol: " + protocol + ", sequence: " + sequenceNum + ")");
        }

        return port;
    }

    /**
     * Allocates a port using the PreserveOriginalThenSequential strategy Tries
     * to keep original port if possible, otherwise allocates next available
     *
     * @param n NAT translation entry
     * @return allocated port number or -1 if failed
     */
    private int allocatePreserveOriginalThenSequentialPort(tabNatPort prt, tabNatTraN n) {
        if (debugger.tabNatDebug) {
            logger.info("DEBUG-PRESERVE-ORIGINAL: Starting port allocation with PreserveOriginalThenSequential");
            logger.info("DEBUG-PRESERVE-ORIGINAL: Original port is " + n.origSrcPort
                    + ", valid range is " + (rangeMin > 0 ? rangeMin : "1") + "-"
                    + (rangeMax > 0 ? rangeMax : "65535"));
        }

        // First check if the original port is in the valid range
        int effectiveRangeMin = (rangeMin > 0) ? rangeMin : 1;
        int effectiveRangeMax = (rangeMax > 0) ? rangeMax : 65535;

        boolean portInRange = (n.origSrcPort >= effectiveRangeMin && n.origSrcPort <= effectiveRangeMax);
        if (debugger.tabNatDebug) {
            if (!portInRange) {
                logger.info("DEBUG-PRESERVE-ORIGINAL: Original port " + n.origSrcPort
                        + " is outside the valid range " + effectiveRangeMin + "-" + effectiveRangeMax);
            } else {
                logger.info("DEBUG-PRESERVE-ORIGINAL: Original port " + n.origSrcPort
                        + " is within the valid range " + effectiveRangeMin + "-" + effectiveRangeMax);
            }
        }

        // Then check if the original port is already in use
        boolean portIsUsed = prt.isPortInUse(n.newSrcAddr, n.origSrcPort, n.protocol);
        if (debugger.tabNatDebug) {
            if (portIsUsed) {
                logger.info("DEBUG-PRESERVE-ORIGINAL: Original port " + n.origSrcPort
                        + " is already in use for " + n.newSrcAddr);
            } else {
                logger.info("DEBUG-PRESERVE-ORIGINAL: Original port " + n.origSrcPort
                        + " is available for " + n.newSrcAddr);
            }
        }

        // Keep the original port if it is in the valid range and not in use
        if (portInRange && !portIsUsed) {
            // Mark the original port as used
            prt.markPortAsUsed(n.newSrcAddr, n.origSrcPort, n.protocol);

            if (debugger.tabNatDebug) {
                logger.info("DEBUG-PRESERVE-ORIGINAL: SUCCESS - Keeping original port " + n.origSrcPort
                        + " for " + n.newSrcAddr);
                logger.debug("Kept original port " + n.origSrcPort + " for " + n.newSrcAddr
                        + " (preserve original then sequential, sequence: " + sequence + ")");
            }

            return n.origSrcPort;
        }

        // If we get here, we cannot use the original port
        if (debugger.tabNatDebug) {
            logger.info("DEBUG-PRESERVE-ORIGINAL: Cannot use original port " + n.origSrcPort
                    + ", falling back to sequential allocation");
        }

        // Sequential port allocation as fallback
        int allocatedPort = allocatePortFromSequence(prt, n.newSrcAddr, sequence, n.protocol, false);

        if (debugger.tabNatDebug) {
            if (allocatedPort < 0) {
                logger.error("DEBUG-PRESERVE-ORIGINAL: Sequential allocation FAILED for " + n.newSrcAddr);
            } else {
                logger.info("DEBUG-PRESERVE-ORIGINAL: Successfully allocated sequential port "
                        + allocatedPort + " for " + n.newSrcAddr);
            }
        }

        return allocatedPort;
    }

    /**
     * Allocates a port using the PreserveOriginalThenRandom strategy Tries to
     * keep original port if possible, otherwise allocates random port
     *
     * @param n NAT translation entry
     * @return allocated port number or -1 if failed
     */
    private int allocatePreserveOriginalThenRandomPort(tabNatPort prt, tabNatTraN n) {
        if (debugger.tabNatDebug) {
            logger.info("DEBUG-PRESERVE-ORIGINAL: Starting port allocation with PreserveOriginalThenRandom");
            logger.info("DEBUG-PRESERVE-ORIGINAL: Original port is " + n.origSrcPort
                    + ", valid range is " + (rangeMin > 0 ? rangeMin : "1") + "-"
                    + (rangeMax > 0 ? rangeMax : "65535"));
        }

        // First check if the original port is in the valid range
        int effectiveRangeMin = (rangeMin > 0) ? rangeMin : 1;
        int effectiveRangeMax = (rangeMax > 0) ? rangeMax : 65535;

        boolean portInRange = (n.origSrcPort >= effectiveRangeMin && n.origSrcPort <= effectiveRangeMax);
        if (debugger.tabNatDebug) {
            if (!portInRange) {
                logger.info("DEBUG-PRESERVE-ORIGINAL: Original port " + n.origSrcPort
                        + " is outside the valid range " + effectiveRangeMin + "-" + effectiveRangeMax);
            } else {
                logger.info("DEBUG-PRESERVE-ORIGINAL: Original port " + n.origSrcPort
                        + " is within the valid range " + effectiveRangeMin + "-" + effectiveRangeMax);
            }
        }

        // Then check if the original port is already in use
        boolean portIsUsed = prt.isPortInUse(n.newSrcAddr, n.origSrcPort, n.protocol);
        if (debugger.tabNatDebug) {
            if (portIsUsed) {
                logger.info("DEBUG-PRESERVE-ORIGINAL: Original port " + n.origSrcPort
                        + " is already in use for " + n.newSrcAddr);
            } else {
                logger.info("DEBUG-PRESERVE-ORIGINAL: Original port " + n.origSrcPort
                        + " is available for " + n.newSrcAddr);
            }
        }

        // Keep the original port if it is in the valid range and not in use
        if (portInRange && !portIsUsed) {
            // Mark the original port as used
            prt.markPortAsUsed(n.newSrcAddr, n.origSrcPort, n.protocol);

            if (debugger.tabNatDebug) {
                logger.info("DEBUG-PRESERVE-ORIGINAL: SUCCESS - Keeping original port " + n.origSrcPort
                        + " for " + n.newSrcAddr);
            }

            if (debugger.tabNatDebug) {
                logger.debug("Kept original port " + n.origSrcPort + " for " + n.newSrcAddr
                        + " (preserve original then random, sequence: " + sequence + ")");
            }
            return n.origSrcPort;
        }

        // If we get here, we cannot use the original port
        if (debugger.tabNatDebug) {
            logger.info("DEBUG-PRESERVE-ORIGINAL: Cannot use original port " + n.origSrcPort
                    + ", falling back to random allocation");
        }

        // Random port allocation as fallback
        int allocatedPort = allocatePortFromSequence(prt, n.newSrcAddr, sequence, n.protocol, true);

        if (debugger.tabNatDebug) {
            if (allocatedPort < 0) {
                logger.error("DEBUG-PRESERVE-ORIGINAL: Random allocation FAILED for " + n.newSrcAddr);
            } else {
                logger.info("DEBUG-PRESERVE-ORIGINAL: Successfully allocated random port "
                        + allocatedPort + " for " + n.newSrcAddr);
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
    private void addPortRangeForSequence(tabNatPort prt, addrIP srcAddr, int sequenceNum) {
        if (debugger.tabNatDebug) {
            logger.info("DEBUG-NAT-POOL: Adding port range for " + srcAddr + " with sequence " + sequenceNum);
        }

        // If rangeMin and rangeMax are configured, we use these values
        if (rangeMin > 0 && rangeMax > 0) {
            if (debugger.tabNatDebug) {
                logger.info("DEBUG-NAT-POOL: Using configured range " + rangeMin + "-" + rangeMax);
            }

            // Create pool with the specified sequence number
            prt.createSubPool(srcAddr, rangeMin, rangeMax, sequenceNum);

        } else {
            // If no ranges are configured, we use the default ranges
            int defaultMinPort = 1;
            int defaultMaxPort = 65535;

            if (debugger.tabNatDebug) {
                logger.info("DEBUG-NAT-POOL: Using default range " + defaultMinPort + "-" + defaultMaxPort);
            }

            // Create pool with the specified sequence number
            prt.createSubPool(srcAddr, defaultMinPort, defaultMaxPort, sequenceNum);

        }

        // Debug log to verify if the pool was created
        if (debugger.tabNatDebug) {
            logger.info("DEBUG-NAT-POOL: Pool creation complete, checking if pool exists: "
                    + prt.hasSubPool(srcAddr));
        }
    }

    /**
     * release resources when the NAT translation is removed This is called when
     * a NAT translation expires or is manually cleared
     *
     * @param prt ports manager
     */
    public void releaseResources(tabNatPort prt) {
        // Release allocated ports back to the pool if needed
        if (protocol == prtTcp.protoNum || protocol == prtUdp.protoNum) {

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
            if (newSrcAddr != null && newSrcPort > 0 && prt.hasSubPool(newSrcAddr)) {
                if (debugger.tabNatDebug) {
                    logger.info("DEBUG-RELEASE: Checking if need to release newSrcPort=" + newSrcPort
                            + " for newSrcAddr=" + newSrcAddr);
                }

                // Check if the port is actually marked in the pool
                if (prt.isPortInUse(newSrcAddr, newSrcPort, protocol)) {
                    prt.releasePort(newSrcAddr, newSrcPort, protocol);

                    if (debugger.tabNatDebug) {
                        logger.info("DEBUG-RELEASE: Released port " + newSrcPort
                                + " for " + newSrcAddr + " (protocol: " + protocol + ")");
                    }
                }
            }

            // 2. For trgport rules, the source port (origSrcPort) in the pool of the destination address (origTrgAddr) must be released
            if (origTrgAddr != null && origSrcPort > 0 && prt.hasSubPool(origTrgAddr)) {
                if (debugger.tabNatDebug) {
                    logger.info("DEBUG-RELEASE: Checking if need to release origSrcPort=" + origSrcPort
                            + " for origTrgAddr=" + origTrgAddr + " (potential trgport rule)");
                }

                // Check if the port is actually marked in the pool
                if (prt.isPortInUse(origTrgAddr, origSrcPort, protocol)) {
                    prt.releasePort(origTrgAddr, origSrcPort, protocol);

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
