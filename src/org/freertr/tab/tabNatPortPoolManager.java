package org.freertr.tab;

import java.util.BitSet;
import java.util.HashMap;
import java.util.Map;
import java.util.TreeMap;
import java.util.concurrent.locks.ReentrantLock;
import org.freertr.addr.addrIP;
import org.freertr.prt.prtTcp;
import org.freertr.prt.prtUdp;
import org.freertr.util.bits;
import org.freertr.util.logger;
import org.freertr.util.debugger;

/**
 * Singleton class for central management of NAT port pools Each IP address has
 * its own independent port pool
 */
public class tabNatPortPoolManager {

    // Singleton instance
    private final static tabNatPortPoolManager INSTANCE = new tabNatPortPoolManager();

    // Map of IP addresses to their independent master pools
    private final Map<String, MasterPortPool> masterPools;

    // Map of composite keys (IP+sequence) to sub-pools
    private final Map<String, SubPortPool> subPools;

    // Map of IP addresses to a sorted map of sequence numbers to sub-pools
    // Used to quickly find all sub-pools for an IP and determine the highest priority one
    private final Map<String, TreeMap<Integer, SubPortPool>> ipToSubPoolsMap;

    // Lock for thread safety
    private final ReentrantLock lock = new ReentrantLock();

    /**
     * Create a composite key for the subPools map
     *
     * @param ipAddr IP address
     * @param sequence Sequence number
     * @return Composite key
     */
    private String createCompositeKey(String ipAddr, int sequence) {
        return ipAddr + ":" + sequence;
    }

    /**
     * Private constructor for singleton pattern
     */
    private tabNatPortPoolManager() {
        masterPools = new HashMap<>();
        subPools = new HashMap<>();
        ipToSubPoolsMap = new HashMap<>();
    }

    /**
     * Get the singleton instance of the tabNatPortPoolManager
     *
     * @return The tabNatPortPoolManager instance
     */
    public static tabNatPortPoolManager getInstance() {
        return INSTANCE;
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
     */
    public void createSubPool(addrIP addr, int minPort, int maxPort, int sequence) {
        lock.lock();
        try {
            String ipKey = "" + addr;
            String compositeKey = createCompositeKey(ipKey, sequence);

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
        } finally {
            lock.unlock();
        }
    }

    /**
     * Create a sub-pool for an IP address with a specific port range (default
     * sequence 1000)
     */
    public void createSubPool(addrIP addr, int minPort, int maxPort) {
        // Default sequence number 1000
        createSubPool(addr, minPort, maxPort, 1000);
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
     */
    public int allocatePort(addrIP addr, int protocol, boolean useRandomAllocation) {
        lock.lock();
        try {
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
        } finally {
            lock.unlock();
        }
    }

    /**
     * Release a port in all sub-pools for an IP address
     */
    public void releasePort(addrIP addr, int port, int protocol) {
        lock.lock();
        try {
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
        } finally {
            lock.unlock();
        }
    }

    /**
     * Check if a specific port is already in use
     */
    public boolean isPortInUse(addrIP addr, int port, int protocol) {
        lock.lock();
        try {
            String ipKey = "" + addr;

            // Get master pool for this IP
            MasterPortPool masterPool = getMasterPool(addr);

            // Check in master pool
            return masterPool.isPortInUse(port, protocol);
        } finally {
            lock.unlock();
        }
    }

    /**
     * Get usage statistics for all sub-pools of an IP
     */
    public String getPoolUsage(addrIP addr) {
        lock.lock();
        try {
            String ipKey = "" + addr;
            TreeMap<Integer, SubPortPool> subPoolsForIp = ipToSubPoolsMap.get(ipKey);

            if (subPoolsForIp == null || subPoolsForIp.isEmpty()) {
                return "No sub-pools for " + ipKey;
            }

            StringBuilder sb = new StringBuilder();
            sb.append("Sub-pools for ").append(ipKey).append(":\n");

            for (SubPortPool pool : subPoolsForIp.values()) {
                sb.append("  ").append(pool.getUsageStatistics()).append("\n");
            }

            // Add master pool information
            MasterPortPool masterPool = masterPools.get(ipKey);
            if (masterPool != null) {
                sb.append("Master pool: ").append(masterPool.getUsageStatistics());
            }

            return "" + sb;
        } finally {
            lock.unlock();
        }
    }

    /**
     * Check if any sub-pool exists for an address
     */
    public boolean hasSubPool(addrIP addr) {
        lock.lock();
        try {
            String ipKey = "" + addr;
            TreeMap<Integer, SubPortPool> subPoolsForIp = ipToSubPoolsMap.get(ipKey);
            return subPoolsForIp != null && !subPoolsForIp.isEmpty();
        } finally {
            lock.unlock();
        }
    }

    /**
     * Remove all sub-pools for an IP address
     */
    public void removeSubPool(addrIP addr) {
        lock.lock();
        try {
            String ipKey = "" + addr;

            // Remove all sub-pools for this IP from the composite map
            TreeMap<Integer, SubPortPool> subPoolsForIp = ipToSubPoolsMap.remove(ipKey);

            if (subPoolsForIp != null) {
                // Remove each sub-pool from the subPools map
                for (Map.Entry<Integer, SubPortPool> entry : subPoolsForIp.entrySet()) {
                    String compositeKey = createCompositeKey(ipKey, entry.getKey());
                    subPools.remove(compositeKey);
                }
            }

            // Remove the master pool
            masterPools.remove(ipKey);

            if (debugger.tabNatDebug) {
                logger.debug("Removed all port pools for " + ipKey);
            }
        } finally {
            lock.unlock();
        }
    }

    /**
     * Get all defined sub-pools
     */
    public Map<addrIP, String> getAllPoolUsages() {
        lock.lock();
        try {
            Map<addrIP, String> result = new HashMap<>();

            // Add master pools summary
            addrIP masterKey = new addrIP(); // Empty/dummy IP for master pool summary
            StringBuilder masterPoolSummary = new StringBuilder("MASTER POOLS:\n");
            for (Map.Entry<String, MasterPortPool> entry : masterPools.entrySet()) {
                masterPoolSummary.append(entry.getKey())
                        .append(": ")
                        .append(entry.getValue().getUsageStatistics())
                        .append("\n");
            }
            result.put(masterKey, "" + masterPoolSummary);

            // Add sub-pools by IP
            for (Map.Entry<String, TreeMap<Integer, SubPortPool>> entry : ipToSubPoolsMap.entrySet()) {
                String ipKey = entry.getKey();
                TreeMap<Integer, SubPortPool> subPoolsForIp = entry.getValue();

                // Create a new addrIP to represent this pool's owner in the result map
                addrIP poolAddr = new addrIP();
                poolAddr.fromString(ipKey);

                StringBuilder sb = new StringBuilder();

                for (SubPortPool pool : subPoolsForIp.values()) {
                    sb.append(pool.getUsageStatistics()).append("\n");
                }

                result.put(poolAddr, "" + sb);
            }

            return result;
        } finally {
            lock.unlock();
        }
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
    public void markPortAsUsed(addrIP addr, int port, int protocol) {
        lock.lock();
        try {
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
        } finally {
            lock.unlock();
        }
    }

    /**
     * Get the number of TCP ports in use across all sub-pools for an address
     *
     * @param addr The address
     * @return Number of TCP ports in use
     */
    public int getTcpUsedCount(addrIP addr) {
        lock.lock();
        try {
            String ipKey = "" + addr;
            MasterPortPool masterPool = masterPools.get(ipKey);
            if (masterPool == null) {
                return 0;
            }

            // Count used TCP ports in the master pool
            int count = 0;
            for (int port = 1; port <= 65535; port++) {
                if (masterPool.isPortInUse(port, prtTcp.protoNum)) {
                    count++;
                }
            }
            return count;
        } finally {
            lock.unlock();
        }
    }

    /**
     * Get the number of UDP ports in use across all sub-pools for an address
     *
     * @param addr The address
     * @return Number of UDP ports in use
     */
    public int getUdpUsedCount(addrIP addr) {
        lock.lock();
        try {
            String ipKey = "" + addr;
            MasterPortPool masterPool = masterPools.get(ipKey);
            if (masterPool == null) {
                return 0;
            }

            // Count used UDP ports in the master pool
            int count = 0;
            for (int port = 1; port <= 65535; port++) {
                if (masterPool.isPortInUse(port, prtUdp.protoNum)) {
                    count++;
                }
            }
            return count;
        } finally {
            lock.unlock();
        }
    }

    /**
     * Get the master pool usage statistics for a specific IP
     *
     * @param addr The IP address
     * @return String containing usage statistics of the master pool
     */
    public String getMasterPoolUsage(addrIP addr) {
        lock.lock();
        try {
            String key = "" + addr;
            MasterPortPool pool = masterPools.get(key);
            if (pool == null) {
                return "No master pool for " + addr;
            }
            return pool.getUsageStatistics();
        } finally {
            lock.unlock();
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
    public int allocatePortFromSequence(addrIP addr, int protocol, int sequence, boolean useRandomAllocation) {
        lock.lock();
        try {
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
        } finally {
            lock.unlock();
        }
    }

    /**
     * Get all sub-pools for a specific IP address as a TreeMap of sequence to
     * pool
     *
     * @param addr IP address
     * @return TreeMap of sequence number to SubPortPool, or null if no pools
     * exist
     */
    public TreeMap<Integer, SubPortPool> getSubPoolsForIp(addrIP addr) {
        lock.lock();
        try {
            String ipKey = "" + addr;
            return ipToSubPoolsMap.get(ipKey);
        } finally {
            lock.unlock();
        }
    }
}
