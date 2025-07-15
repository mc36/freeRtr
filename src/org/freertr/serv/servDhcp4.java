package org.freertr.serv;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Timer;
import java.util.TimerTask;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv4;
import org.freertr.addr.addrMac;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgIfc;
import org.freertr.cfg.cfgVrf;
import org.freertr.ip.ipFwdIface;
import org.freertr.pack.packDhcp4;
import org.freertr.pack.packDhcpOption;
import org.freertr.pack.packHolder;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGenConn;
import org.freertr.prt.prtServP;
import org.freertr.prt.prtServS;
import org.freertr.prt.prtUdp;
import org.freertr.tab.tabGen;
import org.freertr.user.userFilter;
import org.freertr.user.userFormat;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.counter;
import org.freertr.util.debugger;
import org.freertr.util.logger;
import org.freertr.util.state;

/**
 * dynamic host config protocol (rfc2131) server and relay
 *
 * @author matecsaba
 */
public class servDhcp4 extends servGeneric implements prtServS, prtServP {

    /**
     * create instance
     */
    public servDhcp4() {
    }

    /**
     * operation mode enum
     */
    public enum dhcpMode {
        /**
         * server
         */
        server,
        /**
         * relay
         */
        relay
    }

    /**
     * current operation mode
     */
    public dhcpMode mode = dhcpMode.server;

    // Server mode fields
    /**
     * lower address
     */
    public addrIPv4 poolLo;

    /**
     * highest address
     */
    public addrIPv4 poolHi;

    /**
     * mask
     */
    public addrIPv4 poolMsk;

    /**
     * network
     */
    public addrIPv4 poolNet;

    /**
     * gateway
     */
    public addrIPv4 gateway;

    /**
     * network mask
     */
    public addrIPv4 netmask;

    /**
     * dns1
     */
    public addrIPv4 dns1;

    /**
     * dns2
     */
    public addrIPv4 dns2;

    /**
     * boot server
     */
    public String bootServ = "";

    /**
     * boot file
     */
    public String bootFile = "";

    /**
     * domain name
     */
    public String domNam = "";

    /**
     * lease time
     */
    public int lease = 12 * 60 * 60 * 1000;

    /**
     * renew time
     */
    public int renew = lease / 2;

    /**
     * remember time
     */
    public int remember = 0;

    /**
     * options to add
     */
    public tabGen<packDhcpOption> options = new tabGen<packDhcpOption>();

    private List<servDhcp4bind> bindings = new ArrayList<servDhcp4bind>();

    private tabGen<servDhcp4bind> forbidden = new tabGen<servDhcp4bind>();

    private String bindFile;

    private Timer purgeTimer;

    // Relay mode fields
    /**
     * helper addresses - DHCP servers to forward to
     */
    private List<addrIP> helperAddresses = new ArrayList<addrIP>();

    /**
     * add agent options flag
     */
    private boolean addAgentOptions = false;

    /**
     * circuit ID template for agent options
     */
    private String circuitIdTemplate = "interface-name";

    /**
     * remote ID template for agent options
     */
    private String remoteIdTemplate = "hostname";

    /**
     * link selection address for agent options
     */
    private addrIPv4 linkSelectionAddr = null;

    /**
     * subscriber ID for agent options
     */
    private String subscriberId = "";

    /**
     * maximum hop count
     */
    private int maxHopCount = 10;

    /**
     * agent relay mode - how to handle existing agent options
     */
    private String agentRelayMode = "append";

    // Agent Options Constants (RFC 3046)
    private final static int DHCP_OPTION_RELAY_AGENT_INFO = 82;
    private final static int AGENT_CIRCUIT_ID_SUBOPTION = 1;
    private final static int AGENT_REMOTE_ID_SUBOPTION = 2;
    private final static int AGENT_LINK_SELECTION_SUBOPTION = 5;
    private final static int AGENT_SUBSCRIBER_ID_SUBOPTION = 6;

    /**
     * list of interfaces for relay (multiple interface support)
     */
    private List<cfgIfc> relayInterfaces = new ArrayList<cfgIfc>();

    /**
     * DHCP Relay Statistics
     */
    private servDhcp4RelayStats relayStats = new servDhcp4RelayStats();

    /**
     * Statistics reset timestamp
     */
    private long statsResetTime = bits.getTime();

    /**
     * defaults text
     */
    public final static userFilter[] defaultF = {
        new userFilter("server dhcp4 .*", cmds.tabulator + "port " + packDhcp4.portSnum, null),
        new userFilter("server dhcp4 .*", cmds.tabulator + "protocol " + proto2string(protoIp4 + protoUdp), null),
        new userFilter("server dhcp4 .*", cmds.tabulator + "mode server", null),
        new userFilter("server dhcp4 .*", cmds.tabulator + "boot-server ", null),
        new userFilter("server dhcp4 .*", cmds.tabulator + "boot-file ", null),
        new userFilter("server dhcp4 .*", cmds.tabulator + "lease 43200000", null),
        new userFilter("server dhcp4 .*", cmds.tabulator + "renew 21600000", null),
        new userFilter("server dhcp4 .*", cmds.tabulator + "remember 0", null),
        new userFilter("server dhcp4 .*", cmds.tabulator + cmds.negated + cmds.tabulator + "bind-file", null),
        new userFilter("server dhcp4 .*", cmds.tabulator + cmds.negated + cmds.tabulator + "add-agent-options", null),
        new userFilter("server dhcp4 .*", cmds.tabulator + "circuit-id-template interface-name", null),
        new userFilter("server dhcp4 .*", cmds.tabulator + "remote-id-template hostname", null),
        new userFilter("server dhcp4 .*", cmds.tabulator + cmds.negated + cmds.tabulator + "link-selection-address", null),
        new userFilter("server dhcp4 .*", cmds.tabulator + cmds.negated + cmds.tabulator + "subscriber-id", null),
        new userFilter("server dhcp4 .*", cmds.tabulator + "max-hop-count 10", null),
        new userFilter("server dhcp4 .*", cmds.tabulator + "agent-relay-mode append", null)
    };

    public userFilter[] srvDefFlt() {
        return defaultF;
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        if (mode == dhcpMode.server) {
            pipe.setTime(10000);
            new servDhcp4worker(this, pipe, id);
            return false;
        } else {
            // Relay mode: configure pipe properly if not null
            if (pipe != null) {
                pipe.setTime(1000); // Use longer timeout
                pipe.setClose(); // Explicit close after sending
            }
            return false; // Accept connection (false = success)
        }
    }

    public void srvShRun(String beg, List<String> l, int filter) {
        l.add(beg + "mode " + mode);

        if (mode == dhcpMode.server) {
            if ((poolLo == null) || (poolHi == null)) {
                l.add(beg + "no pool");
            } else {
                l.add(beg + "pool " + poolLo + " " + poolHi);
            }
            if (gateway == null) {
                l.add(beg + "no gateway");
            } else {
                l.add(beg + "gateway " + gateway);
            }
            if (netmask == null) {
                l.add(beg + "no netmask");
            } else {
                l.add(beg + "netmask " + netmask);
            }
            String s = "";
            if (dns1 != null) {
                s += " " + dns1;
            }
            if (dns2 != null) {
                s += " " + dns2;
            }
            if (s.length() < 1) {
                l.add(beg + "no dns-server");
            } else {
                l.add(beg + "dns-server" + s);
            }
            l.add(beg + "boot-server " + bootServ);
            l.add(beg + "boot-file " + bootFile);
            l.add(beg + "domain-name " + domNam);
            l.add(beg + "lease " + lease);
            l.add(beg + "renew " + renew);
            l.add(beg + "remember " + remember);
            for (int i = 0; i < forbidden.size(); i++) {
                servDhcp4bind ntry = forbidden.get(i);
                l.add(beg + "forbidden " + ntry.mac);
            }
            synchronized (bindings) {
                for (int i = 0; i < bindings.size(); i++) {
                    servDhcp4bind ntry = bindings.get(i);
                    if (!ntry.confed) {
                        continue;
                    }
                    l.add(beg + "static " + ntry.mac + " " + ntry.ip);
                }
            }
            for (int o = 0; o < options.size(); o++) {
                l.add(beg + "option " + options.get(o));
            }
            cmds.cfgLine(l, bindFile == null, beg, "bind-file", bindFile);
        } else {
            // Relay mode configuration
            String helpers = "";
            for (int i = 0; i < helperAddresses.size(); i++) {
                helpers += " " + helperAddresses.get(i);
            }
            if (helpers.length() > 0) {
                l.add(beg + "helper-addresses" + helpers);
            }
            cmds.cfgLine(l, !addAgentOptions, beg, "add-agent-options", "");
            if (circuitIdTemplate != null) {
                l.add(beg + "circuit-id-template " + circuitIdTemplate);
            }
            if (remoteIdTemplate != null) {
                l.add(beg + "remote-id-template " + remoteIdTemplate);
            }
            if (linkSelectionAddr != null) {
                l.add(beg + "link-selection-address " + linkSelectionAddr);
            }
            if (subscriberId != null && !subscriberId.isEmpty()) {
                l.add(beg + "subscriber-id " + subscriberId);
            }
            l.add(beg + "max-hop-count " + maxHopCount);
            l.add(beg + "agent-relay-mode " + agentRelayMode);
        }
    }

    // prtServP interface methods for relay mode
    public void datagramReady(prtGenConn id) {
    }

    public void datagramClosed(prtGenConn id) {
    }

    public void datagramWork(prtGenConn id) {
    }

    public boolean datagramError(prtGenConn id, packHolder pck, addrIP rtr, counter.reasons err, int lab) {
        return false;
    }

    public boolean datagramState(prtGenConn id, state.states stat) {
        return false;
    }

    public boolean datagramRecv(prtGenConn id, packHolder pck) {
        if (mode != dhcpMode.relay) {
            return false;
        }
        if (debugger.servDhcp4traf) {
            logger.debug("dhcp relay recv from " + id.peerAddr + " port " + id.portRem);
        }
        // Parse DHCP packet
        packDhcp4 dhcp = new packDhcp4();
        if (dhcp.parseHeader(pck, false)) {
            if (debugger.servDhcp4traf) {
                logger.debug("dhcp relay bad packet");
            }
            id.setClosing(); // Close connection for bad packets
            return false;
        }

        // Check hop count
        if (dhcp.bootpHops >= maxHopCount) {
            if (debugger.servDhcp4traf) {
                logger.debug("dhcp relay max hops exceeded: " + dhcp.bootpHops + " >= " + maxHopCount);
            }
            id.setClosing(); // Close connection for dropped packets
            return false;
        }

        boolean result = false;
        // Handle DHCP packet based on operation type
        // Multi-hop relays: Requests can come from both client port (68) and server port (67)
        if (dhcp.bootpOp == packDhcp4.bootpOpRequest) {
            // This is a DHCP request - relay to servers
            // Can come from client port (68) for direct clients OR server port (67) for multi-hop relay
            if (debugger.servDhcp4traf) {
                logger.debug("dhcp4 relay processing request from port " + id.portRem
                        + " (direct client: " + (id.portRem == packDhcp4.portCnum)
                        + ", multi-hop: " + (id.portRem == packDhcp4.portSnum) + ")");
            }
            result = relayClientToServer(dhcp, pck, id);
        } else if (dhcp.bootpOp == packDhcp4.bootpOpReply) {
            // This is a DHCP reply - relay to client
            // Always comes from server port (67)
            if (debugger.servDhcp4traf) {
                logger.debug("dhcp4 relay processing reply from port " + id.portRem);
            }
            result = relayServerToClient(dhcp, pck, id);
        } else {
            if (debugger.servDhcp4traf) {
                logger.debug("dhcp relay unknown operation: " + dhcp.bootpOp + " port: " + id.portRem);
            }
        }

        // Always close the incoming connection after processing
        id.setClosing();
        return result;
    }

    private boolean relayClientToServer(packDhcp4 dhcp, packHolder pck, prtGenConn id) {
        long startTime = bits.getTime();

        if (debugger.servDhcp4traf) {
            logger.debug("dhcp relay client->server from " + id.peerAddr + " hops=" + dhcp.bootpHops);
        }

        // Update statistics
        relayStats.packetsClientToServer++;
        relayStats.totalPacketsProcessed++;

        // Track hop count statistics BEFORE incrementing
        relayStats.hopCountTotal += dhcp.bootpHops;

        // Check max hop count to prevent loops
        if (dhcp.bootpHops >= maxHopCount) {
            if (debugger.servDhcp4traf) {
                logger.debug("dhcp4 relay max hop count exceeded (" + dhcp.bootpHops + " >= " + maxHopCount + ")");
            }
            relayStats.maxHopCountExceeded++;
            relayStats.packetsDropped++;
            relayStats.updateErrorProcessingTime(bits.getTime() - startTime);
            return false; // Drop packet - don't process further
        }

        // Increment hop count
        dhcp.bootpHops++;

        // Track multi-hop statistics (after incrementing)
        if (dhcp.bootpHops > 1) {
            relayStats.multiHopPackets++;
        }

        // Set GIADDR to our interface IP if not already set by another relay
        if (dhcp.bootpGiaddr.isEmpty()) {
            // Use the interface address as giaddr
            if (id.iface != null && id.iface.addr != null) {
                dhcp.bootpGiaddr.setAddr(id.iface.addr.toIPv4());
                if (debugger.servDhcp4traf) {
                    logger.debug("dhcp4 relay set giaddr to " + dhcp.bootpGiaddr);
                }
            }
        }

        // Handle existing agent options based on configured mode
        tabGen<packDhcpOption> options = new tabGen<packDhcpOption>();
        if (addAgentOptions) {
            if (!handleExistingAgentOptions(dhcp, pck, options)) {
                // Packet should be discarded (discard mode with existing agent options)
                if (debugger.servDhcp4traf) {
                    logger.debug("dhcp4 relay packet discarded due to existing agent options");
                }
                relayStats.packetsDropped++;
                relayStats.updateProcessingTime(bits.getTime() - startTime);
                return false; // Discard packet
            }
            addAgentInformationOption(options, id);
        }

        // Create new packet with modified DHCP header and options
        packHolder newPck = new packHolder(true, true);
        dhcp.createHeader(newPck, options);

        // Forward to all helper addresses
        int forwardedCount = 0;
        synchronized (helperAddresses) {
            for (int i = 0; i < helperAddresses.size(); i++) {
                addrIP target = helperAddresses.get(i);
                if (forwardToServer(newPck, target, id)) {
                    forwardedCount++;
                    relayStats.packetsForwardedToServers++;
                } else {
                    relayStats.forwardingErrors++;
                }
            }
        }

        if (debugger.servDhcp4traf) {
            logger.debug("dhcp relay forwarded client request to " + forwardedCount + "/" + helperAddresses.size() + " servers");
        }

        // Count as forwarded if at least one server received it
        if (forwardedCount > 0) {
            relayStats.packetsForwarded++;
        } else {
            relayStats.packetsDropped++;
        }

        relayStats.updateProcessingTime(bits.getTime() - startTime);
        return false; // Packet processed successfully
    }

    private boolean relayServerToClient(packDhcp4 dhcp, packHolder pck, prtGenConn id) {
        long startTime = bits.getTime();

        if (debugger.servDhcp4traf) {
            logger.debug("dhcp relay server->client giaddr=" + dhcp.bootpGiaddr);
        }

        // Update statistics
        relayStats.packetsServerToClient++;
        relayStats.totalPacketsProcessed++;

        // Check if this reply has giaddr
        if (dhcp.bootpGiaddr.isEmpty()) {
            if (debugger.servDhcp4traf) {
                logger.debug("dhcp relay reply has empty giaddr, ignoring");
            }
            relayStats.packetsDropped++;
            relayStats.invalidGiaddrErrors++;
            relayStats.updateErrorProcessingTime(bits.getTime() - startTime);
            return false; // Ignore packet
        }

        // Check if giaddr points to a network we can reach
        // In multi-hop relay, we need to forward to the giaddr, not necessarily the client
        // Determine if we should forward to giaddr (relay) or broadcast to client
        boolean forwardToRelay = !isOurGiaddr(dhcp.bootpGiaddr, srvVrf);

        if (forwardToRelay) {
            // Multi-hop: Forward to the relay (giaddr)
            if (debugger.servDhcp4traf) {
                logger.debug("dhcp relay multi-hop: forwarding to relay at " + dhcp.bootpGiaddr);
            }
            relayStats.multiHopPackets++;

            // Create new packet for multi-hop forwarding
            packHolder newPck = new packHolder(true, true);
            dhcp.createHeader(newPck, null);
            if (forwardToRelay(newPck, dhcp.bootpGiaddr)) {
                relayStats.packetsForwarded++;
            } else {
                relayStats.packetsDropped++;
                relayStats.forwardingErrors++;
            }
        } else {
            // Final hop: Forward to client and clear giaddr
            if (debugger.servDhcp4traf) {
                logger.debug("dhcp4 relay final hop: forwarding to client");
            }

            // Find client interface based on giaddr BEFORE clearing it
            ipFwdIface clientInterface = null;
            if (!dhcp.bootpGiaddr.isEmpty()) {
                for (cfgIfc ifc : relayInterfaces) {
                    if (ifc.addr4 != null && ifc.addr4.compareTo(dhcp.bootpGiaddr) == 0) {
                        clientInterface = ifc.fwdIf4;
                        if (debugger.servDhcp4traf) {
                            logger.debug("dhcp4 relay found client interface " + ifc.name + " for giaddr=" + dhcp.bootpGiaddr);
                        }
                        break;
                    }
                }
            }

            // Extract options without agent information
            tabGen<packDhcpOption> clientOptions = null;
            if (addAgentOptions) {
                clientOptions = extractOptionsWithoutAgentInfo(pck);
                stripAgentInformationOptions(dhcp);
            }

            // Clear giaddr before forwarding to client
            dhcp.bootpGiaddr.fillBytes(0);

            // Create new packet with filtered options for client
            packHolder newPck = new packHolder(true, true);
            dhcp.createHeader(newPck, clientOptions);

            // Forward to client
            if (forwardToClient(newPck, dhcp, clientInterface)) {
                relayStats.packetsForwarded++;
            } else {
                relayStats.packetsDropped++;
                relayStats.forwardingErrors++;
            }
        }

        if (debugger.servDhcp4traf) {
            logger.debug("dhcp relay forwarded server reply");
        }

        relayStats.updateProcessingTime(bits.getTime() - startTime);
        return false; // Packet processed successfully
    }

    private boolean forwardToServer(packHolder pck, addrIP serverAddr, prtGenConn incomingConn) {
        try {
            prtUdp udp = srvVrf.getUdp(serverAddr);

            // Use the interface where the relay is configured,
            // not the interface with best route to server
            ipFwdIface fwdIfc = null;
            if (incomingConn != null && incomingConn.iface != null) {
                // Use the same interface where the DHCP relay received the packet
                fwdIfc = incomingConn.iface;
                if (debugger.servDhcp4traf) {
                    logger.debug("dhcp4 relay using relay interface " + fwdIfc + " to forward to server");
                }
            }

            // Use port 67 as source port so server responses come back to port 67
            prtGenConn conn = udp.packetConnect(this, fwdIfc, packDhcp4.portSnum, serverAddr, packDhcp4.portSnum,
                    "dhcp-relay", -1, null, -1, -1);
            if (conn != null) {
                conn.send2net(pck);
                conn.setClosing();
                return true;
            }
            return false;
        } catch (Exception e) {
            if (debugger.servDhcp4traf) {
                logger.debug("dhcp relay forward to server error: " + e.getMessage());
            }
            return false;
        }
    }

    private boolean forwardToClient(packHolder pck, packDhcp4 dhcp, ipFwdIface clientInterface) {
        try {
            // Determine client address - RFC 2131 compliant
            addrIP clientAddr = new addrIP();
            if (!dhcp.bootpCiaddr.isEmpty()) {
                // Client already has an IP address - unicast is OK
                clientAddr.fromIPv4addr(dhcp.bootpCiaddr);
            } else {
                // Client has no IP address yet - MUST use broadcast
                // This includes DHCP Offer messages where bootpYiaddr is set but client can't receive unicast yet
                clientAddr.fromIPv4addr(addrIPv4.getBroadcast());
            }

            prtUdp udp = srvVrf.getUdp(clientAddr);

            // Use provided client interface
            ipFwdIface fwdIfc = clientInterface;
            if (fwdIfc != null && debugger.servDhcp4traf) {
                logger.debug("dhcp4 relay using provided interface to forward to client");
            }
            // Note: clientInterface should always be provided by the caller

            // Use port 67 as source port so client recognizes DHCP messages
            prtGenConn conn = udp.packetConnect(this, fwdIfc, packDhcp4.portSnum, clientAddr, packDhcp4.portCnum,
                    "dhcp-relay", -1, null, -1, -1);
            if (conn != null) {
                conn.send2net(pck);
                conn.setClosing();
                return true;
            }
            return false;
        } catch (Exception e) {
            if (debugger.servDhcp4traf) {
                logger.debug("dhcp relay forward to client error: " + e.getMessage());
            }
            return false;
        }
    }

    /**
     * Handle existing agent options based on relay mode Returns true if packet
     * should be processed, false if should be discarded
     */
    private boolean handleExistingAgentOptions(packDhcp4 dhcp, packHolder originalPacket, tabGen<packDhcpOption> options) {
        if (debugger.servDhcp4traf) {
            logger.debug("dhcp4 relay handling existing agent options in " + agentRelayMode + " mode");
        }

        // Find existing agent information option in the original packet
        packDhcpOption existingOption = findAgentInformationOption(originalPacket);

        if (existingOption == null) {
            // No existing agent options, proceed normally
            return true;
        }

        // Handle based on configured relay mode
        switch (agentRelayMode.toLowerCase()) {
            case "replace":
                if (debugger.servDhcp4traf) {
                    logger.debug("dhcp4 relay replacing existing agent options");
                }
                // Don't copy existing option, we'll add our own
                relayStats.agentOptionsReplaced++;
                relayStats.replaceOperations++;
                return true;

            case "append":
                if (debugger.servDhcp4traf) {
                    logger.debug("dhcp4 relay appending to existing agent options");
                }
                // Copy existing option and we'll add our own sub-options
                options.add(existingOption);
                relayStats.agentOptionsAppended++;
                relayStats.appendOperations++;
                return true;

            case "forward":
                if (debugger.servDhcp4traf) {
                    logger.debug("dhcp4 relay forwarding existing agent options unchanged");
                }
                // Copy existing option as-is, don't add our own
                options.add(existingOption);
                relayStats.agentOptionsForwarded++;
                relayStats.forwardOperations++;
                return true;

            case "discard":
                if (debugger.servDhcp4traf) {
                    logger.debug("dhcp4 relay discarding packet with existing agent options");
                }
                // Drop the packet
                relayStats.agentOptionsDiscarded++;
                relayStats.discardOperations++;
                return false;

            default:
                if (debugger.servDhcp4traf) {
                    logger.debug("dhcp4 relay unknown agent relay mode " + agentRelayMode + ", using append");
                }
                // Copy existing option and we'll add our own sub-options
                options.add(existingOption);
                relayStats.agentOptionsAppended++;
                relayStats.appendOperations++;
                return true;
        }
    }

    /**
     * Add agent information option (Option 82) to DHCP packet
     */
    private void addAgentInformationOption(tabGen<packDhcpOption> options, prtGenConn id) {
        if (debugger.servDhcp4traf) {
            logger.debug("dhcp4 relay adding agent information option");
        }

        // Update statistics
        relayStats.agentOptionsAdded++;

        // Create the relay agent information option (Option 82)
        packDhcpOption agentOption = new packDhcpOption();
        agentOption.number = DHCP_OPTION_RELAY_AGENT_INFO;

        // Build sub-options data
        List<Byte> subOptionData = new ArrayList<>();

        // Sub-option 1: Circuit ID
        if (circuitIdTemplate != null && !circuitIdTemplate.isEmpty()) {
            String circuitId = formatCircuitId(id);
            if (!circuitId.isEmpty()) {
                subOptionData.add((byte) AGENT_CIRCUIT_ID_SUBOPTION);
                subOptionData.add((byte) circuitId.length());
                for (byte b : circuitId.getBytes()) {
                    subOptionData.add(b);
                }
                relayStats.circuitIdAdded++;
                if (debugger.servDhcp4traf) {
                    logger.debug("dhcp4 relay added circuit-id '" + circuitId + "'");
                }
            }
        }

        // Sub-option 2: Remote ID
        if (remoteIdTemplate != null && !remoteIdTemplate.isEmpty()) {
            String remoteId = formatRemoteId(id);
            if (!remoteId.isEmpty()) {
                subOptionData.add((byte) AGENT_REMOTE_ID_SUBOPTION);
                subOptionData.add((byte) remoteId.length());
                for (byte b : remoteId.getBytes()) {
                    subOptionData.add(b);
                }
                relayStats.remoteIdAdded++;
                if (debugger.servDhcp4traf) {
                    logger.debug("dhcp4 relay added remote-id '" + remoteId + "'");
                }
            }
        }

        // Sub-option 5: Link Selection
        if (linkSelectionAddr != null && !linkSelectionAddr.isEmpty()) {
            subOptionData.add((byte) AGENT_LINK_SELECTION_SUBOPTION);
            subOptionData.add((byte) 4); // IPv4 address length
            byte[] addrBytes = linkSelectionAddr.getBytes();
            for (byte b : addrBytes) {
                subOptionData.add(b);
            }
            relayStats.linkSelectionAdded++;
            if (debugger.servDhcp4traf) {
                logger.debug("dhcp4 relay added link-selection '" + linkSelectionAddr + "'");
            }
        }

        // Sub-option 6: Subscriber ID
        if (subscriberId != null && !subscriberId.isEmpty()) {
            subOptionData.add((byte) AGENT_SUBSCRIBER_ID_SUBOPTION);
            subOptionData.add((byte) subscriberId.length());
            for (byte b : subscriberId.getBytes()) {
                subOptionData.add(b);
            }
            relayStats.subscriberIdAdded++;
            if (debugger.servDhcp4traf) {
                logger.debug("dhcp4 relay added subscriber-id '" + subscriberId + "'");
            }
        }

        // Set the option data
        agentOption.buffer = new byte[subOptionData.size()];
        for (int i = 0; i < subOptionData.size(); i++) {
            agentOption.buffer[i] = subOptionData.get(i);
        }

        // Add to options list
        options.add(agentOption);

        if (debugger.servDhcp4traf) {
            logger.debug("dhcp4 relay agent information option added with " + subOptionData.size() + " bytes");
        }
    }

    /**
     * Format circuit ID based on template and interface
     */
    private String formatCircuitId(prtGenConn id) {
        switch (circuitIdTemplate) {
            case "interface-name":
                if (id.iface != null) {
                    // Find corresponding cfgIfc to get the actual interface name
                    for (cfgIfc ifc : relayInterfaces) {
                        if (ifc.fwdIf4 != null && ifc.fwdIf4.equals(id.iface)) {
                            return "interface-" + ifc.name;
                        }
                    }
                }
                return "interface-unknown";

            case "interface-number":
                if (id.iface != null) {
                    return "interface-iface-" + id.iface.ifwNum; // Use interface number
                }
                return "interface-iface-0";

            default:
                // Should never happen due to config validation
                return "interface-invalid";
        }
    }

    /**
     * Format remote ID based on template and system information
     */
    private String formatRemoteId(prtGenConn id) {
        switch (remoteIdTemplate.toLowerCase()) {
            case "hostname":
                return cfgAll.hostName;
            case "ip-address":
                return id.iface != null ? ("" + id.iface.addr) : "0.0.0.0";
            case "mac-address":
                return id.iface != null ? "mac-" + id.iface.ifwNum : "00:00:00:00:00:00";
            default:
                return remoteIdTemplate;
        }
    }

    /**
     * Parse DHCP options from original packet buffer to find agent information
     * options Returns the agent information option if found, null otherwise
     */
    private packDhcpOption findAgentInformationOption(packHolder originalPacket) {
        try {
            // Make a copy to avoid modifying the original
            packHolder pck = originalPacket.copyBytes(true, true);

            // Skip to the options section (after BOOTP header + magic cookie)
            if (pck.dataSize() < packDhcp4.size1 + 4) {
                return null;
            }
            pck.getSkip(packDhcp4.size1);

            // Check magic cookie
            if (pck.msbGetD(0) != packDhcp4.magic) {
                return null;
            }
            pck.getSkip(4);

            // Parse options to find agent information option (82)
            while (pck.dataSize() > 0) {
                int optionType = pck.getByte(0);
                if (optionType == 255) { // End option
                    break;
                }
                if (optionType == 0) { // Pad option
                    pck.getSkip(1);
                    continue;
                }

                if (pck.dataSize() < 2) {
                    break;
                }

                int optionLength = pck.getByte(1);
                if (pck.dataSize() < 2 + optionLength) {
                    break;
                }

                if (optionType == DHCP_OPTION_RELAY_AGENT_INFO) {
                    // Found agent information option
                    packDhcpOption agentOption = new packDhcpOption();
                    agentOption.number = DHCP_OPTION_RELAY_AGENT_INFO;
                    agentOption.buffer = new byte[optionLength];
                    pck.getCopy(agentOption.buffer, 2, 0, optionLength);

                    if (debugger.servDhcp4traf) {
                        logger.debug("dhcp4 relay found existing agent information option with " + optionLength + " bytes");
                    }

                    return agentOption;
                }

                // Skip this option
                pck.getSkip(2 + optionLength);
            }

            return null; // No agent information option found

        } catch (Exception e) {
            if (debugger.servDhcp4traf) {
                logger.debug("dhcp4 relay error parsing options: " + e.getMessage());
            }
            relayStats.optionParsingErrors++;
            return null;
        }
    }

    /**
     * Strip agent information options from DHCP packet Used when forwarding
     * server replies to clients
     */
    private void stripAgentInformationOptions(packDhcp4 dhcp) {
        // Agent options stripping is now handled in extractOptionsWithoutAgentInfo
        // This method is kept for compatibility but the actual stripping
        // happens when we extract options from the original packet
        if (debugger.servDhcp4traf) {
            logger.debug("dhcp4 relay agent options will be stripped during packet reconstruction");
        }
    }

    /**
     * Extract all DHCP options from a packet, excluding agent information
     * options Used when forwarding server responses to clients
     */
    private tabGen<packDhcpOption> extractOptionsWithoutAgentInfo(packHolder originalPacket) {
        tabGen<packDhcpOption> filteredOptions = new tabGen<packDhcpOption>();

        if (originalPacket == null || originalPacket.dataSize() < packDhcp4.size1 + 4) {
            return filteredOptions;
        }

        // Skip to options section (after DHCP header + magic cookie)
        packHolder pck = originalPacket.copyBytes(true, true);
        pck.getSkip(packDhcp4.size1); // Skip DHCP header

        // Check magic cookie
        if (pck.msbGetD(0) != packDhcp4.magic) {
            return filteredOptions;
        }
        pck.getSkip(4); // Skip magic cookie

        // Parse options
        while (pck.dataSize() > 0) {
            int optionType = pck.getByte(0);

            // End of options
            if (optionType == 255) {
                break;
            }

            // Padding
            if (optionType == 0) {
                pck.getSkip(1);
                continue;
            }

            // Check if we have enough data for length field
            if (pck.dataSize() < 2) {
                break;
            }

            int optionLength = pck.getByte(1);

            // Check if we have enough data for the complete option
            if (pck.dataSize() < (2 + optionLength)) {
                break;
            }

            // Skip agent information option (Option 82)
            if (optionType == DHCP_OPTION_RELAY_AGENT_INFO) {
                if (debugger.servDhcp4traf) {
                    logger.debug("dhcp4 relay stripping agent information option (length=" + optionLength + ")");
                }
                pck.getSkip(2 + optionLength);
                continue;
            }

            // Copy other options
            packDhcpOption option = new packDhcpOption();
            option.number = optionType;
            option.buffer = new byte[optionLength];

            // Copy option data
            pck.getSkip(2); // Skip type and length
            for (int i = 0; i < optionLength; i++) {
                option.buffer[i] = (byte) pck.getByte(i);
            }
            pck.getSkip(optionLength);

            filteredOptions.add(option);

            if (debugger.servDhcp4traf) {
                logger.debug("dhcp4 relay preserved option " + optionType + " (length=" + optionLength + ")");
            }
        }

        if (debugger.servDhcp4traf) {
            logger.debug("dhcp4 relay extracted " + filteredOptions.size() + " options (agent info stripped)");
        }

        return filteredOptions;
    }

    /**
     * Check if the given giaddr matches one of our interfaces
     */
    private boolean isOurGiaddr(addrIPv4 giaddr, cfgVrf vrf) {
        if (giaddr == null || vrf == null) {
            return false;
        }

        addrIP targetAddr = new addrIP();
        targetAddr.fromIPv4addr(giaddr);

        // Check if the giaddr matches any of our interface addresses in this VRF.
        if (vrf.fwd4 != null) {
            for (int i = 0; i < vrf.fwd4.ifaces.size(); i++) {
                ipFwdIface iface = vrf.fwd4.ifaces.get(i);
                if (iface == null || iface.addr == null) {
                    continue;
                }
                if (targetAddr.compareTo(iface.addr) == 0) {
                    if (debugger.servDhcp4traf) {
                        logger.debug("dhcp4 relay giaddr " + giaddr + " matches our interface " + iface);
                    }
                    return true; // It's our address
                }
            }
        }

        if (debugger.servDhcp4traf) {
            logger.debug("dhcp4 relay giaddr " + giaddr + " does not match any of our interfaces in vrf " + vrf.name);
        }
        return false; // Not our address
    }

    /**
     * Forward packet to another relay (multi-hop)
     */
    private boolean forwardToRelay(packHolder pck, addrIPv4 relayAddr) {
        try {

            addrIP targetAddr = new addrIP();
            targetAddr.fromIPv4addr(relayAddr);

            prtUdp udp = srvVrf.getUdp(targetAddr);

            ipFwdIface fwdIfc = null;

            // Forward to relay on port 67
            prtGenConn conn = udp.packetConnect(this, fwdIfc, packDhcp4.portSnum, targetAddr, packDhcp4.portSnum,
                    "dhcp-relay-hop", -1, null, -1, -1);
            if (conn != null) {
                conn.send2net(pck);
                conn.setClosing();
                if (debugger.servDhcp4traf) {
                    logger.debug("dhcp4 relay forwarded to relay " + targetAddr);
                }
                return true;
            } else {
                if (debugger.servDhcp4traf) {
                    logger.debug("dhcp4 relay failed to create connection to " + targetAddr);
                }
                return false;
            }
        } catch (Exception e) {
            if (debugger.servDhcp4traf) {
                logger.debug("dhcp relay forward to relay error: " + e.getMessage());
            }
            return false;
        }
    }

    public boolean srvCfgStr(cmds cmd) {
        String a = cmd.word();
        if (a.equals("mode")) {
            a = cmd.word();
            if (a.equals("server")) {
                mode = dhcpMode.server;
            } else if (a.equals("relay")) {
                mode = dhcpMode.relay;
            } else {
                cmd.error("invalid mode");
                return false;
            }
            return false;
        }
        if (a.equals("helper-addresses")) {
            helperAddresses.clear();
            for (;;) {
                a = cmd.word();
                if (a.length() < 1) {
                    break;
                }
                addrIP addr = new addrIP();
                if (addr.fromString(a)) {
                    cmd.error("bad helper address");
                    return false;
                }
                helperAddresses.add(addr);
            }
            mode = dhcpMode.relay;
            return false;
        }
        if (a.equals("add-agent-options")) {
            addAgentOptions = true;
            return false;
        }
        if (a.equals("circuit-id-template")) {
            String template = cmd.word();
            if (template.equals("interface-name")) {
                circuitIdTemplate = "interface-name";
            } else if (template.equals("interface-number")) {
                circuitIdTemplate = "interface-number";
            }
            return false;
        }
        if (a.equals("remote-id-template")) {
            String template = cmd.word();
            if (template.equals("hostname")) {
                remoteIdTemplate = "hostname";
            } else if (template.equals("ip-address")) {
                remoteIdTemplate = "ip-address";
            } else if (template.equals("mac-address")) {
                remoteIdTemplate = "mac-address";
            }
            return false;
        }
        if (a.equals("link-selection-address")) {
            linkSelectionAddr = new addrIPv4();
            if (linkSelectionAddr.fromString(cmd.word())) {
                cmd.error("bad address");
                return true;
            }
            return false;
        }
        if (a.equals("subscriber-id")) {
            subscriberId = cmd.getRemaining();
            return false;
        }
        if (a.equals("max-hop-count")) {
            maxHopCount = bits.str2num(cmd.word());
            return false;
        }
        if (a.equals("agent-relay-mode")) {
            agentRelayMode = cmd.word();
            return false;
        }
        if (a.equals("bind-file")) {
            bindFile = cmd.getRemaining();
            List<String> res = bits.txt2buf(bindFile);
            if (res == null) {
                return false;
            }
            long tim = bits.getTime();
            for (int i = 0; i < res.size(); i++) {
                servDhcp4bind ntry = new servDhcp4bind();
                if (ntry.fromString(new cmds("b", res.get(i)))) {
                    continue;
                }
                ntry = findBinding(ntry.mac, 1, ntry.ip);
                if (ntry == null) {
                    continue;
                }
                ntry.reqd = tim;
            }
            return false;
        }
        if (a.equals("pool")) {
            poolLo = new addrIPv4();
            if (poolLo.fromString(cmd.word())) {
                poolLo = null;
                cmd.error("bad address");
                return false;
            }
            poolHi = new addrIPv4();
            if (poolHi.fromString(cmd.word())) {
                poolHi = null;
                cmd.error("bad address");
                return false;
            }
            if (poolLo.compareTo(poolHi) >= 0) {
                poolLo = null;
                poolHi = null;
                cmd.error("bad order");
                return false;
            }
            poolMsk = new addrIPv4();
            poolNet = new addrIPv4();
            poolMsk.setSub(poolHi, poolLo);
            poolMsk.setNot(poolMsk);
            poolMsk.fromNetmask(poolMsk.toNetmask() - 1);
            poolNet.setAnd(poolLo, poolMsk);
            poolMsk.setNot(poolMsk);
            return false;
        }
        if (a.equals("gateway")) {
            gateway = new addrIPv4();
            if (gateway.fromString(cmd.word())) {
                gateway = null;
                cmd.error("bad address");
                return false;
            }
            return false;
        }
        if (a.equals("netmask")) {
            netmask = new addrIPv4();
            if (netmask.fromString(cmd.word())) {
                netmask = null;
                cmd.error("bad address");
                return false;
            }
            return false;
        }
        if (a.equals("dns-server")) {
            a = cmd.word();
            dns1 = new addrIPv4();
            if (dns1.fromString(a)) {
                dns1 = null;
                cmd.error("bad address");
                return false;
            }
            dns2 = null;
            a = cmd.word();
            if (a.length() < 1) {
                return false;
            }
            dns2 = new addrIPv4();
            if (dns2.fromString(a)) {
                dns2 = null;
                cmd.error("bad address");
                return false;
            }
            return false;
        }
        if (a.equals("boot-server")) {
            bootServ = cmd.word();
            return false;
        }
        if (a.equals("boot-file")) {
            bootFile = cmd.word();
            return false;
        }
        if (a.equals("domain-name")) {
            domNam = cmd.word();
            return false;
        }
        if (a.equals("lease")) {
            lease = bits.str2num(cmd.word());
            return false;
        }
        if (a.equals("renew")) {
            renew = bits.str2num(cmd.word());
            return false;
        }
        if (a.equals("remember")) {
            remember = bits.str2num(cmd.word());
            return false;
        }
        if (a.equals("forbidden")) {
            addrMac mac = new addrMac();
            if (mac.fromString(cmd.word())) {
                return true;
            }
            servDhcp4bind ntry = new servDhcp4bind();
            ntry.mac = mac;
            forbidden.add(ntry);
            return false;
        }
        if (a.equals("static")) {
            addrMac mac = new addrMac();
            addrIPv4 ip = new addrIPv4();
            if (mac.fromString(cmd.word())) {
                return true;
            }
            if (ip.fromString(cmd.word())) {
                return true;
            }
            servDhcp4bind ntry = findBinding(mac, 1, ip);
            if (ntry == null) {
                return true;
            }
            ntry.mac = mac.copyBytes();
            ntry.ip = ip.copyBytes();
            ntry.confed = true;
            return false;
        }
        if (a.equals("option")) {
            packDhcpOption opt = new packDhcpOption();
            opt.fromString(cmd);
            options.put(opt);
            return false;
        }
        if (!a.equals(cmds.negated)) {
            return true;
        }
        a = cmd.word();
        if (a.equals("bind-file")) {
            bindFile = null;
            return false;
        }
        if (a.equals("pool")) {
            poolLo = null;
            poolHi = null;
            poolMsk = null;
            poolNet = null;
            return false;
        }
        if (a.equals("gateway")) {
            gateway = null;
            return false;
        }
        if (a.equals("netmask")) {
            netmask = null;
            return false;
        }
        if (a.equals("dns-server")) {
            dns1 = null;
            dns2 = null;
            return false;
        }
        if (a.equals("boot-server")) {
            bootServ = "";
            return false;
        }
        if (a.equals("boot-file")) {
            bootFile = "";
            return false;
        }
        if (a.equals("domain-name")) {
            domNam = "";
            return false;
        }
        if (a.equals("lease")) {
            lease = renew * 2;
            return false;
        }
        if (a.equals("renew")) {
            renew = lease / 2;
            return false;
        }
        if (a.equals("remember")) {
            remember = 0;
            return false;
        }
        if (a.equals("forbidden")) {
            addrMac mac = new addrMac();
            if (mac.fromString(cmd.word())) {
                return true;
            }
            servDhcp4bind ntry = new servDhcp4bind();
            ntry.mac = mac;
            forbidden.del(ntry);
            return false;
        }
        if (a.equals("static")) {
            addrMac mac = new addrMac();
            if (mac.fromString(cmd.word())) {
                return true;
            }
            findBinding(mac, 2, null);
            return false;
        }
        if (a.equals("helper-addresses")) {
            helperAddresses.clear();
            return false;
        }
        if (a.equals("add-agent-options")) {
            addAgentOptions = false;
            return false;
        }
        if (a.equals("max-hop-count")) {
            maxHopCount = 10;
            return false;
        }
        if (a.equals("agent-relay-mode")) {
            agentRelayMode = "append";
            return false;
        }
        if (a.equals("option")) {
            packDhcpOption opt = new packDhcpOption();
            opt.fromString(cmd);
            options.del(opt);
            return false;
        }
        return true;
    }

    public void srvHelp(userHelp l) {
        // Mode selection command
        l.add(null, false, 1, new int[]{2}, "mode", "operation mode");
        l.add(null, false, 2, new int[]{-1}, "server", "dhcp server mode");
        l.add(null, false, 2, new int[]{-1}, "relay", "dhcp relay mode");

        // Relay mode commands
        l.add(null, false, 1, new int[]{2}, "helper-addresses", "dhcp server addresses to forward to");
        l.add(null, false, 2, new int[]{2, -1}, "<addr>", "server ip address");
        l.add(null, false, 1, new int[]{-1}, "add-agent-options", "add relay agent options");
        l.add(null, false, 1, new int[]{2}, "circuit-id-template", "circuit id template for agent options");
        l.add(null, false, 2, new int[]{-1}, "interface-name", "use interface name as circuit id");
        l.add(null, false, 2, new int[]{-1}, "interface-number", "use interface number as circuit id");
        l.add(null, false, 1, new int[]{2}, "remote-id-template", "remote id template for agent options");
        l.add(null, false, 2, new int[]{-1}, "hostname", "use hostname as remote id");
        l.add(null, false, 2, new int[]{-1}, "ip-address", "use ip address as remote id");
        l.add(null, false, 2, new int[]{-1}, "mac-address", "use mac address as remote id");
        l.add(null, false, 1, new int[]{2}, "link-selection-address", "specify link selection sub-option address");
        l.add(null, false, 2, new int[]{-1}, "<addr>", "ipv4 address");
        l.add(null, false, 1, new int[]{2}, "subscriber-id", "specify subscriber id string");
        l.add(null, false, 2, new int[]{-1}, "<str>", "subscriber id");
        l.add(null, false, 1, new int[]{2}, "max-hop-count", "specify maximum hop count");
        l.add(null, false, 2, new int[]{-1}, "<num>", "hop count (1-255)");
        l.add(null, false, 1, new int[]{2}, "agent-relay-mode", "specify agent relay mode");
        l.add(null, false, 2, new int[]{-1}, "append", "append to existing agent options (default)");
        l.add(null, false, 2, new int[]{-1}, "replace", "replace existing agent options");
        l.add(null, false, 2, new int[]{-1}, "forward", "forward existing agent options unchanged");
        l.add(null, false, 2, new int[]{-1}, "discard", "discard packets with existing agent options");

        // Server mode commands
        l.add(null, false, 1, new int[]{2}, "bind-file", "save bindings");
        l.add(null, false, 2, new int[]{2, -1}, "<str>", "file name");
        l.add(null, false, 1, new int[]{2}, "pool", "address pool to use");
        l.add(null, false, 2, new int[]{3}, "<addr>", "first address to delegate");
        l.add(null, false, 3, new int[]{-1}, "<addr>", "last address to delegate");
        l.add(null, false, 1, new int[]{2}, "gateway", "gateway address to delegate");
        l.add(null, false, 2, new int[]{-1}, "<addr>", "address of gateway");
        l.add(null, false, 1, new int[]{2}, "dns-server", "address(es) of name server(s) to delegate");
        l.add(null, false, 2, new int[]{3, -1}, "<addr>", "dns#1 server address");
        l.add(null, false, 3, new int[]{-1}, "<addr>", "dns#2 server address");
        l.add(null, false, 1, new int[]{2}, "boot-server", "address of tftp server to delegate");
        l.add(null, false, 2, new int[]{-1}, "<str>", "dns server address");
        l.add(null, false, 1, new int[]{2}, "boot-file", "path of tftp file to delegate");
        l.add(null, false, 2, new int[]{-1}, "<str>", "dns server address");
        l.add(null, false, 1, new int[]{2}, "domain-name", "domain name to delegate");
        l.add(null, false, 2, new int[]{-1}, "<str>", "domain name");
        l.add(null, false, 1, new int[]{2}, "lease", "lease time to delegate");
        l.add(null, false, 2, new int[]{-1}, "<num>", "lease time in ms");
        l.add(null, false, 1, new int[]{2}, "renew", "renew time to delegate");
        l.add(null, false, 2, new int[]{-1}, "<num>", "renew time in ms");
        l.add(null, false, 1, new int[]{2}, "remember", "remember time on release");
        l.add(null, false, 2, new int[]{-1}, "<num>", "remember time in ms");
        l.add(null, false, 1, new int[]{2}, "netmask", "network to delegate");
        l.add(null, false, 2, new int[]{-1}, "<mask>", "netmask to delegate");
        l.add(null, false, 1, new int[]{2}, "static", "address pool to use");
        l.add(null, false, 2, new int[]{3}, "<addr>", "mac address of client");
        l.add(null, false, 3, new int[]{-1}, "<addr>", "ip address of client");
        l.add(null, false, 1, new int[]{2}, "forbidden", "address pool to use");
        l.add(null, false, 2, new int[]{-1}, "<addr>", "mac address of client");
        l.add(null, false, 1, new int[]{2}, "option", "specify custom option");
        l.add(null, false, 2, new int[]{3, -1}, "<num>", "type of option");
        l.add(null, false, 3, new int[]{3, -1}, "<num>", "data byte");
    }

    public String srvName() {
        return "dhcp4";
    }

    public int srvPort() {
        return packDhcp4.portSnum;
    }

    public int srvProto() {
        return protoIp4 + protoUdp;
    }

    public boolean srvInit() {
        if (mode == dhcpMode.server) {
            if (srvIface == null) {
                return true;
            }
            restartTimer(false);
            return genStrmStart(this, new pipeLine(32768, true), 0);
        }

        if (srvVrf == null) {
            return true;
        }

        // Validation: Check upstream servers
        if (helperAddresses.isEmpty()) {
            if (debugger.servDhcp4traf) {
                logger.error("dhcp4 relay: no upstream servers configured");
            }
            return true;
        }

        // Relay mode: start datagram service on specific relay interfaces only
        if (relayInterfaces.isEmpty()) {
            if (debugger.servDhcp4traf) {
                logger.debug("DHCP4 Relay: Waiting for interface configuration - no interfaces configured yet");
            }
            return false; // Not ready yet - wait for interfaces to be configured
        }

        if (debugger.servDhcp4traf) {
            logger.info("DHCP4 Relay: Starting service on " + relayInterfaces.size() + " configured interfaces");
        }

        // Start datagram service on each configured relay interface
        boolean allStarted = true;
        for (cfgIfc ifc : relayInterfaces) {
            if (debugger.servDhcp4traf) {
                logger.info("DHCP4 Relay: Starting packetListen on interface " + ifc.name);
            }

            boolean result = srvVrf.udp4.packetListen(this, ifc.fwdIf4, srvPort(), null, 0, srvName(), -1, null, -1, -1);
            if (debugger.servDhcp4traf) {
                logger.info("DHCP4 Relay: packetListen result=" + result + " for interface " + ifc.name + " port " + srvPort());
            }
            if (result) { // packetListen returns true on error
                allStarted = false;
            }
        }

        if (debugger.servDhcp4traf) {
            logger.info("DHCP4 Relay: Service start result: " + (!allStarted ? "FAILED" : "SUCCESS"));
        }
        return !allStarted; // return true on failure
    }

    public boolean srvDeinit() {
        if (mode == dhcpMode.server) {
            restartTimer(true);
            return genericStop(0);
        }

        if (srvVrf == null) {
            return true;
        }

        for (cfgIfc ifc : relayInterfaces) {
            if (debugger.servDhcp4traf) {
                logger.info("DHCP4 Relay: Starting packetStop on interface " + ifc.name);
            }
            srvVrf.udp4.listenStop(ifc.fwdIf4, srvPort(), null, 0);
        }
        return false;
    }

    private void restartTimer(boolean shutdown) {
        try {
            purgeTimer.cancel();
        } catch (Exception e) {
        }
        purgeTimer = null;
        if (shutdown) {
            return;
        }
        purgeTimer = new Timer();
        servDhcp4timer task = new servDhcp4timer(this);
        purgeTimer.schedule(task, 1000, 60000);
    }

    /**
     * find binding
     *
     * @param mac mac address
     * @param create create mode
     * @param hint hint address
     * @return binding
     */
    protected servDhcp4bind findBinding(addrMac mac, int create, addrIPv4 hint) {
        servDhcp4bind ntry = new servDhcp4bind();
        ntry.mac = mac.copyBytes();
        if (forbidden.find(ntry) != null) {
            return null;
        }
        synchronized (bindings) {
            ntry = new servDhcp4bind();
            Collections.sort(bindings);
            ntry.mac = mac.copyBytes();
            int i = Collections.binarySearch(bindings, ntry);
            if (i >= 0) {
                ntry = bindings.get(i);
                if ((create == 3) && (!ntry.confed)) {
                    create--;
                }
                if (create == 2) {
                    ntry.confed = false;
                    if (remember < 1) {
                        bindings.remove(i);
                    } else {
                        ntry.reqd = bits.getTime() - lease + remember;
                    }
                }
                return ntry;
            }
            if (create != 1) {
                return null;
            }
            if (poolMsk == null) {
                return null;
            }
            if (poolLo == null) {
                return null;
            }
            Collections.sort(bindings, new servDhcp4bindIp());
            if ((hint != null) && (gateway != null) && (netmask != null)) {
                addrIPv4 a1 = new addrIPv4();
                addrIPv4 a2 = new addrIPv4();
                a1.setAnd(gateway, netmask);
                a2.setAnd(hint, netmask);
                if (a1.compareTo(a2) == 0) {
                    hint = hint.copyBytes();
                    ntry.ip = hint;
                    i = Collections.binarySearch(bindings, ntry, new servDhcp4bindIp());
                    if (i < 0) {
                        bindings.add(ntry);
                        return ntry;
                    }
                }
            }
            for (int cnt = 0; cnt < 64; cnt++) {
                addrIPv4 ip = new addrIPv4();
                ip.fillRandom();
                ip.setAnd(ip, poolMsk);
                ip.setAdd(ip, poolLo);
                if (ip.compareTo(poolLo) < 0) {
                    continue;
                }
                if (ip.compareTo(poolHi) > 0) {
                    continue;
                }
                ntry.ip = ip;
                i = Collections.binarySearch(bindings, ntry, new servDhcp4bindIp());
                if (i >= 0) {
                    continue;
                }
                bindings.add(ntry);
                return ntry;
            }
            logger.warn("failed to bind new address");
            return null;
        }
    }

    private void updatePack(packDhcp4 req, packDhcp4 rep, servDhcp4bind ntry) {
        rep.bootpOp = packDhcp4.bootpOpReply;
        rep.bootpXid = req.bootpXid;
        rep.bootpSecs = req.bootpSecs;
        rep.bootpBroadcast = req.bootpBroadcast;
        rep.bootpGiaddr = req.bootpGiaddr.copyBytes(); // RFC 2131: Copy giaddr from request
        rep.bootpYiaddr = ntry.ip.copyBytes();
        rep.bootpChaddr = ntry.mac.copyBytes();
        rep.bootpSiaddr = srvIface.addr4.copyBytes();
        rep.bootpSname = "" + bootServ;
        rep.bootpSfile = "" + bootFile;
        rep.bootpChaddr = req.bootpChaddr.copyBytes();
        rep.dhcpServer = srvIface.addr4.copyBytes();
        if (dns1 != null) {
            rep.dhcpDns1srv = dns1.copyBytes();
        }
        if (dns2 != null) {
            rep.dhcpDns2srv = dns2.copyBytes();
        }
        if (gateway != null) {
            rep.dhcpGateway = gateway.copyBytes();
        }
        if (netmask != null) {
            rep.dhcpNetMask = netmask.copyBytes();
        }
        rep.dhcpLeaseTime = lease / 1000;
        rep.dhcpRenewTime = renew / 1000;
        rep.dhcpDomainName = "" + domNam;
    }

    /**
     * process one received packet
     *
     * @param req packet received
     * @return packet to send back, null=nothing
     */
    protected packDhcp4 gotPack(packDhcp4 req) {
        if (req.bootpOp != packDhcp4.bootpOpRequest) {
            return null;
        }
        servDhcp4bind ntry;
        packDhcp4 rep = new packDhcp4();
        switch (req.dhcpOp) {
            case packDhcp4.dhcpOpDiscover:
                ntry = findBinding(req.bootpChaddr, 1, req.bootpCiaddr);
                if (ntry == null) {
                    return null;
                }
                rep.dhcpOp = packDhcp4.dhcpOpOffer;
                updatePack(req, rep, ntry);
                return rep; // Always return response, let worker handle routing
            case packDhcp4.dhcpOpRequest:
                ntry = findBinding(req.bootpChaddr, 1, req.bootpCiaddr);
                if (ntry == null) {
                    return null;
                }
                ntry.reqd = bits.getTime();
                rep.dhcpOp = packDhcp4.dhcpOpAck;
                updatePack(req, rep, ntry);
                return rep; // Always return response, let worker handle routing
            case packDhcp4.dhcpOpRelease:
                ntry = findBinding(req.bootpChaddr, 3, req.bootpCiaddr);
                return null;
        }
        return null;
    }

    /**
     * purge binding table
     */
    protected void doPurging() {
        synchronized (bindings) {
            long cur = bits.getTime();
            for (int i = bindings.size() - 1; i >= 0; i--) {
                servDhcp4bind ntry = bindings.get(i);
                if (ntry == null) {
                    continue;
                }
                if (ntry.confed) {
                    continue;
                }
                if ((cur - ntry.reqd) < lease) {
                    continue;
                }
                if (debugger.servDhcp4traf) {
                    logger.debug("delete " + ntry);
                }
                bindings.remove(i);
            }
        }
        if (bindFile == null) {
            return;
        }
        List<String> txt = bits.txt2buf(bindFile);
        if (txt == null) {
            txt = new ArrayList<String>();
        }
        if (txt.size() == bindings.size()) {
            return;
        }
        txt = new ArrayList<String>();
        synchronized (bindings) {
            for (int i = 0; i < bindings.size(); i++) {
                txt.add("" + bindings.get(i));
            }
        }
        if (bits.buf2txt(true, txt, bindFile)) {
            logger.error("error saving bindings");
        }
    }

    /**
     * get show
     *
     * @return result
     */
    public userFormat getShow() {
        userFormat res = new userFormat("|", "mac|ip|last");
        for (int i = 0; i < bindings.size(); i++) {
            servDhcp4bind ntry = bindings.get(i);
            res.add(ntry.mac + "|" + ntry.ip + "|" + bits.timePast(ntry.reqd));
        }
        return res;
    }

    /**
     * get relay statistics
     *
     * @return result
     */
    public userFormat getRelayStatistics() {
        userFormat res = new userFormat("|", "statistic|value|description");

        // Packet flow statistics
        res.add("Packets Client->Server|" + relayStats.packetsClientToServer + "|DHCP packets relayed from client to server");
        res.add("Packets Server->Client|" + relayStats.packetsServerToClient + "|DHCP packets relayed from server to client");
        res.add("Packets Forwarded|" + relayStats.packetsForwarded + "|Total packets successfully forwarded");
        res.add("Packets Dropped|" + relayStats.packetsDropped + "|Packets dropped due to errors or policy");
        res.add("Packets Invalid|" + relayStats.packetsInvalid + "|Malformed or invalid packets received");

        // Multi-hop statistics
        res.add("Multi-hop Packets|" + relayStats.multiHopPackets + "|Packets with hop count > 1");
        res.add("Max Hop Count Exceeded|" + relayStats.maxHopCountExceeded + "|Packets dropped due to hop limit");
        res.add("Average Hop Count|" + String.format("%.2f", relayStats.getAverageHopCount()) + "|Average hop count of multi-hop packets");

        // Performance metrics
        if (relayStats.packetProcessingCount > 0) {
            res.add("Average Processing Time|" + relayStats.getAverageProcessingTime() + "|Average packet processing time");
            res.add("Max Processing Time|" + relayStats.maxProcessingTime + "|Maximum packet processing time");
            res.add("Min Processing Time|" + (relayStats.minProcessingTime == Long.MAX_VALUE ? 0 : relayStats.minProcessingTime) + "|Minimum packet processing time");
        }
        if (relayStats.errorProcessingCount > 0) {
            res.add("Average Error Processing Time|" + relayStats.getAverageErrorProcessingTime() + "|Average error processing time");
        }

        // Error statistics
        res.add("Invalid GIADDR Errors|" + relayStats.invalidGiaddrErrors + "|Packets with invalid or missing GIADDR");
        res.add("Routing Errors|" + relayStats.routingErrors + "|Packets that could not be routed");
        res.add("Option Parsing Errors|" + relayStats.optionParsingErrors + "|Errors parsing DHCP options");
        res.add("Buffer Overflow Errors|" + relayStats.bufferOverflowErrors + "|Buffer overflow errors");
        res.add("Forwarding Errors|" + relayStats.forwardingErrors + "|Errors forwarding packets to servers");

        return res;
    }

    /**
     * get agent options statistics
     *
     * @return result
     */
    public userFormat getAgentOptionsStatistics() {
        userFormat res = new userFormat("|", "statistic|value|description");

        // Agent options operations
        res.add("Agent Options Added|" + relayStats.agentOptionsAdded + "|Total agent information options added");
        res.add("Agent Options Replaced|" + relayStats.agentOptionsReplaced + "|Existing options replaced");
        res.add("Agent Options Appended|" + relayStats.agentOptionsAppended + "|Options appended to existing");
        res.add("Agent Options Forwarded|" + relayStats.agentOptionsForwarded + "|Existing options forwarded unchanged");
        res.add("Agent Options Discarded|" + relayStats.agentOptionsDiscarded + "|Packets discarded due to existing options");

        // Relay mode operations
        res.add("Replace Operations|" + relayStats.replaceOperations + "|Replace mode operations");
        res.add("Append Operations|" + relayStats.appendOperations + "|Append mode operations");
        res.add("Forward Operations|" + relayStats.forwardOperations + "|Forward mode operations");
        res.add("Discard Operations|" + relayStats.discardOperations + "|Discard mode operations");

        // Sub-option statistics
        res.add("Circuit ID Added|" + relayStats.circuitIdAdded + "|Circuit ID sub-options added");
        res.add("Remote ID Added|" + relayStats.remoteIdAdded + "|Remote ID sub-options added");
        res.add("Link Selection Added|" + relayStats.linkSelectionAdded + "|Link Selection sub-options added");
        res.add("Subscriber ID Added|" + relayStats.subscriberIdAdded + "|Subscriber ID sub-options added");

        return res;
    }

    /**
     * get performance statistics
     *
     * @return result
     */
    public userFormat getPerformanceStatistics() {
        userFormat res = new userFormat("|", "metric|value|unit");

        long totalProcessed = relayStats.getTotalPacketsProcessed();
        if (totalProcessed > 0) {
            res.add("Total Packets Processed|" + totalProcessed + "|packets");
            res.add("Successful Packets|" + relayStats.packetProcessingCount + "|packets");
            res.add("Error Packets|" + relayStats.errorProcessingCount + "|packets");
            res.add("Total Processing Time|" + relayStats.totalProcessingTime + "|");
            res.add("Average Processing Time|" + relayStats.getAverageProcessingTime() + "|");
            res.add("Maximum Processing Time|" + relayStats.maxProcessingTime + "|");
            res.add("Minimum Processing Time|" + (relayStats.minProcessingTime == Long.MAX_VALUE ? 0 : relayStats.minProcessingTime) + "|");

            // Calculate packets per second (rough estimate)
            long uptimeSeconds = (bits.getTime() - statsResetTime) / 1000;
            if (uptimeSeconds > 0) {
                long packetsPerSecond = totalProcessed / uptimeSeconds;
                res.add("Packets Per Second|" + packetsPerSecond + "|pps");
            }
        } else {
            res.add("No Performance Data|0|No packets processed yet");
        }

        return res;
    }

    /**
     * get statistics summary
     *
     * @return result
     */
    public userFormat getStatisticsSummary() {
        userFormat res = new userFormat("|", "category|packets|percentage");

        long totalPackets = relayStats.getTotalPacketsProcessed();

        if (totalPackets > 0) {
            res.add("Client to Server|" + relayStats.packetsClientToServer + "|"
                    + String.format("%.1f%%", (relayStats.packetsClientToServer * 100.0) / totalPackets));
            res.add("Server to Client|" + relayStats.packetsServerToClient + "|"
                    + String.format("%.1f%%", (relayStats.packetsServerToClient * 100.0) / totalPackets));
            res.add("Forwarded|" + relayStats.packetsForwarded + "|"
                    + String.format("%.1f%%", relayStats.getSuccessRate()));
            res.add("Dropped|" + relayStats.packetsDropped + "|"
                    + String.format("%.1f%%", relayStats.getDropRate()));
            res.add("Forwarded to Servers|" + relayStats.packetsForwardedToServers + "|individual server forwards");
        } else {
            res.add("No Traffic|0|No packets processed");
        }

        // Statistics reset time
        res.add("Statistics Reset|" + bits.time2str(cfgAll.timeZoneName, statsResetTime, 3) + "|Last reset time");

        return res;
    }

    /**
     * reset relay statistics
     */
    public void resetRelayStatistics() {
        relayStats.reset();
        statsResetTime = bits.getTime();
        if (debugger.servDhcp4traf) {
            logger.debug("dhcp4 relay statistics reset");
        }
    }

    /**
     * get relay statistics display for show command
     *
     * @return list of statistics lines
     */
    public List<String> getRelayStatisticsDisplay() {
        List<String> result = new ArrayList<String>();

        result.add("DHCP Relay Statistics for " + srvName);
        result.add("=====================================");
        result.add("");

        // Packet flow statistics
        result.add("Packet Flow Statistics:");
        result.add("  Client->Server: " + relayStats.packetsClientToServer);
        result.add("  Server->Client: " + relayStats.packetsServerToClient);
        result.add("  Forwarded: " + relayStats.packetsForwarded);
        result.add("  Dropped: " + relayStats.packetsDropped);
        result.add("  Invalid: " + relayStats.packetsInvalid);
        result.add("");

        // Multi-hop statistics
        result.add("Multi-hop Statistics:");
        result.add("  Multi-hop Packets: " + relayStats.multiHopPackets);
        result.add("  Max Hop Count Exceeded: " + relayStats.maxHopCountExceeded);
        result.add("  Average Hop Count: " + String.format("%.2f", relayStats.getAverageHopCount()));
        result.add("");

        // Agent options statistics
        result.add("Agent Options Statistics:");
        result.add("  Options Added: " + relayStats.agentOptionsAdded);
        result.add("  Options Replaced: " + relayStats.agentOptionsReplaced);
        result.add("  Options Appended: " + relayStats.agentOptionsAppended);
        result.add("  Options Forwarded: " + relayStats.agentOptionsForwarded);
        result.add("  Options Discarded: " + relayStats.agentOptionsDiscarded);
        result.add("");

        // Relay mode operations
        result.add("Relay Mode Operations:");
        result.add("  Replace Operations: " + relayStats.replaceOperations);
        result.add("  Append Operations: " + relayStats.appendOperations);
        result.add("  Forward Operations: " + relayStats.forwardOperations);
        result.add("  Discard Operations: " + relayStats.discardOperations);
        result.add("");

        // Sub-option statistics
        result.add("Sub-option Statistics:");
        result.add("  Circuit ID Added: " + relayStats.circuitIdAdded);
        result.add("  Remote ID Added: " + relayStats.remoteIdAdded);
        result.add("  Link Selection Added: " + relayStats.linkSelectionAdded);
        result.add("  Subscriber ID Added: " + relayStats.subscriberIdAdded);
        result.add("");

        // Performance metrics
        result.add("Performance Metrics:");
        long totalProcessed = relayStats.getTotalPacketsProcessed();
        if (totalProcessed > 0) {
            result.add("  Total Packets Processed: " + totalProcessed);
            result.add("  Successful Packets: " + relayStats.packetProcessingCount);
            result.add("  Error Packets: " + relayStats.errorProcessingCount);
            result.add("  Total Processing Time: " + relayStats.totalProcessingTime);
            result.add("  Average Processing Time: " + relayStats.getAverageProcessingTime());
            result.add("  Maximum Processing Time: " + relayStats.maxProcessingTime);
            result.add("  Minimum Processing Time: " + (relayStats.minProcessingTime == Long.MAX_VALUE ? 0 : relayStats.minProcessingTime));

            if (relayStats.errorProcessingCount > 0) {
                result.add("  Average Error Processing Time: " + relayStats.getAverageErrorProcessingTime());
            }

            // Calculate packets per second (rough estimate)
            long uptimeSeconds = (bits.getTime() - statsResetTime) / 1000;
            if (uptimeSeconds > 0) {
                long packetsPerSecond = totalProcessed / uptimeSeconds;
                result.add("  Packets Per Second: " + packetsPerSecond + " pps");
            }
        } else {
            result.add("  No Performance Data Available");
        }
        result.add("");

        // Error statistics
        result.add("Error Statistics:");
        result.add("  Invalid GIADDR Errors: " + relayStats.invalidGiaddrErrors);
        result.add("  Routing Errors: " + relayStats.routingErrors);
        result.add("  Option Parsing Errors: " + relayStats.optionParsingErrors);
        result.add("  Buffer Overflow Errors: " + relayStats.bufferOverflowErrors);
        result.add("  Forwarding Errors: " + relayStats.forwardingErrors);
        result.add("");

        // Summary with percentages
        long totalPackets = relayStats.getTotalPacketsProcessed();
        result.add("Summary:");
        if (totalPackets > 0) {
            result.add("  Client to Server: " + relayStats.packetsClientToServer + " ("
                    + String.format("%.1f%%", (relayStats.packetsClientToServer * 100.0) / totalPackets) + ")");
            result.add("  Server to Client: " + relayStats.packetsServerToClient + " ("
                    + String.format("%.1f%%", (relayStats.packetsServerToClient * 100.0) / totalPackets) + ")");
            result.add("  Success Rate: " + String.format("%.1f%%", relayStats.getSuccessRate()));
            result.add("  Drop Rate: " + String.format("%.1f%%", relayStats.getDropRate()));
            result.add("  Forwarded to Servers: " + relayStats.packetsForwardedToServers + " (individual server forwards)");
        } else {
            result.add("  No Traffic Statistics Available");
        }
        result.add("");

        // Statistics reset time
        result.add("Statistics Reset: " + bits.time2str(cfgAll.timeZoneName, statsResetTime, 3));

        return result;
    }

    /**
     * Add interface to relay interfaces list
     *
     * @param iface interface
     */
    public synchronized void addRelayInterface(cfgIfc iface) {
        // Check if already in list
        for (cfgIfc existing : relayInterfaces) {
            if (existing.name.equals(iface.name)) {
                if (debugger.servDhcp4traf) {
                    logger.debug("dhcp4 relay interface " + iface.name + " already in relay list");
                }
                return;
            }
        }
        srvDeinit();
        relayInterfaces.add(iface);
        srvInit();
        if (debugger.servDhcp4traf) {
            logger.info("dhcp4 relay added interface " + iface.name + " to relay list. total interfaces: " + relayInterfaces.size());
        }
    }

    /**
     * Remove interface from relay interfaces list
     *
     * @param iface interface
     */
    public synchronized void removeRelayInterface(cfgIfc iface) {
        srvDeinit();
        boolean removed = relayInterfaces.removeIf(existing -> existing.name.equals(iface.name));
        srvInit();
        if (removed) {
            if (debugger.servDhcp4traf) {
                logger.info("dhcp4 relay removed interface " + iface.name + " from relay list. total interfaces: " + relayInterfaces.size());
            }
        } else {
            if (debugger.servDhcp4traf) {
                logger.debug("dhcp4 relay interface " + iface.name + " was not in relay list");
            }
        }
    }

    /**
     * Send DHCP packet with proper unicast/broadcast handling
     *
     * @param pckd DHCP packet to send
     * @param ntry binding entry with client MAC and IP
     * @return true if failed, false if successful
     */
    protected synchronized boolean sendPack(packDhcp4 pckd, servDhcp4bind ntry) {
        addrIP adr = new addrIP();
        int destPort = packDhcp4.portCnum; // Default: client port 68

        // RFC 2131 compliant: Check giaddr first
        if (!pckd.bootpGiaddr.isEmpty()) {
            // Relayed packet: Send response to relay agent (giaddr) on port 67
            adr.fromIPv4addr(pckd.bootpGiaddr);
            destPort = packDhcp4.portSnum; // Relay port 67
            if (debugger.servDhcp4traf) {
                logger.debug("dhcp server: sending to relay agent at " + pckd.bootpGiaddr + " port " + destPort);
            }
        } else {
            // Direct packet: Use client IP or broadcast on port 68
            adr.fromIPv4addr(ntry.ip);
            srvIface.ipIf4.updateL2info(0, ntry.mac, adr);
            if (pckd.bootpBroadcast) {
                adr.fromIPv4addr(addrIPv4.getBroadcast());
                srvIface.ipIf4.updateL2info(0, ntry.mac, adr);
            }
            destPort = packDhcp4.portCnum; // Client port 68
        }

        if (debugger.servDhcp4traf) {
            logger.debug("tx " + adr + ":" + destPort + " " + pckd);
        }

        // Use the same pattern as relay functions for proper connection cleanup
        prtUdp udp = srvVrf.getUdp(adr);

        // Use port 0 (random) as source port for server replies to avoid conflicts
        int sourcePort = (!pckd.bootpGiaddr.isEmpty()) ? 0 : packDhcp4.portSnum;
        prtGenConn conn = udp.packetConnect(this, srvIface.fwdIf4, sourcePort, adr, destPort, "dhcp4-reply", -1, null, -1, -1);
        if (conn == null) {
            return true;
        }

        packHolder pckh = new packHolder(true, true);
        pckd.createHeader(pckh, options);
        conn.send2net(pckh);
        conn.setClosing(); // Properly close connection
        return false;
    }

}

/**
 * DHCP Relay Statistics Class Comprehensive statistics for DHCP relay
 * operations
 */
class servDhcp4RelayStats {

    // Packet flow statistics
    public long packetsClientToServer = 0;
    public long packetsServerToClient = 0;
    public long packetsDropped = 0;
    public long packetsInvalid = 0;
    public long packetsForwarded = 0;
    public long packetsForwardedToServers = 0;

    // Agent Options statistics
    public long agentOptionsAdded = 0;
    public long agentOptionsReplaced = 0;
    public long agentOptionsAppended = 0;
    public long agentOptionsForwarded = 0;
    public long agentOptionsDiscarded = 0;

    // Multi-hop relay statistics
    public long multiHopPackets = 0;
    public long maxHopCountExceeded = 0;
    public long hopCountTotal = 0;
    public long totalPacketsProcessed = 0;

    // Performance metrics (in microseconds)
    public long totalProcessingTime = 0;
    public long packetProcessingCount = 0;
    public long maxProcessingTime = 0;
    public long minProcessingTime = Long.MAX_VALUE;
    public long errorProcessingTime = 0;
    public long errorProcessingCount = 0;

    // Per sub-option statistics
    public long circuitIdAdded = 0;
    public long remoteIdAdded = 0;
    public long linkSelectionAdded = 0;
    public long subscriberIdAdded = 0;

    // Error statistics
    public long invalidGiaddrErrors = 0;
    public long routingErrors = 0;
    public long optionParsingErrors = 0;
    public long bufferOverflowErrors = 0;
    public long forwardingErrors = 0;

    // Relay mode statistics
    public long replaceOperations = 0;
    public long appendOperations = 0;
    public long forwardOperations = 0;
    public long discardOperations = 0;

    /**
     * Reset all statistics to zero
     */
    public void reset() {
        packetsClientToServer = 0;
        packetsServerToClient = 0;
        packetsDropped = 0;
        packetsInvalid = 0;
        packetsForwarded = 0;
        packetsForwardedToServers = 0;

        agentOptionsAdded = 0;
        agentOptionsReplaced = 0;
        agentOptionsAppended = 0;
        agentOptionsForwarded = 0;
        agentOptionsDiscarded = 0;

        multiHopPackets = 0;
        maxHopCountExceeded = 0;
        hopCountTotal = 0;
        totalPacketsProcessed = 0;

        totalProcessingTime = 0;
        packetProcessingCount = 0;
        maxProcessingTime = 0;
        minProcessingTime = Long.MAX_VALUE;
        errorProcessingTime = 0;
        errorProcessingCount = 0;

        circuitIdAdded = 0;
        remoteIdAdded = 0;
        linkSelectionAdded = 0;
        subscriberIdAdded = 0;

        invalidGiaddrErrors = 0;
        routingErrors = 0;
        optionParsingErrors = 0;
        bufferOverflowErrors = 0;
        forwardingErrors = 0;

        replaceOperations = 0;
        appendOperations = 0;
        forwardOperations = 0;
        discardOperations = 0;
    }

    /**
     * Update processing time statistics for successful packets
     */
    public void updateProcessingTime(long processingTime) {
        totalProcessingTime += processingTime;
        packetProcessingCount++;
        if (processingTime > maxProcessingTime) {
            maxProcessingTime = processingTime;
        }
        if (processingTime < minProcessingTime) {
            minProcessingTime = processingTime;
        }
    }

    /**
     * Update processing time statistics for error packets
     */
    public void updateErrorProcessingTime(long processingTime) {
        errorProcessingTime += processingTime;
        errorProcessingCount++;
        if (processingTime > maxProcessingTime) {
            maxProcessingTime = processingTime;
        }
        if (processingTime < minProcessingTime) {
            minProcessingTime = processingTime;
        }
    }

    /**
     * Get average processing time in microseconds (successful packets only)
     */
    public long getAverageProcessingTime() {
        if (packetProcessingCount == 0) {
            return 0;
        }
        return totalProcessingTime / packetProcessingCount;
    }

    /**
     * Get average error processing time in microseconds
     */
    public long getAverageErrorProcessingTime() {
        if (errorProcessingCount == 0) {
            return 0;
        }
        return errorProcessingTime / errorProcessingCount;
    }

    /**
     * Get average hop count (FIXED: now correctly calculated)
     */
    public double getAverageHopCount() {
        if (totalPacketsProcessed == 0) {
            return 0.0;
        }
        return (double) hopCountTotal / totalPacketsProcessed;
    }

    /**
     * Get total packets processed
     */
    public long getTotalPacketsProcessed() {
        return packetsClientToServer + packetsServerToClient;
    }

    /**
     * Get success rate percentage
     */
    public double getSuccessRate() {
        long totalPackets = getTotalPacketsProcessed();
        if (totalPackets == 0) {
            return 0.0;
        }
        return (double) packetsForwarded * 100.0 / totalPackets;
    }

    /**
     * Get drop rate percentage
     */
    public double getDropRate() {
        long totalPackets = getTotalPacketsProcessed();
        if (totalPackets == 0) {
            return 0.0;
        }
        return (double) packetsDropped * 100.0 / totalPackets;
    }
}

class servDhcp4bindIp implements Comparator<servDhcp4bind> {

    public int compare(servDhcp4bind o1, servDhcp4bind o2) {
        return o1.ip.compareTo(o2.ip);
    }

}

class servDhcp4bind implements Comparable<servDhcp4bind> {

    public boolean confed = false;

    public addrIPv4 ip;

    public addrMac mac;

    public long reqd;

    public servDhcp4bind() {
        reqd = bits.getTime();
    }

    public String toString() {
        return ip + " " + mac;
    }

    public boolean fromString(cmds cmd) {
        ip = new addrIPv4();
        mac = new addrMac();
        if (ip.fromString(cmd.word())) {
            return true;
        }
        if (mac.fromString(cmd.word())) {
            return true;
        }
        return false;
    }

    public int compareTo(servDhcp4bind o) {
        return mac.compareTo(o.mac);
    }

}

class servDhcp4timer extends TimerTask {

    private servDhcp4 parent;

    public servDhcp4timer(servDhcp4 prnt) {
        parent = prnt;
    }

    public void run() {
        if (debugger.servDhcp4traf) {
            logger.debug("purging");
        }
        try {
            parent.doPurging();
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

}

class servDhcp4worker implements Runnable {

    private servDhcp4 parent;

    private pipeSide pipe;

    private prtGenConn conn;

    public servDhcp4worker(servDhcp4 prnt, pipeSide pip, prtGenConn id) {
        parent = prnt;
        pipe = pip;
        pipe.setTime(10000);
        conn = id;
        new Thread(this).start();
    }

    private void doer() {
        packHolder pck = pipe.readPacket(true);
        if (pck == null) {
            logger.info("got no packet");
            return;
        }
        packDhcp4 pckd = new packDhcp4();
        if (pckd.parseHeader(pck, false)) {
            logger.info("got bad packet");
            return;
        }
        if (debugger.servDhcp4traf) {
            logger.debug("rx " + pckd);
        }
        pckd = parent.gotPack(pckd);
        if (pckd == null) {
            return;
        }
        if (debugger.servDhcp4traf) {
            logger.debug("tx " + pckd);
        }

        // Find the binding for this client and use the existing sendPack method
        servDhcp4bind ntry = parent.findBinding(pckd.bootpChaddr, 0, pckd.bootpCiaddr);
        if (ntry != null) {
            parent.sendPack(pckd, ntry);
        }
    }

    public void run() {
        try {
            doer();
        } catch (Exception e) {
            logger.traceback(e);
        }
        conn.setClosing();
        pipe.setClose();
    }

}
