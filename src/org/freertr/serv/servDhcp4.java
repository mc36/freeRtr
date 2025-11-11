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
import org.freertr.pack.packDhcp4;
import org.freertr.pack.packDhcpOption;
import org.freertr.pack.packHolder;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGenConn;
import org.freertr.prt.prtServP;
import org.freertr.prt.prtServS;
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
     * circuit ID template for agent options, 1=name, 2=ifcid
     */
    private int circuitIdTemplate = 1;

    /**
     * remote ID template for agent options, 1=hostname, 2=ipaddr, 3=macaddr
     */
    private int remoteIdTemplate = 1;

    /**
     * link selection address for agent options
     */
    private addrIPv4 linkSelectionAddr = null;

    /**
     * subscriber ID for agent options
     */
    private String subscriberId = null;

    /**
     * maximum hop count
     */
    private int maxHopCount = 10;

    /**
     * agent relay mode: 0=nothing, 1=replace, 2=append, 3=forward, 4=discard
     */
    private int agentRelayMode = 0;

    /**
     * list of interfaces for relay (multiple interface support)
     */
    private List<cfgIfc> relayInterfaces = new ArrayList<cfgIfc>();

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
        new userFilter("server dhcp4 .*", cmds.tabulator + "circuit-id-template interface-name", null),
        new userFilter("server dhcp4 .*", cmds.tabulator + "remote-id-template hostname", null),
        new userFilter("server dhcp4 .*", cmds.tabulator + cmds.negated + cmds.tabulator + "link-selection-address", null),
        new userFilter("server dhcp4 .*", cmds.tabulator + cmds.negated + cmds.tabulator + "subscriber-id", null),
        new userFilter("server dhcp4 .*", cmds.tabulator + "max-hop-count 10", null),
        new userFilter("server dhcp4 .*", cmds.tabulator + "agent-relay-mode nothing", null)
    };

    public userFilter[] srvDefFlt() {
        return defaultF;
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        if (mode == dhcpMode.server) {
            pipe.setTime(10000);
            new servDhcp4worker(this, pipe, id);
            return false;
        }
        // Relay mode: configure pipe properly
        id.timeout = 1000;
        if (pipe != null) {
            pipe.setTime(1000);
            pipe.setClose();
        }
        return false;
    }

    public void srvShRun(String beg, List<String> l, int filter) {
        l.add(beg + "mode " + mode);

        if (mode == dhcpMode.server) {
            if ((poolLo == null) || (poolHi == null)) {
                l.add(beg + cmds.negated + " pool");
            } else {
                l.add(beg + "pool " + poolLo + " " + poolHi);
            }
            if (gateway == null) {
                l.add(beg + cmds.negated + " gateway");
            } else {
                l.add(beg + "gateway " + gateway);
            }
            if (netmask == null) {
                l.add(beg + cmds.negated + " netmask");
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
                l.add(beg + cmds.negated + " dns-server");
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
                    if (ntry == null) {
                        continue;
                    }
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
            String a = "";
            for (int i = 0; i < helperAddresses.size(); i++) {
                a += " " + helperAddresses.get(i);
            }
            if (a.length() > 0) {
                l.add(beg + "helper-addresses" + a);
            }
            switch (circuitIdTemplate) {
                case 1:
                    a = "interface-name";
                    break;
                case 2:
                    a = "interface-number";
                    break;
                default:
                    a = "unknown=" + circuitIdTemplate;
                    break;
            }
            l.add(beg + "circuit-id-template " + a);
            switch (remoteIdTemplate) {
                case 1:
                    a = "hostname";
                    break;
                case 2:
                    a = "ip-address";
                    break;
                case 3:
                    a = "mac-address";
                    break;
                default:
                    a = "unknown=" + remoteIdTemplate;
                    break;
            }
            l.add(beg + "remote-id-template " + a);
            if (linkSelectionAddr != null) {
                l.add(beg + "link-selection-address " + linkSelectionAddr);
            }
            cmds.cfgLine(l, subscriberId == null, beg, "subscriber-id", subscriberId);
            l.add(beg + "max-hop-count " + maxHopCount);
            switch (agentRelayMode) {
                case 0:
                    a = "nothing";
                    break;
                case 1:
                    a = "replace";
                    break;
                case 2:
                    a = "append";
                    break;
                case 3:
                    a = "forward";
                    break;
                case 4:
                    a = "discard";
                    break;
                default:
                    a = "unknown=" + agentRelayMode;
                    break;
            }
            l.add(beg + "agent-relay-mode " + a);
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
        long startTime = bits.getTime();
        id.setClosing();
        if (mode != dhcpMode.relay) {
            return false;
        }

        try {
            packDhcp4 dhcp = new packDhcp4();
            if (dhcp.parseHeader(pck, false)) {
                if (debugger.servDhcp4traf) {
                    logger.debug("dhcp relay bad packet");
                }
                return false;
            }

            // Check hop count
            if (dhcp.bootpHops >= maxHopCount) {
                if (debugger.servDhcp4traf) {
                    logger.debug("dhcp relay max hops exceeded: " + dhcp.bootpHops + " >= " + maxHopCount);
                }
                return false;
            }

            // Handle DHCP packet based on operation type
            // Multi-hop relays: Requests can come from both client port (68) and server port (67)
            switch (dhcp.bootpOp) {
                case packDhcp4.bootpOpRequest:
                    if (debugger.servDhcp4traf) {
                        logger.debug("dhcp relay client->server from " + id.peerAddr + " hops=" + dhcp.bootpHops);
                    }

                    // Check max hop count to prevent loops
                    if (dhcp.bootpHops >= maxHopCount) {
                        if (debugger.servDhcp4traf) {
                            logger.debug("dhcp4 relay max hop count exceeded (" + dhcp.bootpHops + " >= " + maxHopCount + ")");
                        }
                        return false; // Drop packet - don't process further
                    }

                    // Increment hop count
                    dhcp.bootpHops++;

                    // Set GIADDR to our interface IP if not already set by another relay
                    if (dhcp.bootpGiaddr.isEmpty()) {
                        dhcp.bootpGiaddr.setAddr(id.iface.addr.toIPv4());
                        if (debugger.servDhcp4traf) {
                            logger.debug("dhcp4 relay set giaddr to " + dhcp.bootpGiaddr);
                        }
                    }

                    boolean need2add = false;

                    // Handle based on configured relay mode
                    switch (agentRelayMode) {
                        case 0:
                            if (debugger.servDhcp4traf) {
                                logger.debug("dhcp4 relay nothing agent relay mode options");
                            }
                            // Copy existing option and we'll add our own sub-options
                            break;

                        case 1:
                            if (debugger.servDhcp4traf) {
                                logger.debug("dhcp4 relay replacing existing agent options");
                            }
                            // Don't copy existing option, we'll add our own
                            dhcp.dhcpAgentInfo = new byte[0];
                            need2add = true;
                            break;

                        case 2:
                            if (debugger.servDhcp4traf) {
                                logger.debug("dhcp4 relay appending to existing agent options");
                            }
                            // Copy existing option and we'll add our own sub-options
                            need2add = true;
                            break;

                        case 3:
                            if (debugger.servDhcp4traf) {
                                logger.debug("dhcp4 relay forwarding existing agent options unchanged");
                            }
                            // Copy existing option as-is, don't add our own
                            break;

                        case 4:
                            if (dhcp.dhcpAgentInfo == null) {
                                // No existing agent options, proceed normally
                                break;
                            }
                            if (debugger.servDhcp4traf) {
                                logger.debug("dhcp4 relay discarding packet with existing agent options");
                            }
                            // Drop the packet
                            return false; // Discard packet

                        default:
                            if (debugger.servDhcp4traf) {
                                logger.debug("dhcp4 relay unknown agent relay mode " + agentRelayMode + ", using append");
                            }
                            // Copy existing option and we'll add our own sub-options
                            break;
                    }
                    if (need2add) {
                        if (debugger.servDhcp4traf) {
                            logger.debug("dhcp4 relay adding agent information option");
                        }

                        // Build sub-options data
                        List<Byte> subOptionData = new ArrayList<Byte>();
                        if (dhcp.dhcpAgentInfo != null) {
                            for (int i = 0; i < dhcp.dhcpAgentInfo.length; i++) {
                                subOptionData.add(dhcp.dhcpAgentInfo[i]);
                            }
                        }

                        byte[] buf;
                        switch (circuitIdTemplate) {
                            case 1:
                                buf = ("interface-" + id.iface).getBytes();
                                break;
                            case 2:
                                buf = ("interface-iface-" + id.iface.ifwNum).getBytes();
                                break;
                            default:
                                buf = ("" + circuitIdTemplate).getBytes();
                                break;
                        }
                        subOptionData.add((byte) 1);
                        subOptionData.add((byte) buf.length);
                        for (int i = 0; i < buf.length; i++) {
                            subOptionData.add(buf[i]);
                        }

                        // Sub-option 2: Remote ID
                        switch (remoteIdTemplate) {
                            case 1:
                                buf = cfgAll.hostName.getBytes();
                                break;
                            case 2:
                                buf = ("" + id.iface.addr).getBytes();
                                break;
                            case 3:
                                buf = ("mac-" + id.iface.ifwNum).getBytes();
                                break;
                            default:
                                buf = ("" + remoteIdTemplate).getBytes();
                                break;
                        }
                        subOptionData.add((byte) 2);
                        subOptionData.add((byte) buf.length);
                        for (int i = 0; i < buf.length; i++) {
                            subOptionData.add(buf[i]);
                        }

                        // Sub-option 5: Link Selection
                        if (linkSelectionAddr != null) {
                            subOptionData.add((byte) 5);
                            buf = linkSelectionAddr.getBytes();
                            subOptionData.add((byte) buf.length); // IPv4 address length
                            for (int i = 0; i < buf.length; i++) {
                                subOptionData.add(buf[i]);
                            }
                        }

                        // Sub-option 6: Subscriber ID
                        if (subscriberId != null) {
                            subOptionData.add((byte) 6);
                            buf = subscriberId.getBytes();
                            subOptionData.add((byte) buf.length);
                            for (int i = 0; i < buf.length; i++) {
                                subOptionData.add(buf[i]);
                            }
                        }

                        dhcp.dhcpAgentInfo = new byte[subOptionData.size()];
                        for (int i = 0; i < dhcp.dhcpAgentInfo.length; i++) {
                            dhcp.dhcpAgentInfo[i] = subOptionData.get(i);
                        }
                        if (dhcp.dhcpAgentInfo.length < 1) {
                            dhcp.dhcpAgentInfo = null;
                        }

                        if (debugger.servDhcp4traf) {
                            logger.debug("dhcp4 relay agent information option added with " + subOptionData.size() + " bytes");
                        }
                    }

                    // Forward to all helper addresses
                    synchronized (helperAddresses) {
                        for (int i = 0; i < helperAddresses.size(); i++) {
                            addrIP target = helperAddresses.get(i);
                            pck.clear();
                            dhcp.createHeader(pck, null);
                            pck.merge2beg();
                            prtGenConn conn = srvVrf.getUdp(target).packetConnect(this, id.iface, packDhcp4.portSnum, target, packDhcp4.portSnum, "dhcp-relay", -1, null, -1, -1);
                            if (conn == null) {
                                continue;
                            }
                            conn.send2net(pck.copyBytes(true, true));
                            conn.setClosing();
                        }
                    }
                    return false;

                case packDhcp4.bootpOpReply:
                    // This is a DHCP reply - relay to client
                    // Always comes from server port (67)
                    if (debugger.servDhcp4traf) {
                        logger.debug("dhcp4 relay processing reply from port " + id.portRem);
                    }

                    if (debugger.servDhcp4traf) {
                        logger.debug("dhcp relay server->client giaddr=" + dhcp.bootpGiaddr);
                    }

                    // Check if this reply has giaddr
                    if (dhcp.bootpGiaddr.isEmpty()) {
                        if (debugger.servDhcp4traf) {
                            logger.debug("dhcp relay reply has empty giaddr, ignoring");
                        }
                        return false; // Ignore packet
                    }

                    // Check if giaddr points to a network we can reach
                    // In multi-hop relay, we need to forward to the giaddr, not necessarily the client
                    // Determine if we should forward to giaddr (relay) or broadcast to client
                    addrIP targetAddr = new addrIP();
                    targetAddr.fromIPv4addr(dhcp.bootpGiaddr);
                    if (id.iface.addr.compareTo(targetAddr) != 0) {
                        // Multi-hop: Forward to the relay (giaddr)
                        if (debugger.servDhcp4traf) {
                            logger.debug("dhcp relay multi-hop: forwarding to relay at " + dhcp.bootpGiaddr);
                        }

                        // Forward to relay on port 67
                        prtGenConn conn = srvVrf.getUdp(targetAddr).packetConnect(this, id.iface, packDhcp4.portSnum, targetAddr, packDhcp4.portSnum, "dhcp-relay-hop", -1, null, -1, -1);
                        if (conn == null) {
                            return false;
                        }
                        conn.send2net(pck);
                        conn.setClosing();

                    } else {

                        if (debugger.servDhcp4traf) {
                            logger.debug("dhcp4 relay final hop: forwarding to client");
                        }

                        // Extract options without agent information
                        if (agentRelayMode != 0) {
                            dhcp.dhcpAgentInfo = null;
                        }

                        // Clear giaddr before forwarding to client
                        dhcp.bootpGiaddr.fillBytes(0);

                        pck.clear();
                        dhcp.createHeader(pck, null);
                        pck.merge2beg();

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

                        // Use provided client interface
                        // Use port 67 as source port so client recognizes DHCP messages
                        prtGenConn conn = srvVrf.getUdp(clientAddr).packetConnect(this, id.iface, packDhcp4.portSnum, clientAddr, packDhcp4.portCnum, "dhcp-relay", -1, null, -1, -1);
                        if (conn == null) {
                            return false;
                        }
                        conn.send2net(pck);
                        conn.setClosing();
                    }
                    return false;
                default:
                    if (debugger.servDhcp4traf) {
                        logger.debug("dhcp relay unknown operation: " + dhcp.bootpOp + " port: " + id.portRem);
                    }
                    return false;
            }
        } catch (Exception e) {
            logger.traceback(e);
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
        if (a.equals("circuit-id-template")) {
            a = cmd.word();
            if (a.equals("interface-name")) {
                circuitIdTemplate = 1;
            } else if (a.equals("interface-number")) {
                circuitIdTemplate = 2;
            }
            return false;
        }
        if (a.equals("remote-id-template")) {
            a = cmd.word();
            if (a.equals("hostname")) {
                remoteIdTemplate = 1;
            } else if (a.equals("ip-address")) {
                remoteIdTemplate = 2;
            } else if (a.equals("mac-address")) {
                remoteIdTemplate = 3;
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
            a = cmd.word();
            if (a.equals("nothing")) {
                agentRelayMode = 0;
            }
            if (a.equals("replace")) {
                agentRelayMode = 1;
            }
            if (a.equals("append")) {
                agentRelayMode = 2;
            }
            if (a.equals("forward")) {
                agentRelayMode = 3;
            }
            if (a.equals("discard")) {
                agentRelayMode = 4;
            }
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
        if (a.equals("circuit-id-template")) {
            circuitIdTemplate = 1;
            return false;
        }
        if (a.equals("remote-id-template")) {
            remoteIdTemplate = 1;
            return false;
        }
        if (a.equals("link-selection-address")) {
            linkSelectionAddr = null;
            return false;
        }
        if (a.equals("subscriber-id")) {
            subscriberId = null;
            return false;
        }
        if (a.equals("max-hop-count")) {
            maxHopCount = 10;
            return false;
        }
        if (a.equals("agent-relay-mode")) {
            agentRelayMode = 0;
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
        l.add(null, false, 2, new int[]{-1}, "nothing", "do nothing with the option");
        l.add(null, false, 2, new int[]{-1}, "append", "append to existing agent options");
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
        for (int i = 0; i < relayInterfaces.size(); i++) {
            cfgIfc ifc = relayInterfaces.get(i);
            if (ifc == null) {
                continue;
            }
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

        for (int i = 0; i < relayInterfaces.size(); i++) {
            cfgIfc ifc = relayInterfaces.get(i);
            if (ifc == null) {
                continue;
            }
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
            case packDhcp4.dhcpOpAbsent:
                ntry = findBinding(req.bootpChaddr, 1, req.bootpCiaddr);
                if (ntry == null) {
                    return null;
                }
                rep.dhcpOp = packDhcp4.dhcpOpAbsent;
                updatePack(req, rep, ntry);
                return rep; // Always return response, let worker handle routing
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
                servDhcp4bind ntry = bindings.get(i);
                if (ntry == null) {
                    continue;
                }
                txt.add("" + ntry);
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
            if (ntry == null) {
                continue;
            }
            res.add(ntry.mac + "|" + ntry.ip + "|" + bits.timePast(ntry.reqd));
        }
        return res;
    }

    /**
     * Add interface to relay interfaces list
     *
     * @param iface interface
     */
    public synchronized void addRelayInterface(cfgIfc iface) {
        // Check if already in list
        if (relayInterfaces.indexOf(iface) >= 0) {
            if (debugger.servDhcp4traf) {
                logger.debug("dhcp4 relay interface " + iface.name + " already in relay list");
            }
            return;
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
        if (relayInterfaces.indexOf(iface) < 0) {
            if (debugger.servDhcp6traf) {
                logger.debug("dhcp6 relay: interface " + iface.name + " was not in relay list");
            }
            return;
        }
        srvDeinit();
        relayInterfaces.remove(iface);
        srvInit();
        if (debugger.servDhcp6traf) {
            logger.debug("dhcp6 relay: removed interface " + iface.name + " from relay list. total interfaces: " + relayInterfaces.size());
        }
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
        if (ntry == null) {
            return;
        }

        addrIP adr = new addrIP();
        int destPort;

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
            parent.srvIface.ipIf4.updateL2info(0, ntry.mac, adr);
            if (pckd.bootpBroadcast) {
                adr.fromIPv4addr(addrIPv4.getBroadcast());
                parent.srvIface.ipIf4.updateL2info(0, ntry.mac, adr);
            }
            destPort = packDhcp4.portCnum; // Client port 68
        }

        if (debugger.servDhcp4traf) {
            logger.debug("tx " + adr + ":" + destPort + " " + pckd);
        }

        // Use port 0 (random) as source port for server replies to avoid conflicts
        int sourcePort = (!pckd.bootpGiaddr.isEmpty()) ? 0 : packDhcp4.portSnum;
        prtGenConn con = parent.srvVrf.getUdp(adr).packetConnect(parent, parent.srvIface.fwdIf4, sourcePort, adr, destPort, "dhcp4-reply", -1, null, -1, -1);
        if (con == null) {
            return;
        }

        packHolder pckh = new packHolder(true, true);
        pckd.createHeader(pckh, parent.options);
        con.send2net(pckh);
        con.setClosing(); // Properly close connection

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
