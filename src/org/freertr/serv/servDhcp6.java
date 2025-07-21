package org.freertr.serv;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Timer;
import java.util.TimerTask;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv6;
import org.freertr.addr.addrMac;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgIfc;
import org.freertr.pack.packDhcp6;
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
 * dynamic host config protocol (rfc3315) server
 *
 * @author matecsaba
 */
public class servDhcp6 extends servGeneric implements prtServS, prtServP {

    /**
     * create instance
     */
    public servDhcp6() {
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
     * dns1
     */
    public addrIPv6 dns1;

    /**
     * dns2
     */
    public addrIPv6 dns2;

    /**
     * gateway
     */
    public addrIPv6 gateway;

    /**
     * network mask
     */
    public addrIPv6 netmask;

    /**
     * boot url
     */
    public String bootUrl = "";

    /**
     * domain name
     */
    public String domNam = "";

    /**
     * server preference
     */
    public int prefer = 0;

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
     * enable dynamic address allocation
     */
    public boolean dynamicAddress = false;

    /**
     * options to add
     */
    public tabGen<packDhcpOption> options = new tabGen<packDhcpOption>();

    private List<servDhcp6bind> bindings = new ArrayList<servDhcp6bind>();

    private tabGen<servDhcp6bind> forbidden = new tabGen<servDhcp6bind>();

    private String bindFile;

    private Timer purgeTimer;

    // Relay mode fields
    /**
     * upstream servers - DHCP6 servers to forward to
     */
    private List<addrIP> helperAddresses = new ArrayList<addrIP>();

    /**
     * use interface-id option
     */
    private boolean useInterfaceId = false;

    /**
     * subscriber ID for relay
     */
    private String subscriberId = "";

    /**
     * maximum hop count
     */
    private int maxHopCount = 10;

    /**
     * List of relay interfaces for multi-interface support
     */
    private List<cfgIfc> relayInterfaces = new ArrayList<cfgIfc>();

    /**
     * DHCP6 Relay Statistics
     */
    private servDhcp6RelayStats relayStats = new servDhcp6RelayStats();

    /**
     * Statistics reset timestamp
     */
    private long statsResetTime = bits.getTime();

    // DHCP6 Relay Constants
    private final static int D6O_INTERFACE_ID = 18;
    private final static int D6O_RELAY_MSG = 9;
    private final static int D6O_SUBSCRIBER_ID = 38;

    /**
     * defaults text
     */
    public final static userFilter[] defaultF = {
        new userFilter("server dhcp6 .*", cmds.tabulator + "port " + packDhcp6.portSnum, null),
        new userFilter("server dhcp6 .*", cmds.tabulator + "protocol " + proto2string(protoIp6 + protoUdp), null),
        new userFilter("server dhcp6 .*", cmds.tabulator + "mode server", null),
        new userFilter("server dhcp6 .*", cmds.tabulator + "boot-url ", null),
        new userFilter("server dhcp6 .*", cmds.tabulator + "preference 0", null),
        new userFilter("server dhcp6 .*", cmds.tabulator + "lease 43200000", null),
        new userFilter("server dhcp6 .*", cmds.tabulator + "renew 21600000", null),
        new userFilter("server dhcp6 .*", cmds.tabulator + "remember 0", null),
        new userFilter("server dhcp6 .*", cmds.tabulator + cmds.negated + cmds.tabulator + "bind-file", null),
        new userFilter("server dhcp6 .*", cmds.tabulator + cmds.negated + cmds.tabulator + "use-interface-id", null),
        new userFilter("server dhcp6 .*", cmds.tabulator + cmds.negated + cmds.tabulator + "subscriber-id", null),
        new userFilter("server dhcp6 .*", cmds.tabulator + "max-hop-count 10", null)
    };

    public userFilter[] srvDefFlt() {
        return defaultF;
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        if (mode == dhcpMode.relay) {
            // Relay mode: configure pipe properly
            if (pipe != null) {
                pipe.setTime(1000);
                pipe.setClose();
            }
            return false;
        }
        // Server mode: use worker thread
        pipe.setTime(10000);
        new servDhcp6worker(this, pipe, id);
        return false;
    }

    public void srvShRun(String beg, List<String> l, int filter) {
        l.add(beg + "mode " + mode);

        if (mode == dhcpMode.server) {
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
            l.add(beg + "boot-url " + bootUrl);
            l.add(beg + "domain-name " + domNam);
            l.add(beg + "lease " + lease);
            l.add(beg + "renew " + renew);
            l.add(beg + "remember " + remember);
            l.add(beg + "preference " + prefer);
            if (dynamicAddress) {
                l.add(beg + "dynamic-address");
            } else {
                l.add(beg + cmds.negated + " dynamic-address");
            }
            for (int i = 0; i < forbidden.size(); i++) {
                servDhcp6bind ntry = forbidden.get(i);
                l.add(beg + "forbidden " + ntry.mac);
            }
            synchronized (bindings) {
                for (int i = 0; i < bindings.size(); i++) {
                    servDhcp6bind ntry = bindings.get(i);
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
            if (useInterfaceId) {
                l.add(beg + "use-interface-id");
            }
            cmds.cfgLine(l, subscriberId.isEmpty(), beg, "subscriber-id", subscriberId);
            l.add(beg + "max-hop-count " + maxHopCount);
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
        if (a.equals("dynamic-address")) {
            dynamicAddress = true;
            return false;
        }
        if (a.equals("use-interface-id")) {
            useInterfaceId = true;
            return false;
        }
        if (a.equals("subscriber-id")) {
            subscriberId = cmd.word();
            return false;
        }
        if (a.equals("max-hop-count")) {
            maxHopCount = bits.str2num(cmd.word());
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
        if (a.equals("bind-file")) {
            bindFile = cmd.getRemaining();
            List<String> res = bits.txt2buf(bindFile);
            if (res == null) {
                return false;
            }
            long tim = bits.getTime();
            for (int i = 0; i < res.size(); i++) {
                servDhcp6bind ntry = new servDhcp6bind();
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
        if (a.equals("gateway")) {
            gateway = new addrIPv6();
            if (gateway.fromString(cmd.word())) {
                gateway = null;
                cmd.error("bad address");
                return false;
            }
            return false;
        }
        if (a.equals("netmask")) {
            netmask = new addrIPv6();
            if (netmask.fromString(cmd.word())) {
                netmask = null;
                cmd.error("bad address");
                return false;
            }
            return false;
        }
        if (a.equals("dns-server")) {
            a = cmd.word();
            dns1 = new addrIPv6();
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
            dns2 = new addrIPv6();
            if (dns2.fromString(a)) {
                dns2 = null;
                cmd.error("bad address");
                return false;
            }
            return false;
        }
        if (a.equals("boot-url")) {
            bootUrl = cmd.word();
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
        if (a.equals("preference")) {
            prefer = bits.str2num(cmd.word());
            return false;
        }
        if (a.equals("forbidden")) {
            addrMac mac = new addrMac();
            if (mac.fromString(cmd.word())) {
                cmd.error("bad mac address");
                return false;
            }
            servDhcp6bind ntry = new servDhcp6bind();
            ntry.mac = mac;
            forbidden.add(ntry);
            return false;
        }
        if (a.equals("static")) {
            addrMac mac = new addrMac();
            addrIPv6 ip = new addrIPv6();

            String macStr = cmd.word();
            if (mac.fromString(macStr)) {
                cmd.error("bad mac address: " + macStr);
                return false;
            }

            String ipStr = cmd.word();
            if (ip.fromString(ipStr)) {
                cmd.error("bad ipv6 address: " + ipStr);
                return false;
            }

            // check if there's already a binding for this MAC
            boolean existingFound = false;
            synchronized (bindings) {
                for (int i = 0; i < bindings.size(); i++) {
                    servDhcp6bind existing = bindings.get(i);
                    if (existing == null) {
                        continue;
                    }
                    if (existing.mac.compareTo(mac) == 0) {
                        // Update existing binding to be static
                        existing.ip = ip.copyBytes();
                        existing.confed = true;
                        existing.reqd = bits.getTime();

                        if (debugger.servDhcp6traf) {
                            logger.info("dhcp6 static: updated existing binding for mac " + mac + " with ip " + ip);
                        }
                        existingFound = true;
                        break;
                    }
                }

                // If no existing binding found, create a new static one
                if (!existingFound) {
                    servDhcp6bind ntry = new servDhcp6bind();
                    ntry.mac = mac.copyBytes();
                    ntry.ip = ip.copyBytes();
                    ntry.confed = true;
                    ntry.reqd = bits.getTime();

                    bindings.add(ntry);

                    if (debugger.servDhcp6traf) {
                        logger.info("dhcp6 static: created new static binding for mac " + mac + " with ip " + ip);
                    }
                }
            }
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
        if (a.equals("helper-addresses")) {
            helperAddresses.clear();
            return false;
        }
        if (a.equals("bind-file")) {
            bindFile = null;
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
        if (a.equals("boot-url")) {
            bootUrl = "";
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
        if (a.equals("dynamic-address")) {
            dynamicAddress = false;
            return false;
        }
        if (a.equals("use-interface-id")) {
            useInterfaceId = false;
            return false;
        }
        if (a.equals("subscriber-id")) {
            subscriberId = "";
            return false;
        }
        if (a.equals("max-hop-count")) {
            maxHopCount = 10;
            return false;
        }
        if (a.equals("forbidden")) {
            addrMac mac = new addrMac();
            if (mac.fromString(cmd.word())) {
                cmd.error("bad mac address");
                return false;
            }
            servDhcp6bind ntry = new servDhcp6bind();
            ntry.mac = mac;
            forbidden.del(ntry);
            return false;
        }
        if (a.equals("static")) {
            addrMac mac = new addrMac();
            String macStr = cmd.word();
            if (mac.fromString(macStr)) {
                cmd.error("bad mac address: " + macStr);
                return false;
            }

            findBinding(mac, 2, null);
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
        l.add(null, false, 1, new int[]{2}, "mode", "operation mode server (default) or relay");
        l.add(null, false, 2, new int[]{-1}, "server", "server mode");
        l.add(null, false, 2, new int[]{-1}, "relay", "relay mode");
        l.add(null, false, 1, new int[]{-1}, "use-interface-id", "use interface-id option in relay");
        l.add(null, false, 1, new int[]{2}, "subscriber-id", "subscriber ID for relay");
        l.add(null, false, 2, new int[]{-1}, "<str>", "subscriber ID string");
        l.add(null, false, 1, new int[]{2}, "max-hop-count", "maximum hop count for relay");
        l.add(null, false, 2, new int[]{-1}, "<num>", "hop count limit");
        l.add(null, false, 1, new int[]{2}, "helper-addresses", "upstream DHCP6 server for relay");
        l.add(null, false, 2, new int[]{2, -1}, "<addr>", "server address");
        l.add(null, false, 1, new int[]{2}, "bind-file", "save bindings");
        l.add(null, false, 2, new int[]{2, -1}, "<str>", "file name");
        l.add(null, false, 1, new int[]{2}, "gateway", "gateway address to delegate");
        l.add(null, false, 2, new int[]{-1}, "<addr>", "address of gateway");
        l.add(null, false, 1, new int[]{2}, "dns-server", "address(es) of name server(s) to delegate");
        l.add(null, false, 2, new int[]{3, -1}, "<addr>", "dns#1 server address");
        l.add(null, false, 3, new int[]{-1}, "<addr>", "dns#2 server address");
        l.add(null, false, 1, new int[]{2}, "boot-url", "url to boot file");
        l.add(null, false, 2, new int[]{-1}, "<str>", "url");
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
        l.add(null, false, 1, new int[]{2}, "preference", "server preference value");
        l.add(null, false, 2, new int[]{-1}, "<num>", "preference value");
        l.add(null, false, 1, new int[]{2}, "static", "address pool to use");
        l.add(null, false, 2, new int[]{3}, "<addr>", "mac address of client");
        l.add(null, false, 3, new int[]{-1}, "<addr>", "ip address of client");
        l.add(null, false, 1, new int[]{2}, "forbidden", "address pool to use");
        l.add(null, false, 2, new int[]{-1}, "<addr>", "mac address of client");
        l.add(null, false, 1, new int[]{2}, "option", "specify custom option");
        l.add(null, false, 2, new int[]{3, -1}, "<num>", "type of option");
        l.add(null, false, 3, new int[]{3, -1}, "<num>", "data byte");
        l.add(null, false, 1, new int[]{-1}, "dynamic-address", "enable dynamic address allocation");
    }

    public String srvName() {
        return "dhcp6";
    }

    public int srvPort() {
        return packDhcp6.portSnum;
    }

    public int srvProto() {
        return protoIp6 + protoUdp;
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
            if (debugger.servDhcp6traf) {
                logger.error("dhcp6 relay: no upstream servers configured");
            }
            return true;
        }

        // Relay mode: start datagram service on specific relay interfaces only
        if (relayInterfaces.isEmpty()) {
            if (debugger.servDhcp6traf) {
                logger.debug("DHCP6 Relay: Waiting for interface configuration - no interfaces configured yet");
            }
            return false; // Not ready yet - wait for interfaces to be configured
        }

        if (debugger.servDhcp6traf) {
            logger.info("DHCP6 Relay: Starting service on " + relayInterfaces.size() + " configured interfaces");
        }

        // Start UDP listening on interfaces
        boolean allStarted = true;

        // Multi-interface mode - start on all relay interfaces
        if (debugger.servDhcp6traf) {
            logger.info("dhcp6 relay: starting datagram service on " + relayInterfaces.size() + " relay interfaces");
        }
        for (int i = 0; i < relayInterfaces.size(); i++) {
            cfgIfc ifc = relayInterfaces.get(i);
            if (ifc == null) {
                continue;
            }
            boolean result = srvVrf.udp6.packetListen(this, ifc.fwdIf6, srvPort(), null, 0, srvName(), -1, null, -1, -1);
            if (debugger.servDhcp6traf) {
                logger.info("dhcp6 relay: packetListen result=" + result + " for interface " + ifc.name + " port " + srvPort());
            }
            if (result) { // packetListen returns true on error
                allStarted = false;
            }
        }

        if (debugger.servDhcp6traf) {
            logger.info("DHCP6 Relay: Service start result: " + (!allStarted ? "FAILED" : "SUCCESS"));
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
            if (debugger.servDhcp6traf) {
                logger.info("DHCP6 Relay: Starting packetStop on interface " + ifc.name);
            }
            srvVrf.udp6.listenStop(ifc.fwdIf6, srvPort(), null, 0);
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
        servDhcp6timer task = new servDhcp6timer(this);
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
    private servDhcp6bind findBinding(addrMac mac, int create, addrIPv6 hint) {
        if ((hint != null) && (gateway != null) && (netmask != null)) {
            addrIPv6 a1 = new addrIPv6();
            addrIPv6 a2 = new addrIPv6();
            a1.setAnd(gateway, netmask);
            a2.setAnd(hint, netmask);
            if (a1.compareTo(a2) == 0) {
                hint = hint.copyBytes();
            } else {
                hint = null;
            }
        }
        servDhcp6bind ntry = new servDhcp6bind();
        ntry.mac = mac.copyBytes();
        if (forbidden.find(ntry) != null) {
            return null;
        }

        // Debug log for MAC address
        if (debugger.servDhcp6traf) {
            logger.info("dhcp6 findbinding: looking for binding with mac " + mac);
        }

        synchronized (bindings) {
            // Check for static bindings by MAC address
            for (int i = 0; i < bindings.size(); i++) {
                servDhcp6bind existing = bindings.get(i);
                if (existing == null) {
                    continue;
                }
                if (existing.mac.equals(mac) && existing.confed) {
                    if (debugger.servDhcp6traf) {
                        logger.info("dhcp6 findbinding: found static binding for mac " + mac + " with ip " + existing.ip);
                    }

                    // Update timestamp but preserve the static configuration
                    if (create == 1) {
                        existing.reqd = bits.getTime();
                    }

                    return existing;
                }
            }

            // Check for dynamic bindings by MAC address
            for (int i = 0; i < bindings.size(); i++) {
                servDhcp6bind existing = bindings.get(i);
                if (existing == null) {
                    continue;
                }
                if (existing.mac.compareTo(mac) == 0) {
                    if (debugger.servDhcp6traf) {
                        logger.info("dhcp6 findbinding: found dynamic binding for mac " + mac + " with ip " + existing.ip);
                    }

                    // Update timestamp
                    if (create == 1) {
                        existing.reqd = bits.getTime();
                    }

                    // Handle release/decline
                    if ((create == 3) && (!existing.confed)) {
                        create--;
                    }
                    if (create == 2) {
                        existing.confed = false;
                        if (remember < 1) {
                            bindings.remove(existing);
                            if (debugger.servDhcp6traf) {
                                logger.info("dhcp6 findbinding: removed binding for mac " + mac);
                            }
                        } else {
                            existing.reqd = bits.getTime() - lease + remember;
                            if (debugger.servDhcp6traf) {
                                logger.info("dhcp6 findbinding: updated binding expiry for mac " + mac);
                            }
                        }
                    }
                    return existing;
                }
            }

            // create new if requested
            if (create != 1) {
                if (debugger.servDhcp6traf) {
                    logger.info("dhcp6 findbinding: no binding found for mac " + mac + " and create != 1");
                }
                return null;
            }

            // Check if dynamic address allocation is disabled
            if (!dynamicAddress) {
                // If we're creating a static binding (hint is provided and will be marked as confed later),
                // we should allow it even when dynamic address allocation is disabled
                boolean isCreatingStaticBinding = (hint != null && create == 1);

                if (!isCreatingStaticBinding) {
                    if (debugger.servDhcp6traf) {
                        logger.info("dhcp6 findbinding: dynamic address allocation is disabled, checking if " + mac + " has static binding");
                    }

                    // Look for static bindings again
                    for (int i = 0; i < bindings.size(); i++) {
                        servDhcp6bind existing = bindings.get(i);
                        if (existing == null) {
                            continue;
                        }
                        if (existing.mac.compareTo(mac) == 0 && existing.confed) {
                            if (debugger.servDhcp6traf) {
                                logger.info("dhcp6 findbinding: found static binding for mac " + mac + " when checking for dynamic allocation");
                            }
                            return existing;
                        }
                    }

                    if (debugger.servDhcp6traf) {
                        logger.info("dhcp6 findbinding: no static binding found for mac " + mac + ", not creating dynamic binding");
                    }
                    return null;
                } else {
                    if (debugger.servDhcp6traf) {
                        logger.info("dhcp6 findbinding: allowing creation of static binding for mac " + mac + " even though dynamic allocation is disabled");
                    }
                }
            }

            // Create new binding
            ntry = new servDhcp6bind();
            ntry.mac = mac.copyBytes();
            ntry.ip = hint;
            ntry.reqd = bits.getTime();

            // Generate IPv6 address if not provided
            if (ntry.ip == null && gateway != null) {
                ntry.ip = addrIPv6.genPublic(mac, gateway);
                if (debugger.servDhcp6traf) {
                    logger.info("dhcp6 findbinding: generated new ip " + ntry.ip + " for mac " + mac);
                }
            }

            bindings.add(ntry);
            if (debugger.servDhcp6traf) {
                logger.info("dhcp6 findbinding: created new binding for mac " + mac + " with ip " + ntry.ip);
            }
            return ntry;
        }
    }

    private synchronized boolean sendPack(packDhcp6 pckd, servDhcp6bind ntry) {
        addrIP adr = new addrIP();
        adr.fromIPv6addr(ntry.ip);
        srvIface.ipIf6.updateL2info(0, ntry.mac, adr);
        if (debugger.servDhcp6traf) {
            logger.debug("tx " + adr + " " + pckd);
        }
        pipeSide pip = srvVrf.udp6.streamConnect(new pipeLine(32768, true), srvIface.fwdIf6, packDhcp6.portSnum, adr, packDhcp6.portCnum, srvName(), -1, null, -1, -1);
        if (pip == null) {
            return true;
        }
        pip.wait4ready(1000);
        pip.setTime(1000);
        packHolder pckh = new packHolder(true, true);
        pckd.createPacket(pckh, options);
        pckh.merge2end();
        pckh.pipeSend(pip, 0, pckh.dataSize(), 2);
        pip.setClose();
        return false;
    }

    /**
     * process one received packet
     *
     * @param req packet received
     * @return packet to send back, null=nothing
     */
    protected packDhcp6 gotPack(packDhcp6 req) {
        packDhcp6 rep = new packDhcp6();
        rep.msgId = req.msgId;
        rep.clntId = req.clntId;
        rep.servId = packDhcp6.encodeDUID(srvIface.ethtyp.getHwAddr());
        rep.iamod = req.iamod;
        rep.iaid = req.iaid;
        rep.iat1 = renew / 1000;
        rep.iat2 = lease / 1000;

        // If client requested no IA options (iamod = 0), ensure we don't add any
        if (req.iamod == 0) {
            if (debugger.servDhcp6traf) {
                logger.info("dhcp6 server: client requested configuration only (no IA options)");
            }
        }
        if (dns1 != null) {
            rep.dns1srv = dns1.copyBytes();
        }
        if (dns2 != null) {
            rep.dns2srv = dns2.copyBytes();
        }
        rep.domainName = domNam;
        rep.bootUrl = bootUrl;
        rep.lifetimP = lease;
        rep.lifetimV = lease;
        rep.servPref = prefer;
        rep.servAddr = gateway != null ? gateway.copyBytes() : null;
        rep.ipsize = netmask != null ? netmask.toNetmask() : 0;
        rep.status = 0;
        int crt = 1;
        switch (req.msgTyp) {
            case packDhcp6.typSolicit:
                rep.msgTyp = packDhcp6.typAdvertise;
                break;
            case packDhcp6.typInfo:
                rep.msgTyp = packDhcp6.typReply;
                // Info requests don't need IA options, always respond
                return rep;
            case packDhcp6.typRequest:
            case packDhcp6.typConfirm:
            case packDhcp6.typRenew:
            case packDhcp6.typRebind:
                rep.msgTyp = packDhcp6.typReply;
                break;
            case packDhcp6.typRelease:
            case packDhcp6.typDecline:
                rep.msgTyp = packDhcp6.typReply;
                crt = 3;
                break;
            case packDhcp6.typReReq: // Relay-Forward
                if (debugger.servDhcp6traf) {
                    logger.info("dhcp6 server: received relay-forward packet, processing...");
                }
                return processRelayForward(req);
            default:
                if (debugger.servDhcp6traf) {
                    logger.info("dhcp6 server: unknown message type: " + req.msgTyp);
                }
                return null;
        }

        // Skip IP address assignment if client didn't request any IA options
        if (req.iamod == 0) {
            // Client requests configuration only (no IA options)
            rep.ipaddr = null; // Don't set any IP address
            if (debugger.servDhcp6traf) {
                logger.info("dhcp6 server: client requested configuration only, sending without IA options");
            }
            return rep;
        }

        // Only respond if server supports the requested IA type
        if (req.iamod == 3) {
            // Client requests IA_PD (prefix delegation) but server doesn't support it
            rep.status = 2; // NoAddrsAvail
            rep.ipaddr = null; // Don't set any IP address for IA_PD requests
            if (debugger.servDhcp6traf) {
                logger.info("dhcp6 server: rejecting IA_PD request with NoAddrsAvail");
            }
            return rep;
        }

        addrMac mac = packDhcp6.decodeDUID(req.clntId);
        if (mac == null) {
            rep.status = 1;
            return rep;
        }
        servDhcp6bind ntry = findBinding(mac, crt, req.ipaddr);
        if (crt != 1) {
            rep.ipaddr = req.ipaddr;
            rep.ipsize = req.ipsize;
            rep.lifetimP = 0;
            rep.lifetimV = 0;
            rep.iat1 = 0;
            rep.iat2 = 0;
            rep.status = 0;
            return rep;
        }
        if (ntry == null) {
            if (debugger.servDhcp6traf) {
                logger.error("dhcp6 gotpack: findbinding returned null for mac " + mac);
            }

            // Create a new binding directly
            ntry = new servDhcp6bind();
            ntry.mac = mac.copyBytes();
            ntry.reqd = bits.getTime();

            if (forbidden.find(ntry) != null) {
                return null;
            }

            // Generate IP address using MAC and gateway
            if (gateway != null) {
                ntry.ip = addrIPv6.genPublic(mac, gateway);
                if (debugger.servDhcp6traf) {
                    logger.info("dhcp6 gotpack: created new binding with ip " + ntry.ip + " for mac " + mac);
                }
            } else {
                if (debugger.servDhcp6traf) {
                    logger.error("dhcp6 gotpack: refused to create default ip address for mac " + mac);
                }
                return null;
            }

            // Always add to bindings list if we have an IP
            if (ntry.ip != null) {
                synchronized (bindings) {
                    bindings.add(ntry);
                    if (debugger.servDhcp6traf) {
                        logger.info("dhcp6 gotpack: added new binding to list, total bindings: " + bindings.size());
                    }
                }
            }
        } else {
            // Update request time
            ntry.reqd = bits.getTime();

            // Generate IP address if not already set
            if (ntry.ip == null && gateway != null) {
                if (debugger.servDhcp6traf) {
                    logger.info("dhcp6 gotpack: generating new ipv6 address for mac " + mac + " using gateway " + gateway);
                }
                ntry.ip = addrIPv6.genPublic(mac, gateway);
                if (debugger.servDhcp6traf) {
                    logger.info("dhcp6 gotpack: generated ip: " + ntry.ip);
                }
            }
        }

        // Set IP address in reply
        if (ntry.ip != null) {
            rep.ipaddr = ntry.ip.copyBytes();
            if (debugger.servDhcp6traf) {
                logger.info("dhcp6 gotpack: assigned ip " + ntry.ip + " to mac " + mac);
            }
        } else {
            if (debugger.servDhcp6traf) {
                logger.error("dhcp6 gotpack: failed to assign ip address to mac " + mac);
            }
        }

        // Ensure binding is in the list
        boolean foundInList = false;
        synchronized (bindings) {
            for (int i = 0; i < bindings.size(); i++) {
                servDhcp6bind existing = bindings.get(i);
                if (existing == null) {
                    continue;
                }
                if (existing.mac.compareTo(mac) == 0) {
                    foundInList = true;
                    break;
                }
            }

            if (!foundInList && ntry != null && ntry.ip != null) {
                bindings.add(ntry);
                if (debugger.servDhcp6traf) {
                    logger.info("dhcp6 gotpack: added missing binding to list, total bindings: " + bindings.size());
                }
            }
        }

        // Send packet
        sendPack(rep, ntry);
        return rep;
    }

    /**
     * Process DHCP6 Relay-Forward packet and create relay-reply
     *
     * @param relayReq the relay-forward packet
     * @return relay-reply packet
     */
    private packDhcp6 processRelayForward(packDhcp6 relayReq) {
        if (debugger.servDhcp6traf) {
            logger.info("dhcp6 server: processing relay-forward packet");
        }

        // For now, create a simple advertise response instead of a complex relay-reply
        // This is a simplified approach that should work with the existing createPacket method
        packDhcp6 advertise = new packDhcp6();
        advertise.msgTyp = packDhcp6.typAdvertise; // Send normal Advertise for now
        advertise.msgId = relayReq.msgId;
        advertise.servId = packDhcp6.encodeDUID(srvIface.ethtyp.getHwAddr());
        advertise.servPref = prefer;
        if (gateway != null) {
            advertise.servAddr = gateway.copyBytes();
        }
        advertise.domainName = domNam;
        advertise.bootUrl = bootUrl;

        if (debugger.servDhcp6traf) {
            logger.info("dhcp6 server: created advertise response for relay-forward (simplified)");
        }
        return advertise;
    }

    /**
     * Create nested relay-reply structure that mirrors the Relay-Forward
     * nesting
     *
     * @param relayForwardPck packet to deal with
     * @return reply, null if nothing
     */
    public packHolder createRelayReply(packHolder relayForwardPck) {
        try {
            if (debugger.servDhcp6traf) {
                logger.info("dhcp6 createRelayReply: creating nested rfc 3315 compliant relay-reply");
            }

            // Check packet size
            if (relayForwardPck.dataSize() < 34) {
                if (debugger.servDhcp6traf) {
                    logger.error("dhcp6 createRelayReply: relay-forward packet too small: " + relayForwardPck.dataSize());
                }
                return null;
            }

            // Extract message type and hop count
            int msgType = relayForwardPck.getByte(0) & 0xff;
            int hopCount = relayForwardPck.getByte(1) & 0xff;

            if (debugger.servDhcp6traf) {
                logger.info("dhcp6 createRelayReply: msgType=" + msgType + ", hopCount=" + hopCount);
            }

            // Verify this is a Relay-Forward message
            if (msgType != packDhcp6.typReReq) {
                if (debugger.servDhcp6traf) {
                    logger.error("dhcp6 createRelayReply: not a relay-forward message: " + msgType);
                }
                return null;
            }

            // Use the new recursive method to create nested reply
            return createNestedRelayReply(relayForwardPck);

        } catch (Exception e) {
            logger.traceback(e);
            return null;
        }
    }

    /**
     * Recursively create nested relay-reply structure
     */
    private packHolder createNestedRelayReply(packHolder relayForwardPck) {
        try {
            if (debugger.servDhcp6traf) {
                logger.info("dhcp6 createNestedRelayReply: processing relay forward packet");
            }

            // Extract header information
            int msgType = relayForwardPck.getByte(0) & 0xff;
            int hopCount = relayForwardPck.getByte(1) & 0xff;

            // Verify this is a Relay-Forward message
            if (msgType != packDhcp6.typReReq) {
                if (debugger.servDhcp6traf) {
                    logger.error("dhcp6 createNestedRelayReply: not a relay-forward message: " + msgType);
                }
                return null;
            }

            // Copy link-address (16 bytes)
            byte[] linkAddr = new byte[16];
            relayForwardPck.getCopy(linkAddr, 0, 2, 16);

            // Copy peer-address (16 bytes)
            byte[] peerAddr = new byte[16];
            relayForwardPck.getCopy(peerAddr, 0, 18, 16);

            // Find and extract the relay message option and other options
            int offset = 34;
            packHolder relayMessageContent = null;
            List<byte[]> otherOptions = new ArrayList<byte[]>();

            while (offset + 4 <= relayForwardPck.dataSize()) {
                int optType = relayForwardPck.msbGetW(offset);
                int optLen = relayForwardPck.msbGetW(offset + 2);

                if (optType == D6O_RELAY_MSG) { // Relay Message Option
                    if (debugger.servDhcp6traf) {
                        logger.info("dhcp6 createNestedRelayReply: found relay message option, length=" + optLen);
                    }

                    // Extract the relay message content
                    relayMessageContent = new packHolder(true, true);
                    for (int i = 0; i < optLen; i++) {
                        relayMessageContent.putByte(i, relayForwardPck.getByte(offset + 4 + i));
                    }
                    relayMessageContent.putSkip(optLen);
                    relayMessageContent.merge2beg();

                } else {
                    // Copy other options (like Interface-Id)
                    if (debugger.servDhcp6traf) {
                        logger.info("dhcp6 createNestedRelayReply: found option type " + optType + ", length=" + optLen);
                    }

                    byte[] optionData = new byte[optLen + 4];
                    relayForwardPck.getCopy(optionData, 0, offset, optionData.length);
                    otherOptions.add(optionData);
                }

                offset += 4 + optLen;
            }

            if (relayMessageContent == null) {
                if (debugger.servDhcp6traf) {
                    logger.error("dhcp6 createNestedRelayReply: relay message option not found");
                }
                return null;
            }

            // Determine what to put in this relay reply
            packHolder responseContent;

            // Check if the relay message content is another Relay-Forward
            if (relayMessageContent.dataSize() > 0
                    && relayMessageContent.getByte(0) == packDhcp6.typReReq) {

                if (debugger.servDhcp6traf) {
                    logger.info("dhcp6 createNestedRelayReply: found nested relay-forward, processing recursively");
                }

                // Recursively process the nested Relay-Forward
                responseContent = createNestedRelayReply(relayMessageContent);
                if (responseContent == null) {
                    if (debugger.servDhcp6traf) {
                        logger.error("dhcp6 createNestedRelayReply: failed to process nested relay-forward");
                    }
                    return null;
                }

            } else {
                // This is the innermost level - create server response
                if (debugger.servDhcp6traf) {
                    logger.info("dhcp6 createNestedRelayReply: found client message, creating server response");
                }

                responseContent = createServerResponse(relayMessageContent);
                if (responseContent == null) {
                    if (debugger.servDhcp6traf) {
                        logger.error("dhcp6 createNestedRelayReply: could not create server response");
                    }
                    return null;
                }
            }

            // Create the relay-reply packet
            packHolder relayReply = new packHolder(true, true);

            // Message type (1 byte) = 13 (RELAY-REPL)
            relayReply.putByte(0, packDhcp6.typReRep);

            // Copy hop count from Relay-Forward
            relayReply.putByte(1, hopCount);

            // Copy link-address (16 bytes)
            relayReply.putCopy(linkAddr, 0, 2, 16);

            // Copy peer-address (16 bytes)
            relayReply.putCopy(peerAddr, 0, 18, 16);

            // Skip to end of header
            relayReply.putSkip(34);

            // Add all other options first (like Interface-Id)
            for (int i = 0; i < otherOptions.size(); i++) {
                byte[] optionBytes = otherOptions.get(i);
                relayReply.putCopy(optionBytes, 0, 0, optionBytes.length);
                relayReply.putSkip(optionBytes.length);
            }

            // Add Relay Message option containing the response content
            relayReply.msbPutW(0, D6O_RELAY_MSG);      // Option type
            relayReply.msbPutW(2, responseContent.dataSize()); // Length

            // Copy response content into option
            byte[] responseData = new byte[responseContent.dataSize()];
            responseContent.getCopy(responseData, 0, 0, responseContent.dataSize());
            relayReply.putCopy(responseData, 0, 4, responseContent.dataSize());
            relayReply.putSkip(4 + responseContent.dataSize());

            // Finalize packet
            relayReply.merge2beg();

            if (debugger.servDhcp6traf) {
                logger.info("dhcp6 createNestedRelayReply: created relay-reply packet, size=" + relayReply.dataSize() + ", hopCount=" + hopCount);
            }

            return relayReply;

        } catch (Exception e) {
            logger.traceback(e);
            return null;
        }
    }

    /**
     * Extract original client message from Relay Message Option
     */
    private packHolder extractRelayMessageFromForward(packHolder relayForwardPck, int startOffset) {
        try {
            int offset = startOffset;

            // Search for Relay Message Option (Type 9)
            while (offset + 4 <= relayForwardPck.dataSize()) {
                int optType = relayForwardPck.msbGetW(offset);
                int optLen = relayForwardPck.msbGetW(offset + 2);

                if (optType == 9) { // Relay Message Option
                    if (debugger.servDhcp6traf) {
                        logger.info("dhcp6 extractRelayMessageFromForward: found relay message option, length=" + optLen);
                    }

                    packHolder originalMsg = new packHolder(true, true);
                    for (int i = 0; i < optLen; i++) {
                        originalMsg.putByte(i, relayForwardPck.getByte(offset + 4 + i));
                    }
                    originalMsg.putSkip(optLen);
                    originalMsg.merge2beg();

                    // Check if this is another Relay-Forward message, and if so, extract its content recursively
                    if (originalMsg.dataSize() > 0 && originalMsg.getByte(0) == packDhcp6.typReReq) {
                        if (debugger.servDhcp6traf) {
                            logger.info("dhcp6 extractRelayMessageFromForward: extracted message is another relay-forward, extracting recursively");
                        }
                        // This is another relay-forward message, extract its content recursively
                        packHolder nestedMsg = extractRelayMessageFromForward(originalMsg, 34); // Skip header of nested relay-forward
                        if (nestedMsg != null) {
                            return nestedMsg;
                        }
                    }

                    return originalMsg;
                }

                offset += 4 + optLen;
            }

            if (debugger.servDhcp6traf) {
                logger.error("dhcp6 extractRelayMessageFromForward: relay message option not found");
            }
            return null;

        } catch (Exception e) {
            logger.traceback(e);
            return null;
        }
    }

    /**
     * Create server response from original client message with proper IA_NA and
     * IP address assignment
     */
    private packHolder createServerResponse(packHolder originalMessage) {
        try {
            if (originalMessage.dataSize() < 4) {
                if (debugger.servDhcp6traf) {
                    logger.error("dhcp6 createServerResponse: original message too small");
                }
                return null;
            }

            int clientMsgType = originalMessage.getByte(0) & 0xff;
            int transactionId = originalMessage.msbGetD(1) >>> 8;

            if (debugger.servDhcp6traf) {
                logger.info("dhcp6 createServerResponse: client msgType=" + clientMsgType + ", transId=" + transactionId);
            }

            // Special handling for Relay-Forward messages that weren't properly extracted
            if (clientMsgType == packDhcp6.typReReq) {  // Relay-Forward (12)
                if (debugger.servDhcp6traf) {
                    logger.info("dhcp6 createServerResponse: found nested relay-forward, extracting client message");
                }
                // Try to extract the client message from this nested Relay-Forward
                packHolder extractedClientMsg = extractRelayMessageFromForward(originalMessage, 34);
                if (extractedClientMsg != null) {
                    // Process the extracted client message
                    return createServerResponse(extractedClientMsg);
                } else {
                    if (debugger.servDhcp6traf) {
                        logger.error("dhcp6 createServerResponse: failed to extract client message from nested relay-forward");
                    }
                    return null;
                }
            }

            // Extract Client Identifier and IA from original message if present
            int clientIAID = extractIAID(originalMessage);
            byte[] clientDUID = extractClientDUID(originalMessage);
            int requestedIAType = extractRequestedIAType(originalMessage);

            // Client requested no IA options
            if (requestedIAType == 0) {
                if (debugger.servDhcp6traf) {
                    logger.info("dhcp6 createServerResponse: client requested configuration only (no IA options)");
                }

                // Create response without IA options
                packHolder configOnlyResponse = new packHolder(true, true);
                configOnlyResponse.putByte(0, (clientMsgType == packDhcp6.typSolicit) ? packDhcp6.typAdvertise : packDhcp6.typReply);
                configOnlyResponse.msbPutD(1, transactionId << 8);

                int pos = 4;

                // Add Server Identifier (Option 2)
                byte[] serverDUID = packDhcp6.encodeDUID(srvIface.ethtyp.getHwAddr());
                if (serverDUID != null) {
                    configOnlyResponse.putByte(pos++, 0); // Option Type high
                    configOnlyResponse.putByte(pos++, 2); // Option Type low (2 = Server ID)
                    configOnlyResponse.putByte(pos++, 0); // Length high
                    configOnlyResponse.putByte(pos++, (byte) serverDUID.length); // Length low
                    for (int i = 0; i < serverDUID.length; i++) {
                        configOnlyResponse.putByte(pos++, serverDUID[i]);
                    }
                }

                // Add Client Identifier (Option 1)
                if (clientDUID != null) {
                    configOnlyResponse.putByte(pos++, 0); // Option Type high
                    configOnlyResponse.putByte(pos++, 1); // Option Type low (1 = Client ID)
                    configOnlyResponse.putByte(pos++, 0); // Length high
                    configOnlyResponse.putByte(pos++, (byte) clientDUID.length); // Length low
                    for (int i = 0; i < clientDUID.length; i++) {
                        configOnlyResponse.putByte(pos++, clientDUID[i]);
                    }
                }

                configOnlyResponse.setDataSize(pos);
                return configOnlyResponse;
            }

            // Handle different IA option requests
            if (requestedIAType == 0) {
                // Client requested configuration only (no IA options)
                if (debugger.servDhcp6traf) {
                    logger.info("dhcp6 createServerResponse: client requested configuration only (no IA options)");
                }
                // Already handled above, so this is just a redundant check
            } else if (requestedIAType == 25) {
                // Client requests IA_PD but server doesn't support it
                if (debugger.servDhcp6traf) {
                    logger.info("dhcp6 createServerResponse: client requests IA_PD but server doesn't support it, sending NoAddrsAvail");
                }
                // Create minimal error response
                packHolder errorResponse = new packHolder(true, true);
                errorResponse.putByte(0, (clientMsgType == packDhcp6.typSolicit) ? packDhcp6.typAdvertise : packDhcp6.typReply);
                errorResponse.msbPutD(1, transactionId << 8);

                // Add Server Identifier (Option 2)
                byte[] serverDUID = packDhcp6.encodeDUID(srvIface.ethtyp.getHwAddr());
                if (serverDUID != null) {
                    int pos = 4;
                    errorResponse.putByte(pos++, 0); // Option Type high
                    errorResponse.putByte(pos++, 2); // Option Type low (2 = Server ID)
                    errorResponse.putByte(pos++, 0); // Length high
                    errorResponse.putByte(pos++, (byte) serverDUID.length); // Length low
                    for (int i = 0; i < serverDUID.length; i++) {
                        errorResponse.putByte(pos++, serverDUID[i]);
                    }
                }

                // Add Client Identifier (Option 1)
                if (clientDUID != null) {
                    int pos = errorResponse.dataSize();
                    errorResponse.putByte(pos++, 0); // Option Type high
                    errorResponse.putByte(pos++, 1); // Option Type low (1 = Client ID)
                    errorResponse.putByte(pos++, 0); // Length high
                    errorResponse.putByte(pos++, (byte) clientDUID.length); // Length low
                    for (int i = 0; i < clientDUID.length; i++) {
                        errorResponse.putByte(pos++, clientDUID[i]);
                    }
                }

                // Add Status Code option (Type 13) with NoAddrsAvail (2)
                int pos = errorResponse.dataSize();
                errorResponse.putByte(pos++, 0);   // Option Type high
                errorResponse.putByte(pos++, 13);  // Option Type low (13 = Status Code)
                errorResponse.putByte(pos++, 0);   // Option Length high
                errorResponse.putByte(pos++, 2);   // Option Length low (2 bytes)
                errorResponse.putByte(pos++, 0);   // Status Code high
                errorResponse.putByte(pos++, 2);   // Status Code low (2 = NoAddrsAvail)

                return errorResponse;
            } else if (requestedIAType == 28) {
                // Client requested both IA_NA and IA_PD (RFC 7550)
                if (debugger.servDhcp6traf) {
                    logger.info("dhcp6 createServerResponse: client requests both IA_NA and IA_PD, but server only supports IA_NA");
                }
                // We'll continue and create a response with IA_NA only
                // The server should specify the unavailability of IA_PD resources with status codes
                requestedIAType = 3; // Treat as IA_NA request
            }

            // Create appropriate server response
            packHolder response = new packHolder(true, true);
            int responseType;

            if (clientMsgType == packDhcp6.typSolicit) {
                // Respond with Advertise containing proper IA_NA with IP address
                responseType = packDhcp6.typAdvertise;
                if (debugger.servDhcp6traf) {
                    logger.info("dhcp6 createServerResponse: creating Advertise response for solicit");
                }
            } else if (clientMsgType == packDhcp6.typRequest) {
                // Respond with Reply containing proper IA_NA with IP address
                responseType = packDhcp6.typReply;
                if (debugger.servDhcp6traf) {
                    logger.info("dhcp6 createServerResponse: creating Reply response for request");
                }
            } else {
                if (debugger.servDhcp6traf) {
                    logger.error("dhcp6 createServerResponse: unsupported client message type: " + clientMsgType);
                }
                return null;
            }

            // Build response packet
            response.putByte(0, responseType);
            response.msbPutD(1, transactionId << 8);

            int currentPos = 4;

            // Add IA_NA (Identity Association for Non-temporary Address) - Option Type 3
            response.putByte(currentPos++, 0);  // Option Type high
            response.putByte(currentPos++, 3);  // Option Type low (3 = IA_NA)
            response.putByte(currentPos++, 0);  // Option Length high
            response.putByte(currentPos++, 40); // Option Length low (40 bytes: 12 header + 28 IA Address)

            // IA_NA header (12 bytes)
            response.msbPutD(currentPos, clientIAID); // IAID
            currentPos += 4;

            response.msbPutD(currentPos, 0); // T1 (4 bytes) - 0 means server chooses
            currentPos += 4;

            response.msbPutD(currentPos, 0); // T2 (4 bytes) - 0 means server chooses
            currentPos += 4;

            // IA Address Option (Type 5) - 28 bytes
            response.msbPutW(currentPos, 5);  // Option Type
            currentPos += 2;
            response.msbPutW(currentPos, 24); // Option Length
            currentPos += 2;

            // IPv6 Address (16 bytes) - Generate from pool or use configured range
            // Create a MAC address from the client DUID
            addrMac clientMac = new addrMac();
            if (clientDUID != null && clientDUID.length >= 8) {
                // Extract MAC from DUID (usually starts at offset 8 for DUID-LL)
                int macOffset = 8;
                if (clientDUID.length >= 2 && clientDUID[0] == 0 && clientDUID[1] == 3) {
                    // DUID-LL type, MAC starts at offset 4
                    macOffset = 4;
                }
                if (clientDUID.length >= macOffset + 6) {
                    byte[] macBytes = new byte[6];
                    System.arraycopy(clientDUID, macOffset, macBytes, 0, 6);
                    clientMac.fromBuf(macBytes, 0);
                    if (debugger.servDhcp6traf) {
                        logger.info("dhcp6 createServerResponse: extracted mac " + clientMac + " from client duid");
                    }
                }
            }

            // If MAC extraction failed, create a random MAC
            if (clientMac.isFilled(0)) {
                if (debugger.servDhcp6traf) {
                    logger.info("dhcp6 createServerResponse: found default mac " + clientMac);
                }
                return null;
            }

            // First check if there's a static binding before generating IP
            addrIPv6 assignedAddr = null;
            servDhcp6bind staticBinding = null;
            synchronized (bindings) {
                for (int i = 0; i < bindings.size(); i++) {
                    servDhcp6bind existing = bindings.get(i);
                    if (existing == null) {
                        continue;
                    }
                    if (existing.mac.compareTo(clientMac) == 0 && existing.confed) {
                        staticBinding = existing;
                        assignedAddr = staticBinding.ip.copyBytes();
                        if (debugger.servDhcp6traf) {
                            logger.info("dhcp6 createServerResponse: found static binding for mac " + clientMac + " with ip " + existing.ip);
                        }
                        break;
                    }
                }
            }

            // Generate IP only if no static binding found and dynamic address allocation is enabled
            if (assignedAddr == null) {
                if (!dynamicAddress) {
                    if (debugger.servDhcp6traf) {
                        logger.info("dhcp6 createServerResponse: dynamic address allocation disabled and no static binding, not serving mac " + clientMac);
                    }
                    return null;
                }

                if (gateway == null) {
                    return null;
                }
                // Use addrIPv6.genPublic to generate a MAC-based address
                assignedAddr = addrIPv6.genPublic(clientMac, gateway);
                if (debugger.servDhcp6traf) {
                    logger.info("dhcp6 generateIPv6Address: generated " + assignedAddr + " for mac " + clientMac);
                }

                if (debugger.servDhcp6traf) {
                    logger.info("dhcp6 createServerResponse: generated new ip " + assignedAddr + " for mac " + clientMac);
                }
            }

            byte[] ipv6Bytes = assignedAddr.getBytes();
            for (int i = 0; i < 16; i++) {
                response.putByte(currentPos++, ipv6Bytes[i] & 0xff);
            }

            // Preferred lifetime (4 bytes) - 27000 seconds
            response.msbPutD(currentPos, renew / 1000);
            currentPos += 4;

            // Valid lifetime (4 bytes) - 43200 seconds
            response.msbPutD(currentPos, lease / 1000);
            currentPos += 4;

            // Add Server Identifier Option (Type 2)
            response.putByte(currentPos++, 0);  // Option Type high
            response.putByte(currentPos++, 2);  // Option Type low (2 = Server Identifier)
            response.putByte(currentPos++, 0);  // Option Length high
            response.putByte(currentPos++, 14); // Option Length low (14 bytes DUID)

            // DUID-LLT format (Type 1)
            response.putByte(currentPos++, 0);   // DUID Type high
            response.putByte(currentPos++, 1);   // DUID Type low (1 = DUID-LLT)
            response.putByte(currentPos++, 0);   // Hardware Type high
            response.putByte(currentPos++, 1);   // Hardware Type low (1 = Ethernet)

            // Time (4 bytes) - using current time approximation
            response.putByte(currentPos++, 0x60); // Time bytes
            response.putByte(currentPos++, 0x00);
            response.putByte(currentPos++, 0x00);
            response.putByte(currentPos++, 0x00);

            // Link-layer address (6 bytes MAC)
            response.putByte(currentPos++, 0x00);
            response.putByte(currentPos++, 0x11);
            response.putByte(currentPos++, 0x22);
            response.putByte(currentPos++, 0x33);
            response.putByte(currentPos++, 0x44);
            response.putByte(currentPos++, 0x55);

            // Add DNS Server Option (Type 23) if configured
            if (dns1 != null) {
                response.putByte(currentPos++, 0);  // Option Type high
                response.putByte(currentPos++, 23); // Option Type low (23 = DNS recursive name server)
                response.putByte(currentPos++, 0);  // Option Length high
                response.putByte(currentPos++, 16); // Option Length low (16 bytes for 1 IPv6 address)

                response.putAddr(currentPos, dns1);
                currentPos += addrIPv6.size;
            }

            response.putSkip(currentPos);
            response.merge2beg();

            // Create or update binding for this client
            // If we found a static binding earlier, update its timestamp
            if (staticBinding != null) {
                // Update static binding's timestamp
                staticBinding.reqd = bits.getTime();
                if (debugger.servDhcp6traf) {
                    logger.info("dhcp6 createServerResponse: using static binding IP " + assignedAddr);
                }
            } else {
                // Normal flow - use the generated address
                servDhcp6bind ntry = findBinding(clientMac, 1, assignedAddr);
                if (ntry == null) {
                    // Create new binding directly
                    ntry = new servDhcp6bind();
                    ntry.mac = clientMac.copyBytes();
                    ntry.ip = assignedAddr.copyBytes();
                    ntry.reqd = bits.getTime();

                    synchronized (bindings) {
                        bindings.add(ntry);
                        if (debugger.servDhcp6traf) {
                            logger.info("dhcp6 createServerResponse: added new binding for mac " + clientMac + " with ip " + assignedAddr + ", total bindings: " + bindings.size());
                        }
                    }
                } else {
                    // Update existing binding
                    ntry.reqd = bits.getTime();
                    if (debugger.servDhcp6traf) {
                        logger.info("dhcp6 createServerResponse: updated binding for mac " + clientMac + " with ip " + assignedAddr);
                    }
                }
            }

            String responseTypeName = (responseType == packDhcp6.typAdvertise) ? "Advertise" : "Reply";
            if (debugger.servDhcp6traf) {
                logger.info("dhcp6 createServerResponse: created " + responseTypeName + " with ia_na, size=" + response.dataSize() + ", assigned ip=" + assignedAddr);
            }
            return response;

        } catch (Exception e) {
            logger.traceback(e);
            return null;
        }
    }

    /**
     * Extract requested IA option type from the message (0=none, 3=IA_NA,
     * 25=IA_PD, 28=both) Returns a combined value when both IA_NA and IA_PD are
     * found (RFC 7550)
     */
    private int extractRequestedIAType(packHolder message) {
        if (message == null || message.dataSize() < 4) {
            if (debugger.servDhcp6traf) {
                logger.info("dhcp6 extractRequestedIAType: message too small or null");
            }
            return 0; // No IA options requested
        }

        // Special handling for Relay-Forward messages
        int msgType = message.getByte(0) & 0xff;
        if (msgType == packDhcp6.typReReq) { // Relay-Forward (12)
            if (debugger.servDhcp6traf) {
                logger.info("dhcp6 extractRequestedIAType: found relay-forward, extracting client message");
            }
            // Extract relay message option and check it
            packHolder extractedMsg = extractRelayMessageFromForward(message, 34);
            if (extractedMsg != null) {
                return extractRequestedIAType(extractedMsg); // Recursive call
            }
        }

        boolean foundIA_NA = false;
        boolean foundIA_PD = false;

        int offset = 4; // Skip message header
        while (offset + 4 <= message.dataSize()) {
            int optionType = message.msbGetW(offset);
            int optionLength = message.msbGetW(offset + 2);

            if (optionType == 3) { // IA_NA
                foundIA_NA = true;
                if (debugger.servDhcp6traf) {
                    logger.info("dhcp6 extractRequestedIAType: found IA_NA option");
                }
            } else if (optionType == 25) { // IA_PD
                foundIA_PD = true;
                if (debugger.servDhcp6traf) {
                    logger.info("dhcp6 extractRequestedIAType: found IA_PD option");
                }
            }

            offset += 4 + optionLength;

            // If both types found, we can return immediately
            if (foundIA_NA && foundIA_PD) {
                if (debugger.servDhcp6traf) {
                    logger.info("dhcp6 extractRequestedIAType: found both IA_NA and IA_PD options");
                }
                return 28; // Special value for both IA types
            }
        }

        // Return based on what was found
        if (foundIA_NA) {
            return 3;  // IA_NA only
        } else if (foundIA_PD) {
            return 25; // IA_PD only
        }

        // No IA options found
        if (debugger.servDhcp6traf) {
            logger.info("dhcp6 extractRequestedIAType: no IA options found, client requesting configuration only");
        }
        return 0; // No IA options requested
    }

    /**
     * Extract IAID from client message
     */
    private int extractIAID(packHolder message) {
        try {
            // Search for IA_NA option (Type 3) in the message
            int offset = 4; // Skip message header
            while (offset + 4 <= message.dataSize()) {
                int optType = message.msbGetW(offset);
                int optLen = message.msbGetW(offset + 2);

                if (optType == 3 && optLen >= 12) { // IA_NA option
                    // Extract IAID (first 4 bytes of IA_NA data)
                    int iaid = message.msbGetD(offset + 4);
                    if (debugger.servDhcp6traf) {
                        logger.info("dhcp6 extractIAID: found iaid=" + iaid);
                    }
                    return iaid;
                }

                offset += 4 + optLen;
            }
        } catch (Exception e) {
            logger.traceback(e);
        }

        // Default IAID if not found
        if (debugger.servDhcp6traf) {
            logger.info("dhcp6 extractIAID: using default iaid=0x12345678");
        }
        return 0x12345678;
    }

    /**
     * Extract Client DUID from client message
     */
    private byte[] extractClientDUID(packHolder message) {
        try {
            // Check for null or too small message
            if (message == null || message.dataSize() < 4) {
                if (debugger.servDhcp6traf) {
                    logger.warn("dhcp6 extractClientDUID: message too small or null");
                }
                return getDefaultDUID();
            }

            // Search for Client Identifier option (Type 1) in the message
            int offset = 4; // Skip message header
            while (offset + 4 <= message.dataSize()) {
                int optType = message.msbGetW(offset);
                int optLen = message.msbGetW(offset + 2);

                // Validate option length is within bounds
                if (optLen < 0 || offset + 4 + optLen > message.dataSize()) {
                    if (debugger.servDhcp6traf) {
                        logger.warn("dhcp6 extractClientDUID: invalid option length: " + optLen
                                + ", message size: " + message.dataSize()
                                + ", offset: " + offset);
                    }
                    break;
                }

                if (optType == 1) { // Client Identifier option
                    byte[] duid = new byte[optLen];
                    for (int i = 0; i < optLen; i++) {
                        duid[i] = (byte) (message.getByte(offset + 4 + i) & 0xff);
                    }
                    if (debugger.servDhcp6traf) {
                        logger.info("dhcp6 extractClientDUID: found client duid, length=" + optLen);
                    }
                    return duid;
                }

                offset += 4 + optLen;
            }
        } catch (Exception e) {
            logger.traceback(e);
        }

        return getDefaultDUID();
    }

    /**
     * Return a default DUID when extraction fails
     */
    private byte[] getDefaultDUID() {
        // Default DUID if not found
        if (debugger.servDhcp6traf) {
            logger.info("dhcp6 extractClientDUID: using default duid");
        }
        return new byte[]{0x00, 0x01, 0x00, 0x01, 0x60, 0x00, 0x00, 0x00, 0x00, 0x11, 0x22, 0x33, 0x44, 0x55};
    }

    /**
     * purge binding table
     */
    protected void doPurging() {
        if (debugger.servDhcp6traf) {
            logger.info("dhcp6 doPurging: starting purge process, bindings count: " + bindings.size());
        }

        synchronized (bindings) {
            long cur = bits.getTime();
            int purgedCount = 0;

            for (int i = bindings.size() - 1; i >= 0; i--) {
                servDhcp6bind ntry = bindings.get(i);
                if (ntry == null) {
                    if (debugger.servDhcp6traf) {
                        logger.info("dhcp6 doPurging: null binding at index " + i);
                    }
                    continue;
                }

                if (ntry.confed) {
                    if (debugger.servDhcp6traf) {
                        logger.info("dhcp6 doPurging: skipping confirmed binding for mac " + ntry.mac);
                    }
                    continue;
                }

                long age = cur - ntry.reqd;
                if (age < lease) {
                    if (debugger.servDhcp6traf) {
                        logger.info("dhcp6 doPurging: binding for mac " + ntry.mac + " still valid, age: " + bits.timePast(ntry.reqd) + ", lease: " + (lease / 1000) + "s");
                    }
                    continue;
                }

                if (debugger.servDhcp6traf) {
                    logger.info("dhcp6 doPurging: removing expired binding for mac " + ntry.mac + ", ip: " + ntry.ip + ", age: " + bits.timePast(ntry.reqd));
                }
                bindings.remove(i);
                purgedCount++;
            }

            if (debugger.servDhcp6traf) {
                logger.info("dhcp6 doPurging: purge complete, removed " + purgedCount + " bindings, remaining: " + bindings.size());
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
                servDhcp6bind ntry = bindings.get(i);
                if (ntry == null) {
                    continue;
                }
                txt.add("" + ntry);
            }
        }
        if (bits.buf2txt(true, txt, bindFile)) {
            if (debugger.servDhcp6traf) {
                logger.error("error saving bindings");
            }
        }
    }

    /**
     * get show
     *
     * @return result
     */
    public userFormat getShow() {
        userFormat res = new userFormat("|", "mac|ip|last");

        // Debug log for bindings
        if (debugger.servDhcp6traf) {
            logger.info("dhcp6 getShow: bindings count: " + bindings.size());
        }

        // Add each binding to the result
        for (int i = 0; i < bindings.size(); i++) {
            servDhcp6bind ntry = bindings.get(i);
            if (ntry == null) {
                continue;
            }
            String entry = ntry.mac + "|" + ntry.ip + "|" + bits.timePast(ntry.reqd);
            if (debugger.servDhcp6traf) {
                logger.info("dhcp6 getShow: adding binding: " + entry);
            }
            res.add(entry);
        }

        return res;
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
        if (debugger.servDhcp6traf) {
            logger.info("dhcp6 datagramRecv called, mode=" + mode + ", packet size=" + pck.dataSize() + ", from=" + id.peerAddr);
        }
        if (mode != dhcpMode.relay) {
            if (debugger.servDhcp6traf) {
                logger.info("dhcp6 datagramRecv: Not in relay mode, returning false");
            }
            return false;
        }

        long startTime = bits.getTime();

        try {
            // For relay purposes, we only need basic packet validation and message type
            if (pck.dataSize() < 4) {
                if (debugger.servDhcp6traf) {
                    logger.error("dhcp6 relayPacket: Packet too small: " + pck.dataSize());
                }
                relayStats.packetsInvalid++;
                relayStats.packetsDropped++;
                return false;
            }

            int msgType = pck.getByte(0) & 0xff;
            int msgId = pck.msbGetD(0) & 0xffffff;
            if (debugger.servDhcp6traf) {
                logger.info("dhcp6 relayPacket: received packet - msgType=" + msgType + ", msgId=0x" + Integer.toHexString(msgId));
            }

            // Create a simple DHCP6 object with just the message type
            packDhcp6 dhcp = new packDhcp6();
            dhcp.msgTyp = msgType;
            dhcp.msgId = msgId;

            if (debugger.servDhcp6traf) {
                logger.info("dhcp6 relayPacket: processing as msgType=" + dhcp.msgTyp);
            }

            // Handle different message types
            switch (dhcp.msgTyp) {
                case packDhcp6.typSolicit:
                case packDhcp6.typRequest:
                case packDhcp6.typConfirm:
                case packDhcp6.typRenew:
                case packDhcp6.typRebind:
                case packDhcp6.typRelease:
                case packDhcp6.typDecline:
                case packDhcp6.typInfo:
                case packDhcp6.typReReq:
                    return relayClientToServer(dhcp, pck, id);

                case packDhcp6.typAdvertise:
                case packDhcp6.typReply:
                case packDhcp6.typReconfig:
                case packDhcp6.typReRep:
                    return relayServerToClient(dhcp, pck, id);

                default:
                    if (debugger.servDhcp6traf) {
                        logger.error("dhcp6 relayPacket: unknown message type: " + dhcp.msgTyp);
                    }
                    relayStats.packetsInvalid++;
                    relayStats.packetsDropped++;
                    return false;
            }
        } catch (Exception e) {
            logger.traceback(e);
            relayStats.routingErrors++;
            relayStats.packetsDropped++;
            return false;
        } finally {
            long processingTime = bits.getTime() - startTime;
            relayStats.updateProcessingTime(processingTime);
            id.setClosing();
        }
    }

    private boolean relayClientToServer(packDhcp6 dhcp, packHolder pck, prtGenConn id) {
        relayStats.packetsClientToServer++;
        if (debugger.servDhcp6traf) {
            logger.info("dhcp6 relayClientToServer: processing client packet, msgType=" + dhcp.msgTyp + ", upstreamServers=" + helperAddresses.size());
        }

        // Check hop count for relay-forward messages
        if (dhcp.msgTyp == packDhcp6.typReReq) {
            // Extract hop count from relay message
            int hopCount = extractHopCount(pck);
            if (hopCount >= maxHopCount) {
                if (debugger.servDhcp6traf) {
                    logger.error("dhcp6 relayClientToServer: max hop count exceeded: " + hopCount);
                }
                relayStats.maxHopCountExceeded++;
                relayStats.packetsDropped++;
                return false;
            }
            relayStats.hopCountTotal += hopCount + 1;
            relayStats.relayForwardPackets++;
        }

        // Forward to all upstream servers
        if (helperAddresses.isEmpty()) {
            if (debugger.servDhcp6traf) {
                logger.error("dhcp6 relayClientToServer: no upstream servers configured");
            }
            relayStats.packetsDropped++;
            return false;
        }

        // Determine source interface - use the interface the packet came from
        cfgIfc sourceIface = findInterfaceByConnection(id);
        if (sourceIface == null) {
        } else {
            if (debugger.servDhcp6traf) {
                logger.info("dhcp6 relayClientToServer: using source interface: " + sourceIface.name);
            }
        }

        int forwardedCount = 0;
        for (int i = 0; i < helperAddresses.size(); i++) {
            addrIP serverAddr = helperAddresses.get(i);
            if (debugger.servDhcp6traf) {
                logger.info("dhcp6 relayClientToServer: forwarding to server " + serverAddr + " from interface " + (sourceIface != null ? sourceIface.name : "default"));
            }
            boolean success = false;
            if (sourceIface != null) {
                success = forwardToServerFromInterface(pck, serverAddr, dhcp, id, sourceIface);
            } else {
                success = forwardToServer(pck, serverAddr, dhcp, id);
            }
            if (success) {
                forwardedCount++;
                relayStats.packetsForwarded++;
            }
        }

        // If no servers were successfully contacted, count as dropped
        if (forwardedCount == 0) {
            relayStats.packetsDropped++;
            return false;
        }

        return true;
    }

    private boolean relayServerToClient(packDhcp6 dhcp, packHolder pck, prtGenConn id) {
        relayStats.packetsServerToClient++;

        // Only process relay-reply messages
        if (dhcp.msgTyp != packDhcp6.typReRep) {
            if (debugger.servDhcp6traf) {
                logger.info("dhcp6 relayServerToClient: dropping non-relay-reply message, msgType=" + dhcp.msgTyp);
            }
            relayStats.packetsDropped++;
            return false;
        }

        // Extract link address to determine source interface
        cfgIfc sourceIface = null;
        addrIPv6 linkAddr = extractLinkAddress(pck);
        if (linkAddr != null) {
            sourceIface = findInterfaceByAddress(linkAddr);
            if (sourceIface != null) {
                if (debugger.servDhcp6traf) {
                    logger.info("dhcp6 relayServerToClient: found interface " + sourceIface.name + " for link address " + linkAddr);
                }
            } else {
                if (debugger.servDhcp6traf) {
                    logger.warn("dhcp6 relayServerToClient: no interface found for link address " + linkAddr);
                }
            }
        }

        // Use interface-specific forwarding if available
        if (sourceIface != null) {
            return forwardToClientFromInterface(pck, dhcp, sourceIface);
        } else {
            return forwardToClient(pck, dhcp);
        }
    }

    private boolean forwardToServer(packHolder pck, addrIP serverAddr, packDhcp6 dhcp, prtGenConn id) {
        try {
            if (debugger.servDhcp6traf) {
                logger.info("dhcp6 forwardToServer: creating relay-forward message for server " + serverAddr);
            }
            // Create relay-forward message
            packHolder relayPck = createRelayForward(pck, dhcp, id);
            if (relayPck == null) {
                if (debugger.servDhcp6traf) {
                    logger.error("dhcp6 forwardToServer: failed to create relay-forward message");
                }
                relayStats.routingErrors++;
                return false;
            }

            if (debugger.servDhcp6traf) {
                logger.info("dhcp6 forwardToServer: created relay packet, size=" + relayPck.dataSize());
            }

            // Send to server
            prtGenConn conn = srvVrf.udp6.packetConnect(this, srvIface.fwdIf6, packDhcp6.portSnum, serverAddr, packDhcp6.portSnum, srvName(), -1, null, -1, -1);
            if (conn == null) {
                if (debugger.servDhcp6traf) {
                    logger.error("dhcp6 forwardToServer: failed to create connection to server " + serverAddr);
                }
                relayStats.routingErrors++;
                return false;
            }

            if (debugger.servDhcp6traf) {
                logger.info("dhcp6 forwardToServer: sending packet to server " + serverAddr + ", size=" + relayPck.dataSize());
            }
            conn.send2net(relayPck);
            conn.setClosing();
            return true;

        } catch (Exception e) {
            logger.traceback(e);
            relayStats.routingErrors++;
            return false;
        }
    }

    private boolean forwardToClient(packHolder pck, packDhcp6 dhcp) {
        try {
            // Extract original message from relay-reply
            packHolder clientPck = extractRelayMessage(pck);
            if (clientPck == null) {
                relayStats.optionParsingErrors++;
                relayStats.packetsDropped++;
                return false;
            }

            int extractedMsgType = clientPck.getByte(0) & 0xff;
            if (debugger.servDhcp6traf) {
                logger.info("dhcp6 forwardToClient: extracted client packet, size=" + clientPck.dataSize());
                logger.info("dhcp6 forwardToClient: client packet type=" + extractedMsgType);
            }

            // Check if the extracted message is another relay-reply (nested relay)
            if (extractedMsgType == packDhcp6.typReRep) {
                if (debugger.servDhcp6traf) {
                    logger.info("dhcp6 forwardToClient: extracted message is a relay-reply, forwarding to next relay agent");
                }

                // This is a nested relay-reply - forward it to the peer address from the OUTER relay-reply
                // The peer address in the outer relay-reply is the address of the next relay agent
                addrIPv6 peerAddr = extractPeerAddress(pck); // Peer address from OUTER relay-reply
                if (peerAddr == null) {
                    if (debugger.servDhcp6traf) {
                        logger.error("dhcp6 forwardToClient: failed to extract peer address for nested relay");
                    }
                    relayStats.optionParsingErrors++;
                    relayStats.packetsDropped++;
                    return false;
                }

                if (debugger.servDhcp6traf) {
                    logger.info("dhcp6 forwardToClient: forwarding nested relay-reply to next relay agent at " + peerAddr);
                }

                // Send the nested relay-reply to the peer (which should be the next relay agent)
                addrIP peerAddrIP = new addrIP();
                peerAddrIP.fromIPv6addr(peerAddr);
                prtGenConn conn = srvVrf.udp6.packetConnect(this, srvIface.fwdIf6, packDhcp6.portSnum, peerAddrIP, packDhcp6.portSnum, srvName(), -1, null, -1, -1);
                if (conn == null) {
                    if (debugger.servDhcp6traf) {
                        logger.error("dhcp6 forwardToClient: failed to create connection to next relay agent " + peerAddrIP);
                    }
                    relayStats.routingErrors++;
                    relayStats.packetsDropped++;
                    return false;
                }

                if (debugger.servDhcp6traf) {
                    logger.info("dhcp6 forwardToClient: sending nested relay-reply to next relay agent " + peerAddrIP + ", size=" + clientPck.dataSize());
                }

                conn.send2net(clientPck); // Send the nested relay-reply
                conn.setClosing();
                relayStats.packetsForwarded++;

                if (debugger.servDhcp6traf) {
                    logger.info("dhcp6 forwardToClient: successfully forwarded nested relay-reply to next relay agent");
                }

                return true;
            }

            // Extract peer address and send to client
            addrIPv6 peerAddr = extractPeerAddress(pck);
            if (peerAddr == null) {
                relayStats.optionParsingErrors++;
                relayStats.packetsDropped++;
                return false;
            }

            // Send to client
            addrIP peerAddrIP = new addrIP();
            peerAddrIP.fromIPv6addr(peerAddr);
            prtGenConn conn = srvVrf.udp6.packetConnect(this, srvIface.fwdIf6, packDhcp6.portSnum, peerAddrIP, packDhcp6.portCnum, srvName(), -1, null, -1, -1);
            if (conn == null) {
                relayStats.routingErrors++;
                relayStats.packetsDropped++;
                return false;
            }

            conn.send2net(clientPck);
            conn.setClosing();
            relayStats.packetsForwarded++;

            return true;
        } catch (Exception e) {
            logger.traceback(e);
            relayStats.routingErrors++;
            relayStats.packetsDropped++;
            return false;
        }
    }

    private packHolder createRelayForward(packHolder originalPck, packDhcp6 dhcp, prtGenConn id) {
        try {
            // Create new packet for relay-forward message
            packHolder relayPck = new packHolder(true, true);

            // Message type (1 byte) = 12 (RELAY-FORW)
            relayPck.putByte(0, packDhcp6.typReReq);

            // Determine the hop count
            int hopCount = 0;
            if (dhcp.msgTyp == packDhcp6.typReReq) {
                // Relay-Forward Message - extract and increment
                hopCount = extractHopCount(originalPck) + 1;
            } else {
                // Client Message - start with 0 (per RFC 8415)
                hopCount = 0;
            }

            // Check maxHopCount
            if (hopCount >= maxHopCount) {
                // Discard message
                return null;
            }

            // Set the incremented hop count
            relayPck.putByte(1, hopCount);

            // Link address (16 bytes) - address of link on which client is located
            relayPck.putAddr(2, id.iface.addr.toIPv6());

            // Peer address (16 bytes) - address of client or previous relay
            relayPck.putAddr(18, id.peerAddr.toIPv6());

            relayPck.putSkip(34); // Header is 34 bytes

            // Add interface-id option if enabled
            if (useInterfaceId) {
                byte[] ifNameBytes = ("" + id.iface).getBytes();
                relayPck.msbPutW(0, D6O_INTERFACE_ID); // Option type
                relayPck.msbPutW(2, ifNameBytes.length); // Length
                relayPck.putCopy(ifNameBytes, 0, 4, ifNameBytes.length);
                relayPck.putSkip(4 + ifNameBytes.length);
                relayStats.interfaceIdAdded++;
            }

            // Add subscriber-id option if configured
            if (subscriberId != null && !subscriberId.isEmpty()) {
                byte[] subIdBytes = subscriberId.getBytes();
                relayPck.msbPutW(0, D6O_SUBSCRIBER_ID); // Option type
                relayPck.msbPutW(2, subIdBytes.length);  // Length
                relayPck.putCopy(subIdBytes, 0, 4, subIdBytes.length);
                relayPck.putSkip(4 + subIdBytes.length);
                relayStats.subscriberIdAdded++;
            }

            // Add relay message option containing original packet
            relayPck.msbPutW(0, D6O_RELAY_MSG);      // Option type
            relayPck.msbPutW(2, originalPck.dataSize()); // Length
            byte[] originalData = new byte[originalPck.dataSize()];
            originalPck.getCopy(originalData, 0, 0, originalPck.dataSize());
            relayPck.putCopy(originalData, 0, 4, originalPck.dataSize());
            relayPck.putSkip(4 + originalPck.dataSize());
            relayStats.relayMessageAdded++;

            relayPck.merge2beg();
            return relayPck;

        } catch (Exception e) {
            logger.traceback(e);
            relayStats.bufferOverflowErrors++;
            return null;
        }
    }

    private packHolder extractRelayMessage(packHolder pck) {
        try {
            // DHCP6 relay-reply message format:
            // Skip header (34 bytes): msg-type(1) + hop-count(1) + link-address(16) + peer-address(16)
            if (pck.dataSize() < 34) {
                return null;
            }

            int pos = 34; // Start after header

            // Parse options to find relay message option (type 9)
            while (pos + 4 <= pck.dataSize()) {
                int optType = pck.msbGetW(pos);
                int optLen = pck.msbGetW(pos + 2);

                if (optType == D6O_RELAY_MSG) {
                    // Found relay message option, extract the original message
                    if (pos + 4 + optLen <= pck.dataSize()) {
                        packHolder originalMsg = new packHolder(true, true);
                        byte[] msgData = new byte[optLen];
                        pck.getCopy(msgData, 0, pos + 4, optLen);
                        originalMsg.putCopy(msgData, 0, 0, optLen);
                        originalMsg.putSkip(optLen);
                        originalMsg.merge2beg();
                        return originalMsg;
                    }
                    break;
                }

                pos += 4 + optLen; // Move to next option
            }

            return null;
        } catch (Exception e) {
            logger.traceback(e);
            return null;
        }
    }

    private addrIPv6 extractPeerAddress(packHolder pck) {
        try {
            // DHCP6 Relay message format:
            // Skip msg-type(1) + hop-count(1) + link-address(16) to get to peer-address(16)
            if (pck.dataSize() < 34) {
                return null;
            }

            addrIPv6 peerAddr = new addrIPv6();
            pck.getAddr(peerAddr, 18);
            return peerAddr;

        } catch (Exception e) {
            logger.traceback(e);
            return null;
        }
    }

    private int extractHopCount(packHolder pck) {
        try {
            // DHCP6 Relay message format:
            // msg-type(1) + hop-count(1) + etc
            if (pck.dataSize() < 2) {
                return 0;
            }

            return pck.getByte(1) & 0xff; // Hop count is at offset 1

        } catch (Exception e) {
            logger.traceback(e);
            return 0;
        }
    }

    private addrIPv6 extractLinkAddress(packHolder pck) {
        try {
            // DHCP6 relay-reply message format:
            // 0-1: msg-type + hop-count
            // 2-17: link-address (16 bytes)
            if (pck.dataSize() < 18) {
                return null;
            }

            byte[] linkAddrBytes = new byte[16];
            pck.getCopy(linkAddrBytes, 0, 2, 16);

            addrIPv6 linkAddr = new addrIPv6();
            linkAddr.fromBuf(linkAddrBytes, 0);

            // Check if it's not all zeros
            if (linkAddr.isLinkLocal() || !linkAddr.isEmpty()) {
                return linkAddr;
            }

            return null;
        } catch (Exception e) {
            logger.traceback(e);
            return null;
        }
    }

    /**
     * get relay statistics display for show command
     *
     * @return list of statistics lines
     */
    public List<String> getRelayStatisticsDisplay() {
        List<String> result = new ArrayList<String>();

        result.add("DHCP6 Relay Statistics for " + srvName);
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
        result.add("  Multi-hop Packets: " + relayStats.relayForwardPackets);
        result.add("  Max Hop Count Exceeded: " + relayStats.maxHopCountExceeded);
        result.add("  Average Hop Count: " + String.format("%.2f", relayStats.getAverageHopCount()));
        result.add("");

        // Agent options statistics (IPv6 specific)
        result.add("DHCPv6 Options Statistics:");
        result.add("  Interface-ID Options Added: " + relayStats.interfaceIdAdded);
        result.add("  Subscriber-ID Options Added: " + relayStats.subscriberIdAdded);
        result.add("  Relay Message Options Added: " + relayStats.relayMessageAdded);
        result.add("  Total Options Added: " + (relayStats.interfaceIdAdded + relayStats.subscriberIdAdded + relayStats.relayMessageAdded));
        result.add("");

        // Sub-option statistics (IPv6 specific)
        result.add("Sub-option Statistics:");
        result.add("  Interface-ID Added: " + relayStats.interfaceIdAdded);
        result.add("  Subscriber-ID Added: " + relayStats.subscriberIdAdded);
        result.add("  Relay Message Added: " + relayStats.relayMessageAdded);
        result.add("");

        // Performance metrics
        result.add("Performance Metrics:");
        if (relayStats.packetProcessingCount > 0) {
            result.add("  Total Packets Processed: " + relayStats.packetProcessingCount);
            result.add("  Total Processing Time: " + relayStats.totalProcessingTime);
            result.add("  Average Processing Time: " + relayStats.getAverageProcessingTime());
            result.add("  Maximum Processing Time: " + relayStats.maxProcessingTime);
            result.add("  Minimum Processing Time: " + (relayStats.minProcessingTime == Long.MAX_VALUE ? 0 : relayStats.minProcessingTime));

            // Calculate packets per second (rough estimate)
            long uptimeSeconds = (bits.getTime() - statsResetTime) / 1000;
            if (uptimeSeconds > 0) {
                long packetsPerSecond = relayStats.packetProcessingCount / uptimeSeconds;
                result.add("  Packets Per Second: " + packetsPerSecond + " pps");
            } else {
                result.add("  Packets Per Second: 0 pps");
            }
        } else {
            result.add("  Total Packets Processed: 0");
            result.add("  Total Processing Time: 0");
            result.add("  Average Processing Time: 0");
            result.add("  Maximum Processing Time: 0");
            result.add("  Minimum Processing Time: 0");
            result.add("  Packets Per Second: 0 pps");
        }
        result.add("");

        // Error statistics
        result.add("Error Statistics:");
        result.add("  Invalid GIADDR Errors: 0");  // IPv6 doesn't have GIADDR
        result.add("  Routing Errors: " + relayStats.routingErrors);
        result.add("  Option Parsing Errors: " + relayStats.optionParsingErrors);
        result.add("  Buffer Overflow Errors: " + relayStats.bufferOverflowErrors);
        result.add("");

        // Summary with percentages
        long totalPackets = relayStats.packetsClientToServer + relayStats.packetsServerToClient;
        result.add("Summary:");
        if (totalPackets > 0) {
            result.add("  Client to Server: " + relayStats.packetsClientToServer + " ("
                    + String.format("%.1f%%", (relayStats.packetsClientToServer * 100.0) / totalPackets) + ")");
            result.add("  Server to Client: " + relayStats.packetsServerToClient + " ("
                    + String.format("%.1f%%", (relayStats.packetsServerToClient * 100.0) / totalPackets) + ")");
            result.add("  Success Rate: " + String.format("%.1f%%", (relayStats.packetsForwarded * 100.0) / totalPackets));
            result.add("  Drop Rate: " + String.format("%.1f%%", (relayStats.packetsDropped * 100.0) / totalPackets));
        } else {
            result.add("  Client to Server: 0 (0.0%)");
            result.add("  Server to Client: 0 (0.0%)");
            result.add("  Success Rate: 0.0%");
            result.add("  Drop Rate: 0.0%");
        }
        result.add("");

        // Statistics reset time
        result.add("Statistics Reset: " + bits.time2str(cfgAll.timeZoneName, statsResetTime, 3));

        return result;
    }

    /**
     * reset relay statistics
     */
    public void resetRelayStatistics() {
        relayStats.reset();
        statsResetTime = bits.getTime();
    }

    /**
     * Add interface to relay interfaces list
     *
     * @param iface interface
     */
    public synchronized void addRelayInterface(cfgIfc iface) {
        // Check if already in list
        for (int i = 0; i < relayInterfaces.size(); i++) {
            cfgIfc existing = relayInterfaces.get(i);
            if (existing == null) {
                continue;
            }
            if (existing.name.equals(iface.name)) {
                if (debugger.servDhcp6traf) {
                    logger.debug("dhcp6 relay:  interface " + iface.name + " already in relay list");
                }
                return;
            }
        }
        srvDeinit();
        relayInterfaces.add(iface);
        srvInit();
        if (debugger.servDhcp6traf) {
            logger.info("dhcp6 relay:  added interface " + iface.name + " to relay list. total interfaces: " + relayInterfaces.size());
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
            if (debugger.servDhcp6traf) {
                logger.info("dhcp6 relay: removed interface " + iface.name + " from relay list. total interfaces: " + relayInterfaces.size());
            }
        } else {
            if (debugger.servDhcp6traf) {
                logger.debug("dhcp6 relay: interface " + iface.name + " was not in relay list");
            }
        }
    }

    /**
     * Forward packet to server from specific interface
     */
    private boolean forwardToServerFromInterface(packHolder pck, addrIP serverAddr, packDhcp6 dhcp, prtGenConn id, cfgIfc sourceIface) {
        try {
            if (debugger.servDhcp6traf) {
                logger.info("dhcp6 forwardToServerFromInterface: creating relay-forward message for server " + serverAddr + " from interface " + sourceIface.name);
            }
            // Create relay-forward message with specific interface
            packHolder relayPck = createRelayForwardFromInterface(pck, dhcp, id, sourceIface);
            if (relayPck == null) {
                if (debugger.servDhcp6traf) {
                    logger.error("dhcp6 forwardToServerFromInterface: failed to create relay-forward message");
                }
                relayStats.routingErrors++;
                return false;
            }

            if (debugger.servDhcp6traf) {
                logger.info("dhcp6 forwardToServerFromInterface: created relay packet, size=" + relayPck.dataSize());
            }

            // Send to server using the correct source interface
            prtGenConn conn = sourceIface.vrfFor.udp6.packetConnect(this, sourceIface.fwdIf6, packDhcp6.portSnum, serverAddr, packDhcp6.portSnum, srvName(), -1, null, -1, -1);
            if (conn == null) {
                if (debugger.servDhcp6traf) {
                    logger.error("dhcp6 forwardToServerFromInterface: failed to create connection to server " + serverAddr + " from interface " + sourceIface.name);
                }
                relayStats.routingErrors++;
                return false;
            }

            if (debugger.servDhcp6traf) {
                logger.info("dhcp6 forwardToServerFromInterface: sending packet to server " + serverAddr + " from interface " + sourceIface.name + ", size=" + relayPck.dataSize());
            }
            conn.send2net(relayPck);
            conn.setClosing();
            return true;

        } catch (Exception e) {
            logger.traceback(e);
            relayStats.routingErrors++;
            return false;
        }
    }

    /**
     * Forward packet to client using specific interface
     */
    private boolean forwardToClientFromInterface(packHolder pck, packDhcp6 dhcp, cfgIfc sourceIface) {
        try {
            // Extract original message from relay-reply
            packHolder clientPck = extractRelayMessage(pck);
            if (clientPck == null) {
                relayStats.optionParsingErrors++;
                relayStats.packetsDropped++;
                return false;
            }

            int extractedMsgType = clientPck.getByte(0) & 0xff;
            if (debugger.servDhcp6traf) {
                logger.info("dhcp6 forwardToClientFromInterface: extracted client packet, size=" + clientPck.dataSize());
                logger.info("dhcp6 forwardToClientFromInterface: client packet type=" + extractedMsgType);
            }

            // Check if the extracted message is another relay-reply (nested relay)
            if (extractedMsgType == packDhcp6.typReRep) {
                if (debugger.servDhcp6traf) {
                    logger.info("dhcp6 forwardToClientFromInterface: extracted message is a relay-reply, forwarding to next relay agent");
                }

                // This is a nested relay-reply - forward it to the peer address from the OUTER relay-reply
                // The peer address in the outer relay-reply is the address of the next relay agent (R1)
                addrIPv6 peerAddr = extractPeerAddress(pck); // Peer address from OUTER relay-reply
                if (peerAddr == null) {
                    if (debugger.servDhcp6traf) {
                        logger.error("dhcp6 forwardToClientFromInterface: failed to extract peer address for nested relay");
                    }
                    relayStats.optionParsingErrors++;
                    relayStats.packetsDropped++;
                    return false;
                }

                if (debugger.servDhcp6traf) {
                    logger.info("dhcp6 forwardToClientFromInterface: forwarding nested relay-reply to next relay agent at " + peerAddr);
                }

                // Send the nested relay-reply to the peer (which should be the next relay agent)
                addrIP peerAddrIP = new addrIP();
                peerAddrIP.fromIPv6addr(peerAddr);
                prtGenConn conn = sourceIface.vrfFor.udp6.packetConnect(this, sourceIface.fwdIf6, packDhcp6.portSnum, peerAddrIP, packDhcp6.portSnum, srvName(), -1, null, -1, -1);
                if (conn == null) {
                    if (debugger.servDhcp6traf) {
                        logger.error("dhcp6 forwardToClientFromInterface: failed to create connection to next relay agent " + peerAddrIP);
                    }
                    relayStats.routingErrors++;
                    relayStats.packetsDropped++;
                    return false;
                }

                if (debugger.servDhcp6traf) {
                    logger.info("dhcp6 forwardToClientFromInterface: sending nested relay-reply to next relay agent " + peerAddrIP + ", size=" + clientPck.dataSize());
                }

                conn.send2net(clientPck); // Send the nested relay-reply
                conn.setClosing();
                relayStats.packetsForwarded++;

                if (debugger.servDhcp6traf) {
                    logger.info("dhcp6 forwardToClientFromInterface: successfully forwarded nested relay-reply to next relay agent");
                }

                return true;
            }

            // Extract peer address and send to client
            addrIPv6 peerAddr = extractPeerAddress(pck);
            if (peerAddr == null) {
                relayStats.optionParsingErrors++;
                relayStats.packetsDropped++;
                return false;
            }

            // Send to client using the correct source interface
            addrIP peerAddrIP = new addrIP();
            peerAddrIP.fromIPv6addr(peerAddr);
            prtGenConn conn = sourceIface.vrfFor.udp6.packetConnect(this, sourceIface.fwdIf6, packDhcp6.portSnum, peerAddrIP, packDhcp6.portCnum, srvName(), -1, null, -1, -1);
            if (conn == null) {
                if (debugger.servDhcp6traf) {
                    logger.error("dhcp6 forwardToClientFromInterface: failed to create connection to client " + peerAddrIP + " from interface " + sourceIface.name);
                }
                relayStats.routingErrors++;
                relayStats.packetsDropped++;
                return false;
            }

            if (debugger.servDhcp6traf) {
                logger.info("dhcp6 forwardToClientFromInterface: sending response to client " + peerAddrIP + " from interface " + sourceIface.name);
            }
            conn.send2net(clientPck);
            conn.setClosing();
            relayStats.packetsForwarded++;

            return true;
        } catch (Exception e) {
            logger.traceback(e);
            relayStats.routingErrors++;
            relayStats.packetsDropped++;
            return false;
        }
    }

    /**
     * Create relay forward message with specific interface
     */
    private packHolder createRelayForwardFromInterface(packHolder originalPck, packDhcp6 dhcp, prtGenConn id, cfgIfc sourceIface) {
        try {
            // Create new packet for relay-forward message
            packHolder relayPck = new packHolder(true, true);

            // Message type (1 byte) = 12 (RELAY-FORW)
            relayPck.putByte(0, packDhcp6.typReReq);

            // Determine the hop count
            int hopCount = 0;
            if (dhcp.msgTyp == packDhcp6.typReReq) {
                // Relay-Forward Message - extract and increment
                hopCount = extractHopCount(originalPck) + 1;
            } else {
                // Client Message - start with 0 (per RFC 8415)
                hopCount = 0;
            }

            // Check maxHopCount
            if (hopCount >= maxHopCount) {
                // Discard message
                return null;
            }

            // Set the incremented hop count
            relayPck.putByte(1, hopCount);

            // Link address (16 bytes) - address of link on which client is located
            if (sourceIface != null && sourceIface.addr6 != null) {
                byte[] linkAddrBytes = new byte[16];
                sourceIface.addr6.toBuffer(linkAddrBytes, 0);
                relayPck.putCopy(linkAddrBytes, 0, 2, 16);
            } else {
                // All zeros if no link address
                for (int i = 0; i < 16; i++) {
                    relayPck.putByte(2 + i, 0);
                }
            }

            // Peer address (16 bytes) - address of client or previous relay
            if (id.peerAddr != null) {
                addrIPv6 peerAddr = id.peerAddr.toIPv6();
                byte[] peerAddrBytes = new byte[16];
                peerAddr.toBuffer(peerAddrBytes, 0);
                relayPck.putCopy(peerAddrBytes, 0, 18, 16);
            } else {
                // All zeros if no peer address
                for (int i = 0; i < 16; i++) {
                    relayPck.putByte(18 + i, 0);
                }
            }

            relayPck.putSkip(34); // Header is 34 bytes

            // Add interface-id option if enabled
            if (useInterfaceId && sourceIface != null) {
                String ifName = sourceIface.name;
                byte[] ifNameBytes = ifName.getBytes();
                relayPck.msbPutW(0, D6O_INTERFACE_ID); // Option type
                relayPck.msbPutW(2, ifNameBytes.length); // Length
                relayPck.putCopy(ifNameBytes, 0, 4, ifNameBytes.length);
                relayPck.putSkip(4 + ifNameBytes.length);
                relayStats.interfaceIdAdded++;
            }

            // Add subscriber-id option if configured
            if (subscriberId != null && !subscriberId.isEmpty()) {
                byte[] subIdBytes = subscriberId.getBytes();
                relayPck.msbPutW(0, D6O_SUBSCRIBER_ID); // Option type
                relayPck.msbPutW(2, subIdBytes.length);  // Length
                relayPck.putCopy(subIdBytes, 0, 4, subIdBytes.length);
                relayPck.putSkip(4 + subIdBytes.length);
                relayStats.subscriberIdAdded++;
            }

            // Add relay message option containing original packet
            relayPck.msbPutW(0, D6O_RELAY_MSG);      // Option type
            relayPck.msbPutW(2, originalPck.dataSize()); // Length
            byte[] originalData = new byte[originalPck.dataSize()];
            originalPck.getCopy(originalData, 0, 0, originalPck.dataSize());
            relayPck.putCopy(originalData, 0, 4, originalPck.dataSize());
            relayPck.putSkip(4 + originalPck.dataSize());
            relayStats.relayMessageAdded++;

            relayPck.merge2beg();
            return relayPck;

        } catch (Exception e) {
            logger.traceback(e);
            relayStats.bufferOverflowErrors++;
            return null;
        }
    }

    /**
     * Find interface by IPv6 address - simple direct lookup in relay interfaces
     */
    private cfgIfc findInterfaceByAddress(addrIPv6 linkAddr) {
        if (linkAddr == null) {
            return null;
        }

        addrIP targetAddr = new addrIP();
        targetAddr.fromIPv6addr(linkAddr);

        // Direct lookup in relay interfaces using their IP addresses
        for (int i = 0; i < relayInterfaces.size(); i++) {
            cfgIfc iface = relayInterfaces.get(i);
            if (iface == null) {
                continue;
            }

            // Check IPv6 address field
            if (iface.addr6 != null) {
                if (linkAddr.compareTo(iface.addr6) == 0) {
                    if (debugger.servDhcp6traf) {
                        logger.debug("dhcp6 relay: found interface " + iface.name + " for address " + linkAddr + " using iface.addr6");
                    }
                    return iface;
                }
            }

            // Check forwarding interface address
            if (iface.fwdIf6 != null && iface.fwdIf6.addr != null) {
                if (targetAddr.compareTo(iface.fwdIf6.addr) == 0) {
                    if (debugger.servDhcp6traf) {
                        logger.debug("dhcp6 relay: found interface " + iface.name + " for address " + linkAddr + " using fwdIf6.addr");
                    }
                    return iface;
                }
            }
        }

        if (debugger.servDhcp6traf) {
            logger.debug("dhcp6 relay: no interface found for address " + linkAddr + " in " + relayInterfaces.size() + " relay interfaces");
        }
        return null;
    }

    /**
     * Find interface by connection - determines which interface a packet came
     * from
     */
    private cfgIfc findInterfaceByConnection(prtGenConn conn) {
        if (conn == null || conn.iface == null) {
            return null;
        }

        // Search through relay interfaces to find matching forwarding interface
        for (int i = 0; i < relayInterfaces.size(); i++) {
            cfgIfc iface = relayInterfaces.get(i);
            if (iface == null) {
                continue;
            }
            if (iface.fwdIf6 != null && iface.fwdIf6 == conn.iface) {
                if (debugger.servDhcp6traf) {
                    logger.debug("dhcp6 findInterfaceByConnection: found interface " + iface.name + " for connection");
                }
                return iface;
            }
        }

        if (debugger.servDhcp6traf) {
            logger.debug("dhcp6 findInterfaceByConnection: no matching interface found for connection");
        }
        return null;
    }

}

class servDhcp6bindIp implements Comparator<servDhcp6bind> {

    public int compare(servDhcp6bind o1, servDhcp6bind o2) {
        return o1.ip.compareTo(o2.ip);
    }

}

class servDhcp6bind implements Comparable<servDhcp6bind> {

    public boolean confed = false;

    public addrIPv6 ip;

    public addrMac mac;

    public long reqd;

    public servDhcp6bind() {
        reqd = bits.getTime();
    }

    public String toString() {
        return ip + " " + mac;
    }

    public boolean fromString(cmds cmd) {
        ip = new addrIPv6();
        mac = new addrMac();
        if (ip.fromString(cmd.word())) {
            return true;
        }
        if (mac.fromString(cmd.word())) {
            return true;
        }
        return false;
    }

    public int compareTo(servDhcp6bind o) {
        return mac.compareTo(o.mac);
    }

}

class servDhcp6timer extends TimerTask {

    private servDhcp6 parent;

    public servDhcp6timer(servDhcp6 prnt) {
        parent = prnt;
    }

    public void run() {
        if (debugger.servDhcp6traf) {
            logger.debug("purging");
        }
        try {
            parent.doPurging();
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

}

class servDhcp6worker implements Runnable {

    private servDhcp6 parent;

    private pipeSide pipe;

    private prtGenConn conn;

    public servDhcp6worker(servDhcp6 prnt, pipeSide pip, prtGenConn id) {
        parent = prnt;
        pipe = pip;
        pipe.setTime(10000);
        conn = id;
        new Thread(this).start();
    }

    private void doer() {
        packHolder pck = pipe.readPacket(true);
        if (pck == null) {
            if (debugger.servDhcp6traf) {
                logger.info("dhcp6 server: got no packet");
            }
            return;
        }

        // Check packet size first
        if (pck.dataSize() < 4) {
            if (debugger.servDhcp6traf) {
                logger.error("dhcp6 server: packet too small: " + pck.dataSize());
            }
            return;
        }

        // Manual message type extraction for debugging
        int msgType = pck.getByte(0) & 0xff;
        if (debugger.servDhcp6traf) {
            logger.info("dhcp6 server: received packet, msgType=" + msgType + ", size=" + pck.dataSize());
        }

        // Special handling for Relay-Forward packets (RFC 3315 compliant)
        if (msgType == packDhcp6.typReReq) {
            if (debugger.servDhcp6traf) {
                logger.info("dhcp6 server: processing relay-forward packet");
            }
            packHolder relayReply = parent.createRelayReply(pck);
            if (relayReply == null) {
                return;
            }
            if (debugger.servDhcp6traf) {
                logger.info("dhcp6 server: sending rfc 3315 compliant relay-reply");
                logger.info("dhcp6 server: relay-reply size=" + relayReply.dataSize()
                        + ", message type=" + (relayReply.getByte(0) & 0xff)
                        + ", hop count=" + (relayReply.getByte(1) & 0xff));

                logger.info("dhcp6 server: connection info - peer=" + conn.peerAddr);
            }
            conn.send2net(relayReply);

            // Verify send operation
            if (debugger.servDhcp6traf) {
                logger.info("dhcp6 server: relay-reply sent to network");
            }
            return;
        }

        // Normal DHCP6 packet processing
        packDhcp6 pckd = new packDhcp6();
        if (pckd.parsePacket(pck)) {
            if (debugger.servDhcp6traf) {
                logger.error("dhcp6 server: failed to parse packet, msgType=" + msgType);
            }
            return;
        }
        if (debugger.servDhcp6traf) {
            logger.debug("rx " + pckd);
            logger.info("dhcp6 server: calling gotPack for msgType=" + pckd.msgTyp);
        }

        pckd = parent.gotPack(pckd);

        if (pckd == null) {
            if (debugger.servDhcp6traf) {
                logger.info("dhcp6 server: gotPack returned null, no response");
            }
            return;
        }

        if (debugger.servDhcp6traf) {
            logger.debug("tx " + pckd);
        }

        // Extract MAC address from client DUID for logging
        addrMac clientMac = packDhcp6.decodeDUID(pckd.clntId);

        if (debugger.servDhcp6traf) {
            if (clientMac != null) {
                logger.info("dhcp6 server: processing request from client MAC " + clientMac);
            }
            logger.info("dhcp6 server: sending response, msgType=" + pckd.msgTyp);
        }

        pckd.createPacket(pck, parent.options);
        conn.send2net(pck);

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

/**
 * DHCP6 relay statistics
 */
class servDhcp6RelayStats {

    // Packet counters
    public long packetsClientToServer = 0;
    public long packetsServerToClient = 0;
    public long packetsDropped = 0;
    public long packetsInvalid = 0;
    public long packetsForwarded = 0;

    // Hop count statistics
    public long maxHopCountExceeded = 0;
    public long hopCountTotal = 0;
    public long relayForwardPackets = 0;

    // Performance statistics
    public long totalProcessingTime = 0;
    public long packetProcessingCount = 0;
    public long maxProcessingTime = 0;
    public long minProcessingTime = Long.MAX_VALUE;

    // Error counters
    public long routingErrors = 0;
    public long optionParsingErrors = 0;
    public long bufferOverflowErrors = 0;

    // Option statistics
    public long interfaceIdAdded = 0;
    public long subscriberIdAdded = 0;
    public long relayMessageAdded = 0;

    /**
     * reset all statistics
     */
    public void reset() {
        packetsClientToServer = 0;
        packetsServerToClient = 0;
        packetsDropped = 0;
        packetsInvalid = 0;
        packetsForwarded = 0;
        maxHopCountExceeded = 0;
        hopCountTotal = 0;
        relayForwardPackets = 0;
        totalProcessingTime = 0;
        packetProcessingCount = 0;
        maxProcessingTime = 0;
        minProcessingTime = Long.MAX_VALUE;
        routingErrors = 0;
        optionParsingErrors = 0;
        bufferOverflowErrors = 0;
        interfaceIdAdded = 0;
        subscriberIdAdded = 0;
        relayMessageAdded = 0;
    }

    /**
     * update processing time statistics
     *
     * @param processingTime processing time in milliseconds
     */
    public void updateProcessingTime(long processingTime) {
        totalProcessingTime += processingTime;
        packetProcessingCount++;
        if (processingTime > maxProcessingTime) {
            maxProcessingTime = processingTime;
        }
        if (minProcessingTime == Long.MAX_VALUE || processingTime < minProcessingTime) {
            minProcessingTime = processingTime;
        }
    }

    /**
     * get average processing time
     *
     * @return average processing time in milliseconds
     */
    public long getAverageProcessingTime() {
        if (packetProcessingCount == 0) {
            return 0;
        }
        return totalProcessingTime / packetProcessingCount;
    }

    /**
     * get average hop count
     *
     * @return average hop count
     */
    public double getAverageHopCount() {
        if (relayForwardPackets == 0) {
            return 0.0;
        }
        return (double) hopCountTotal / relayForwardPackets;
    }
}
