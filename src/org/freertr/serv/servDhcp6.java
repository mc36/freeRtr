package org.freertr.serv;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Timer;
import java.util.TimerTask;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv6;
import org.freertr.addr.addrMac;
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
    private String subscriberId = null;

    /**
     * maximum hop count
     */
    private int maxHopCount = 10;

    /**
     * List of relay interfaces for multi-interface support
     */
    private List<cfgIfc> relayInterfaces = new ArrayList<cfgIfc>();

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
        if (mode == dhcpMode.server) {
            pipe.setTime(10000);
            new servDhcp6worker(this, pipe, id);
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
            cmds.cfgLine(l, !useInterfaceId, beg, "use-interface-id", "");
            cmds.cfgLine(l, subscriberId == null, beg, "subscriber-id", subscriberId);
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
            subscriberId = null;
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
                boolean isCreatingStaticBinding = ((hint != null) && (create == 1));

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
            if ((ntry.ip == null) && (gateway != null)) {
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
        rep.lifetimP = lease / 1000;
        rep.lifetimV = lease / 1000;
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
            if (gateway == null) {
                if (debugger.servDhcp6traf) {
                    logger.error("dhcp6 gotpack: refused to create default ip address for mac " + mac);
                }
                return null;
            }
            ntry.ip = addrIPv6.genPublic(mac, gateway);
            if (debugger.servDhcp6traf) {
                logger.info("dhcp6 gotpack: created new binding with ip " + ntry.ip + " for mac " + mac);
            }

            synchronized (bindings) {
                bindings.add(ntry);
                if (debugger.servDhcp6traf) {
                    logger.info("dhcp6 gotpack: added new binding to list, total bindings: " + bindings.size());
                }
            }
        } else {
            // Update request time
            ntry.reqd = bits.getTime();

            // Generate IP address if not already set
            if ((ntry.ip == null) && (gateway != null)) {
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

            if (!foundInList && (ntry.ip != null)) {
                bindings.add(ntry);
                if (debugger.servDhcp6traf) {
                    logger.info("dhcp6 gotpack: added missing binding to list, total bindings: " + bindings.size());
                }
            }

            if (srvIface != null) {
                addrIP adr = new addrIP();
                adr.fromIPv6addr(ntry.ip);
                srvIface.ipIf6.updateL2info(0, ntry.mac, adr);
            }

        }

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
     * Recursively create nested relay-reply structure
     *
     * @param orig original packet
     * @param pck original packet
     * @return reply packet, null if error
     */
    protected packDhcp6 createNestedRelayReply(packDhcp6 orig, packHolder pck) {
        if (debugger.servDhcp6traf) {
            logger.info("dhcp6 createNestedRelayReply: processing relay forward packet");
        }

        if (orig.relayed == null) {
            if (debugger.servDhcp6traf) {
                logger.error("dhcp6 createNestedRelayReply: not a relay-forward message");
            }
            return null;
        }

        pck.clear();
        pck.putCopy(orig.relayed, 0, 0, orig.relayed.length);
        pck.putSkip(orig.relayed.length);
        pck.merge2beg();

        packDhcp6 inner = new packDhcp6();
        if (inner.parsePacket(pck)) {
            if (debugger.servDhcp6traf) {
                logger.error("dhcp6 server: failed to parse packet");
            }
            return null;
        }
        if (debugger.servDhcp6traf) {
            logger.debug("rx " + inner);
        }

        if (inner.msgTyp == packDhcp6.typReReq) {
            inner = createNestedRelayReply(inner, pck);
        } else {
            inner = gotPack(inner);
        }

        if (inner == null) {
            if (debugger.servDhcp6traf) {
                logger.error("dhcp6 createNestedRelayReply: could not create server response");
            }
            return null;
        }

        if (debugger.servDhcp6traf) {
            logger.debug("tx " + inner);
        }
        pck.clear();
        inner.createPacket(pck, options);

        orig.msgTyp = packDhcp6.typReRep;
        orig.relayed = pck.getCopy();

        return orig;

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
        long startTime = bits.getTime();
        id.setClosing();
        if (mode != dhcpMode.relay) {
            return false;
        }

        try {
            packDhcp6 dhcp = new packDhcp6();
            byte[] buf = pck.getCopy();
            if (dhcp.parsePacket(pck)) {
                if (debugger.servDhcp6traf) {
                    logger.error("dhcp6 server: failed to parse packet");
                }
                return false;
            }
            if (debugger.servDhcp6traf) {
                logger.debug("rx " + dhcp);
            }

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
                    if (debugger.servDhcp6traf) {
                        logger.info("dhcp6 relayClientToServer: processing client packet, msgType=" + dhcp.msgTyp + ", upstreamServers=" + helperAddresses.size());
                    }
                    int hops;
                    if (dhcp.msgLink == null) {
                        hops = 0;
                    } else {
                        hops = dhcp.msgHop + 1;
                    }
                    if (hops >= maxHopCount) {
                        if (debugger.servDhcp6traf) {
                            logger.error("dhcp6 relayClientToServer: max hop count exceeded: " + hops);
                        }
                        return false;
                    }
                    dhcp = new packDhcp6();
                    dhcp.msgHop = hops;
                    dhcp.msgTyp = packDhcp6.typReReq;
                    dhcp.relayed = buf;
                    dhcp.msgLink = id.iface.addr.toIPv6();
                    dhcp.msgPeer = id.peerAddr.toIPv6();
                    if (useInterfaceId) {
                        dhcp.ifcId = "" + id.iface;
                    }
                    if (subscriberId != null) {
                        dhcp.subId = subscriberId;
                    }
                    for (int i = 0; i < helperAddresses.size(); i++) {
                        addrIP serverAddr = helperAddresses.get(i);
                        if (debugger.servDhcp6traf) {
                            logger.info("dhcp6 relayClientToServer: forwarding to server " + serverAddr);
                        }
                        pck.clear();
                        dhcp.createPacket(pck, options);
                        pck.merge2beg();
                        prtGenConn conn = srvVrf.getUdp(serverAddr).packetConnect(this, id.iface, packDhcp6.portSnum, serverAddr, packDhcp6.portSnum, srvName(), -1, null, -1, -1);
                        if (conn == null) {
                            if (debugger.servDhcp6traf) {
                                logger.error("dhcp6 forwardToServerFromInterface: failed to create connection to server " + serverAddr);
                            }
                            continue;
                        }
                        conn.send2net(pck);
                        conn.setClosing();
                    }
                    return false;

                case packDhcp6.typAdvertise:
                case packDhcp6.typReply:
                case packDhcp6.typReconfig:
                case packDhcp6.typReRep:
                    if (dhcp.msgTyp != packDhcp6.typReRep) {
                        if (debugger.servDhcp6traf) {
                            logger.info("dhcp6 relayServerToClient: dropping non-relay-reply message, msgType=" + dhcp.msgTyp);
                        }
                        return false;
                    }
                    if (dhcp.relayed == null) {
                        if (debugger.servDhcp6traf) {
                            logger.error("dhcp6 createNestedRelayReply: not a relay-forward message");
                        }
                        return false;
                    }
                    pck.clear();
                    pck.putCopy(dhcp.relayed, 0, 0, dhcp.relayed.length);
                    pck.putSkip(dhcp.relayed.length);
                    pck.merge2beg();
                    if (dhcp.msgPeer == null) {
                        return false;
                    }
                    addrIP peerAddrIP = new addrIP();
                    peerAddrIP.fromIPv6addr(dhcp.msgPeer);
                    packDhcp6 inner = new packDhcp6();
                    if (inner.parsePacket(pck)) {
                        if (debugger.servDhcp6traf) {
                            logger.error("dhcp6 server: failed to parse packet");
                        }
                        return false;
                    }
                    if (debugger.servDhcp6traf) {
                        logger.debug("rx " + inner);
                    }
                    pck.clear();
                    pck.putCopy(dhcp.relayed, 0, 0, dhcp.relayed.length);
                    pck.putSkip(dhcp.relayed.length);
                    pck.merge2beg();
                    prtGenConn conn;
                    if (inner.msgTyp == packDhcp6.typReRep) {
                        conn = srvVrf.getUdp(peerAddrIP).packetConnect(this, id.iface, packDhcp6.portSnum, peerAddrIP, packDhcp6.portSnum, srvName(), -1, null, -1, -1);
                    } else {
                        conn = srvVrf.getUdp(peerAddrIP).packetConnect(this, id.iface, packDhcp6.portSnum, peerAddrIP, packDhcp6.portCnum, srvName(), -1, null, -1, -1);
                    }
                    if (conn == null) {
                        if (debugger.servDhcp6traf) {
                            logger.error("dhcp6 forwardToClientFromInterface: failed to create connection to client " + peerAddrIP);
                        }
                        return false;
                    }
                    if (debugger.servDhcp6traf) {
                        logger.info("dhcp6 forwardToClientFromInterface: sending response to client " + peerAddrIP);
                    }
                    conn.send2net(pck);
                    conn.setClosing();
                    return false;

                default:
                    if (debugger.servDhcp6traf) {
                        logger.error("dhcp6 relayPacket: unknown message type: " + dhcp.msgTyp);
                    }
                    return false;
            }
        } catch (Exception e) {
            logger.traceback(e);
            return false;
        }
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

        packDhcp6 pckd = new packDhcp6();
        if (pckd.parsePacket(pck)) {
            if (debugger.servDhcp6traf) {
                logger.error("dhcp6 server: failed to parse packet");
            }
            return;
        }
        if (debugger.servDhcp6traf) {
            logger.debug("rx " + pckd);
        }

        if (pckd.msgTyp == packDhcp6.typReReq) {
            pckd = parent.createNestedRelayReply(pckd, pck);
        } else {
            pckd = parent.gotPack(pckd);
        }

        if (pckd == null) {
            return;
        }
        if (debugger.servDhcp6traf) {
            logger.debug("tx " + pckd);
        }
        pck.clear();
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
