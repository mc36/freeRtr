package serv;

import addr.addrIP;
import addr.addrIPv4;
import addr.addrMac;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Timer;
import java.util.TimerTask;
import pack.packDhcp4;
import pack.packDhcpOption;
import pack.packHolder;
import pipe.pipeLine;
import pipe.pipeSide;
import prt.prtGenConn;
import prt.prtServS;
import tab.tabGen;
import user.userFilter;
import user.userHelping;
import util.bits;
import util.cmds;
import util.debugger;
import util.logger;

/**
 * dynamic host config protocol (rfc2131) server
 *
 * @author matecsaba
 */
public class servDhcp4 extends servGeneric implements prtServS {

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
    public int lease = 12 * 60 * 60;

    /**
     * renew time
     */
    public int renew = lease / 2;

    /**
     * options to add
     */
    public tabGen<packDhcpOption> options = new tabGen<packDhcpOption>();

    private Timer purgeTimer;

    private List<servDhcp4binding> bindings = new ArrayList<servDhcp4binding>();

    /**
     * defaults text
     */
    public final static String defaultL[] = {
        "server dhcp4 .*! port " + packDhcp4.portSnum,
        "server dhcp4 .*! protocol " + proto2string(protoIp4 + protoUdp),
        "server dhcp4 .*! boot-server ",
        "server dhcp4 .*! boot-file ",
        "server dhcp4 .*! lease 43200",
        "server dhcp4 .*! renew 21600"
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public tabGen<userFilter> srvDefFlt() {
        return defaultF;
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.timeout = 10000;
        new servDhcp4worker(this, pipe, id);
        return false;
    }

    public void srvShRun(String beg, List<String> l) {
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
        synchronized (bindings) {
            for (int i = 0; i < bindings.size(); i++) {
                servDhcp4binding ntry = bindings.get(i);
                if (!ntry.confed) {
                    continue;
                }
                l.add(beg + "static " + ntry.mac + " " + ntry.ip);
            }
        }
        for (int o = 0; o < options.size(); o++) {
            l.add(beg + "option " + options.get(o));
        }
    }

    public boolean srvCfgStr(cmds cmd) {
        String a = cmd.word();
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
            if (poolHi.compare(poolLo, poolHi) >= 0) {
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
        if (a.equals("static")) {
            addrMac mac = new addrMac();
            addrIPv4 ip = new addrIPv4();
            if (mac.fromString(cmd.word())) {
                return true;
            }
            if (ip.fromString(cmd.word())) {
                return true;
            }
            servDhcp4binding ntry = findBinding(mac, 1);
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
        if (!a.equals("no")) {
            return true;
        }
        a = cmd.word();
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
        if (a.equals("static")) {
            addrMac mac = new addrMac();
            if (mac.fromString(cmd.word())) {
                return true;
            }
            findBinding(mac, 2);
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

    public void srvHelp(userHelping l) {
        l.add("1 2  pool                   address pool to use");
        l.add("2 3    <addr>               first address to delegate");
        l.add("3 .      <addr>             last address to delegate");
        l.add("1 2  gateway                gateway address to delegate");
        l.add("2 .    <addr>               address of gateway");
        l.add("1 2  dns-server             address(es) of name server(s) to delegate");
        l.add("2 3,.  <addr>               dns#1 server address");
        l.add("3 .      <addr>             dns#2 server address");
        l.add("1 2  boot-server            address of tftp server to delegate");
        l.add("2 .    <name>               dns server address");
        l.add("1 2  boot-file              path of tftp file to delegate");
        l.add("2 .    <name>               dns server address");
        l.add("1 2  domain-name            domain name to delegate");
        l.add("2 .    <name>               domain name");
        l.add("1 2  lease                  lease time to delegate");
        l.add("2 .    <sec>                lease time in seconds");
        l.add("1 2  renew                  renew time to delegate");
        l.add("2 .    <sec>                renew time in seconds");
        l.add("1 2  netmask                network to delegate");
        l.add("2 .    <mask>               netmask to delegate");
        l.add("1 2  static                 address pool to use");
        l.add("2 3    <addr>               mac address of client");
        l.add("3 .      <addr>             ip address of client");
        l.add("1 2  option                 specify custom option");
        l.add("2 3,.  <num>                type of option");
        l.add("3 3,.    <num>              data byte");
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
        if (srvIface == null) {
            return true;
        }
        restartTimer(false);
        return genStrmStart(this, new pipeLine(32768, true), 0);
    }

    public boolean srvDeinit() {
        restartTimer(true);
        return genericStop(0);
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

    private servDhcp4binding findBinding(addrMac mac, int create) {
        synchronized (bindings) {
            servDhcp4binding ntry = new servDhcp4binding();
            Collections.sort(bindings, new servDhcp4bindingMac());
            ntry.mac = mac.copyBytes();
            int i = Collections.binarySearch(bindings, ntry, new servDhcp4bindingMac());
            if (i >= 0) {
                ntry = bindings.get(i);
                if ((create == 3) && (!ntry.confed)) {
                    create--;
                }
                if (create == 2) {
                    bindings.remove(i);
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
            Collections.sort(bindings, new servDhcp4bindingIp());
            for (int cnt = 0; cnt < 64; cnt++) {
                addrIPv4 ip = new addrIPv4();
                ip.fillRandom();
                ip.setAnd(ip, poolMsk);
                ip.setAdd(ip, poolLo);
                if (ip.compare(ip, poolLo) < 0) {
                    continue;
                }
                if (ip.compare(ip, poolHi) > 0) {
                    continue;
                }
                ntry.ip = ip;
                i = Collections.binarySearch(bindings, ntry, new servDhcp4bindingIp());
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

    private synchronized boolean sendPack(packDhcp4 pckd, servDhcp4binding ntry) {
        addrIP adr = new addrIP();
        if (pckd.bootpBroadcast) {
            adr.fromIPv4addr(addrIPv4.getBroadcast());
        } else {
            adr.fromIPv4addr(ntry.ip);
        }
        if (debugger.servDhcp4traf) {
            logger.debug("tx " + adr + " " + pckd);
        }
        srvIface.ipIf4.updateL2info(0, ntry.mac, adr);
        pipeSide pip = srvVrf.udp4.streamConnect(new pipeLine(32768, true), srvIface.fwdIf4, packDhcp4.portSnum, adr, packDhcp4.portCnum, srvName(), null, -1);
        if (pip == null) {
            return true;
        }
        pip.wait4ready(1000);
        pip.timeout = 1000;
        packHolder pckh = new packHolder(true, true);
        pckd.createHeader(pckh, options);
        pckh.merge2end();
        pckh.pipeSend(pip, 0, pckh.dataSize(), 2);
        pip.setClose();
        return false;
    }

    private void updatePack(packDhcp4 req, packDhcp4 rep, servDhcp4binding ntry) {
        rep.bootpOp = packDhcp4.bootpOpReply;
        rep.bootpXid = req.bootpXid;
        rep.bootpSecs = req.bootpSecs;
        rep.bootpBroadcast = req.bootpBroadcast;
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
        rep.dhcpLeaseTime = lease;
        rep.dhcpRenewTime = renew;
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
        servDhcp4binding ntry;
        packDhcp4 rep = new packDhcp4();
        switch (req.dhcpOp) {
            case packDhcp4.dhcpOpDiscover:
                ntry = findBinding(req.bootpChaddr, 1);
                if (ntry == null) {
                    return null;
                }
                rep.dhcpOp = packDhcp4.dhcpOpOffer;
                updatePack(req, rep, ntry);
                sendPack(rep, ntry);
                return rep;
            case packDhcp4.dhcpOpRequest:
                ntry = findBinding(req.bootpChaddr, 1);
                if (ntry == null) {
                    return null;
                }
                ntry.reqd = bits.getTime();
                rep.dhcpOp = packDhcp4.dhcpOpAck;
                updatePack(req, rep, ntry);
                sendPack(rep, ntry);
                return rep;
            case packDhcp4.dhcpOpRelease:
                ntry = findBinding(req.bootpChaddr, 3);
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
            for (int i = 0; i < bindings.size(); i++) {
                servDhcp4binding ntry = bindings.get(i);
                if (ntry == null) {
                    continue;
                }
                if (ntry.confed) {
                    continue;
                }
                if ((cur - ntry.reqd) < (lease * 1000)) {
                    continue;
                }
                if (debugger.servDhcp4traf) {
                    logger.debug("delete " + ntry);
                }
                bindings.remove(i);
            }
        }
    }

}

class servDhcp4bindingIp implements Comparator<servDhcp4binding> {

    public int compare(servDhcp4binding o1, servDhcp4binding o2) {
        return o1.ip.compare(o1.ip, o2.ip);
    }

}

class servDhcp4bindingMac implements Comparator<servDhcp4binding> {

    public int compare(servDhcp4binding o1, servDhcp4binding o2) {
        return o1.mac.compare(o1.mac, o2.mac);
    }

}

class servDhcp4binding {

    public boolean confed = false;

    public addrIPv4 ip;

    public addrMac mac;

    public long reqd;

    public servDhcp4binding() {
        reqd = bits.getTime();
    }

    public String toString() {
        return ip + " " + mac;
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
        pipe.timeout = 10000;
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
        pckd.createHeader(pck, parent.options);
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
