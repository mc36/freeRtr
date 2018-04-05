package serv;

import addr.addrIP;
import addr.addrIPv6;
import addr.addrMac;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Timer;
import java.util.TimerTask;
import pack.packDhcp6;
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
 * dynamic host config protocol (rfc3315) server
 *
 * @author matecsaba
 */
public class servDhcp6 extends servGeneric implements prtServS {

    /**
     * dns1
     */
    public addrIPv6 dns1;

    /**
     * dns2
     */
    public addrIPv6 dns2;

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
    public int lease = 12 * 60 * 60;

    /**
     * renew time
     */
    public int renew = lease / 2;

    /**
     * options to add
     */
    public tabGen<packDhcpOption> options = new tabGen<packDhcpOption>();

    private List<servDhcp6binding> bindings = new ArrayList<servDhcp6binding>();

    private Timer purgeTimer;

    /**
     * defaults text
     */
    public final static String defaultL[] = {
        "server dhcp6 .*! port " + packDhcp6.portSnum,
        "server dhcp6 .*! protocol " + proto2string(protoIp6 + protoUdp),
        "server dhcp6 .*! boot-url ",
        "server dhcp6 .*! preference 0",
        "server dhcp6 .*! lease 43200",
        "server dhcp6 .*! renew 21600"
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
        new servDhcp6worker(this, pipe, id);
        return false;
    }

    public void srvShRun(String beg, List<String> l) {
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
        l.add(beg + "boot-url " + bootUrl);
        l.add(beg + "domain-name " + domNam);
        l.add(beg + "lease " + lease);
        l.add(beg + "renew " + renew);
        l.add(beg + "preference " + prefer);
        synchronized (bindings) {
            for (int i = 0; i < bindings.size(); i++) {
                servDhcp6binding ntry = bindings.get(i);
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
        if (a.equals("preference")) {
            prefer = bits.str2num(cmd.word());
            return false;
        }
        if (a.equals("static")) {
            addrMac mac = new addrMac();
            addrIPv6 ip = new addrIPv6();
            if (mac.fromString(cmd.word())) {
                return true;
            }
            if (ip.fromString(cmd.word())) {
                return true;
            }
            servDhcp6binding ntry = findBinding(mac, 1);
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
        l.add("1 2  dns-server             address(es) of name server(s) to delegate");
        l.add("2 3,.  <addr>               dns#1 server address");
        l.add("3 .      <addr>             dns#2 server address");
        l.add("1 2  boot-url               url to boot file");
        l.add("2 .    <name>               url");
        l.add("1 2  domain-name            domain name to delegate");
        l.add("2 .    <name>               domain name");
        l.add("1 2  lease                  lease time to delegate");
        l.add("2 .    <sec>                lease time in seconds");
        l.add("1 2  renew                  renew time to delegate");
        l.add("2 .    <sec>                renew time in seconds");
        l.add("1 2  preference             server preference value");
        l.add("2 .    <num>                preference value");
        l.add("1 2  static                 address pool to use");
        l.add("2 3    <addr>               mac address of client");
        l.add("3 .      <addr>             ip address of client");
        l.add("1 2  option                 specify custom option");
        l.add("2 3,.  <num>                type of option");
        l.add("3 3,.    <num>              data byte");
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
        servDhcp6timer task = new servDhcp6timer(this);
        purgeTimer.schedule(task, 1000, 60000);
    }

    private servDhcp6binding findBinding(addrMac mac, int create) {
        synchronized (bindings) {
            servDhcp6binding ntry = new servDhcp6binding();
            Collections.sort(bindings, new servDhcp6bindingMac());
            ntry.mac = mac.copyBytes();
            int i = Collections.binarySearch(bindings, ntry, new servDhcp6bindingMac());
            if (i < 0) {
                if (create != 1) {
                    return null;
                }
                bindings.add(ntry);
                return ntry;
            }
            ntry = bindings.get(i);
            if ((create == 3) && (!ntry.confed)) {
                create--;
            }
            if (create == 2) {
                bindings.remove(i);
            }
            return ntry;
        }
    }

    private synchronized boolean sendPack(packDhcp6 pckd, servDhcp6binding ntry) {
        addrIP adr = new addrIP();
        adr.fromIPv6addr(ntry.ip);
        if (debugger.servDhcp6traf) {
            logger.debug("tx " + adr + " " + pckd);
        }
        srvIface.ipIf6.updateL2info(0, ntry.mac, adr);
        pipeSide pip = srvVrf.udp6.streamConnect(new pipeLine(32768, true), srvIface.fwdIf6, packDhcp6.portSnum, adr, packDhcp6.portCnum, srvName(), null, -1);
        if (pip == null) {
            return true;
        }
        pip.wait4ready(1000);
        pip.timeout = 1000;
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
        rep.iat1 = renew;
        rep.iat2 = lease;
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
        rep.servAddr = srvIface.addr6.copyBytes();
        int crt = 1;
        switch (req.msgTyp) {
            case packDhcp6.typSolicit:
                rep.msgTyp = packDhcp6.typAdvertise;
                break;
            case packDhcp6.typInfo:
                rep.msgTyp = packDhcp6.typReply;
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
            default:
                return null;
        }
        rep.status = 1;
        addrMac mac = packDhcp6.decodeDUID(req.clntId);
        if (mac == null) {
            return rep;
        }
        servDhcp6binding ntry = findBinding(mac, crt);
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
            return rep;
        }
        if (ntry.ip == null) {
            ntry.ip = addrIPv6.genPublic(mac, srvIface.addr6);
        }
        rep.ipaddr = ntry.ip.copyBytes();
        rep.ipsize = rep.ipaddr.maxBits();
        rep.status = 0;
        sendPack(rep, ntry);
        return rep;
    }

    /**
     * purge binding table
     */
    protected void doPurging() {
        synchronized (bindings) {
            long cur = bits.getTime();
            for (int i = 0; i < bindings.size(); i++) {
                servDhcp6binding ntry = bindings.get(i);
                if (ntry == null) {
                    continue;
                }
                if (ntry.confed) {
                    continue;
                }
                if ((cur - ntry.reqd) < (lease * 1000)) {
                    continue;
                }
                if (debugger.servDhcp6traf) {
                    logger.debug("delete " + ntry);
                }
                bindings.remove(i);
            }
        }
    }

}

class servDhcp6bindingIp implements Comparator<servDhcp6binding> {

    public int compare(servDhcp6binding o1, servDhcp6binding o2) {
        return o1.ip.compare(o1.ip, o2.ip);
    }

}

class servDhcp6bindingMac implements Comparator<servDhcp6binding> {

    public int compare(servDhcp6binding o1, servDhcp6binding o2) {
        return o1.mac.compare(o1.mac, o2.mac);
    }

}

class servDhcp6binding {

    public boolean confed = false;

    public addrIPv6 ip;

    public addrMac mac;

    public long reqd;

    public servDhcp6binding() {
        reqd = bits.getTime();
    }

    public String toString() {
        return ip + " " + mac;
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
        packDhcp6 pckd = new packDhcp6();
        if (pckd.parsePacket(pck)) {
            logger.info("got bad packet");
            return;
        }
        if (debugger.servDhcp6traf) {
            logger.debug("rx " + pckd);
        }
        pckd = parent.gotPack(pckd);
        if (pckd == null) {
            return;
        }
        if (debugger.servDhcp6traf) {
            logger.debug("tx " + pckd);
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
