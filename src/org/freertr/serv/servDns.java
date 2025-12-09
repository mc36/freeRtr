package org.freertr.serv;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv4;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgProxy;
import org.freertr.clnt.clntDns;
import org.freertr.pack.packDns;
import org.freertr.pack.packDnsRec;
import org.freertr.pack.packDnsZone;
import org.freertr.pack.packHolder;
import org.freertr.pack.packSize;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGenConn;
import org.freertr.prt.prtServS;
import org.freertr.prt.prtTcp;
import org.freertr.sec.secInfoCfg;
import org.freertr.sec.secInfoCls;
import org.freertr.sec.secInfoUtl;
import org.freertr.sec.secInfoWrk;
import org.freertr.tab.tabGen;
import org.freertr.user.userFilter;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.debugger;
import org.freertr.util.logger;

/**
 * domain name protocol (rfc1035) server
 *
 * @author matecsaba
 */
public class servDns extends servGeneric implements prtServS {

    /**
     * create instance
     */
    public servDns() {
    }

    /**
     * list of zones
     */
    public tabGen<packDnsZone> zones = new tabGen<packDnsZone>();

    /**
     * list of resolvers
     */
    public tabGen<servDnsResolv> resolvs = new tabGen<servDnsResolv>();

    /**
     * list of recursions
     */
    public tabGen<servDnsResolv> rcrsvia = new tabGen<servDnsResolv>();

    /**
     * name of last
     */
    public String lastZone = "";

    /**
     * logging
     */
    public boolean logging;

    /**
     * recursion available
     */
    protected boolean recursEna = false;

    /**
     * access list to use
     */
    protected secInfoCfg recursAcl;

    /**
     * 6to4 prefix
     */
    protected addrIP recurs6to4;

    /**
     * defaults text
     */
    public final static userFilter[] defaultF = {
        new userFilter("server dns .*", cmds.tabulator + "port " + packDns.portNum, null),
        new userFilter("server dns .*", cmds.tabulator + "protocol " + proto2string(protoAll), null),
        new userFilter("server dns .*", cmds.tabulator + "recursion 6to4nothing", null),
        new userFilter("server dns .*", cmds.tabulator + "recursion disable", null),
        new userFilter("server dns .*", cmds.tabulator + cmds.negated + cmds.tabulator + "logging", null)
    };

    public userFilter[] srvDefFlt() {
        return defaultF;
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.setTime(10000);
        new servDnsDoer(this, pipe, id);
        return false;
    }

    public String srvName() {
        return "dns";
    }

    public int srvPort() {
        return packDns.portNum;
    }

    public int srvProto() {
        return protoAll;
    }

    public boolean srvInit() {
        dynBlckMod = true;
        return genStrmStart(this, new pipeLine(32768, false), 0);
    }

    public boolean srvDeinit() {
        return genericStop(0);
    }

    public void srvShRun(String beg, List<String> lst, int filter) {
        cmds.cfgLine(lst, !logging, beg, "logging", "");
        if (recursAcl != null) {
            secInfoUtl.getConfig(lst, recursAcl, beg + "recursion access-");
        }
        if (recurs6to4 != null) {
            lst.add(beg + "recursion 6to4prefix " + recurs6to4);
        } else {
            lst.add(beg + "recursion 6to4nothing");
        }
        if (recursEna) {
            lst.add(beg + "recursion enable");
        } else {
            lst.add(beg + "recursion disable");
        }
        for (int i = 0; i < resolvs.size(); i++) {
            servDnsResolv res = resolvs.get(i);
            lst.add(beg + "resolver " + res);
        }
        for (int i = 0; i < rcrsvia.size(); i++) {
            servDnsResolv res = rcrsvia.get(i);
            lst.add(beg + "recursion via " + res);
        }
        for (int i = 0; i < zones.size(); i++) {
            packDnsZone zon = zones.get(i);
            lst.addAll(zon.saveZone(beg + "zone " + zon.name));
        }
    }

    public void srvHelp(userHelp l) {
        l.add(null, false, 1, new int[]{-1}, "logging", "log queries");
        l.add(null, false, 1, new int[]{2}, "recursion", "recursive parameters");
        l.add(null, false, 2, new int[]{-1}, "enable", "allow recursion");
        l.add(null, false, 2, new int[]{-1}, "disable", "forbid recursion");
        secInfoUtl.getHelp(l, 1, "access-", null);
        l.add(null, false, 2, new int[]{3}, "6to4prefix", "setup 6to4 prefix");
        l.add(null, false, 3, new int[]{-1}, "<addr>", "address to prepend");
        l.add(null, false, 2, new int[]{-1}, "6to4nothing", "clear 6to4 prefix");
        l.add(null, false, 2, new int[]{3}, "via", "define root");
        l.add(null, false, 3, new int[]{4}, "<str>", "zone name");
        l.add(null, false, 4, new int[]{5}, "<name:prx>", "proxy to use");
        l.add(null, false, 5, new int[]{5, -1}, "<addr>", "address of resolver");
        l.add(null, false, 1, new int[]{2}, "resolver", "define resolver");
        l.add(null, false, 2, new int[]{3}, "<str>", "zone name");
        l.add(null, false, 3, new int[]{4}, "<name:prx>", "proxy to use");
        l.add(null, false, 4, new int[]{4, -1}, "<addr>", "address of resolver");
        l.add(null, false, 1, new int[]{2}, "zone", "name of a zone");
        List<String> lst = new ArrayList<String>();
        for (int i = 0; i < zones.size(); i++) {
            lst.add(zones.get(i).name);
        }
        l.add(lst, false, 2, new int[]{1, -1}, "<name:loc>", "zone name");
        l.add(null, false, 1, new int[]{3}, "defttl", "specify time to live");
        l.add(null, false, 3, new int[]{-1}, "<num>", "time to live");
        l.add(null, false, 1, new int[]{3}, "axfr", "specify zone transfer");
        l.add(null, false, 3, new int[]{-1}, "enable", "allow");
        l.add(null, false, 3, new int[]{-1}, "disable", "prohibit");
        l.add(null, false, 1, new int[]{-1}, "clear", "clear all records from zone");
        l.add(null, false, 1, new int[]{3}, "reverse", "generate reverse zone");
        l.add(null, false, 3, new int[]{-1}, "<str>", "name of zone");
        l.add(null, false, 1, new int[]{3}, "download", "download zone with dns axfr if changed");
        l.add(null, false, 3, new int[]{4}, "<name:prx>", "proxy to use");
        l.add(null, false, 4, new int[]{-1}, "<str>", "name of server");
        l.add(null, false, 1, new int[]{3}, "redownload", "download zone with dns axfr anyway");
        l.add(null, false, 3, new int[]{4}, "<name:prx>", "proxy to use");
        l.add(null, false, 4, new int[]{-1}, "<str>", "name of server");
        l.add(null, false, 1, new int[]{3}, "rr", "specify a record");
        lst = new ArrayList<String>();
        packDnsZone zon = new packDnsZone(lastZone);
        zon = zones.find(zon);
        if (zon != null) {
            lst = zon.subDomains();
        }
        l.add(lst, false, 3, new int[]{4}, "<name:loc>", "domain name");
        l.add(null, false, 4, new int[]{5}, "soa", "specify a start of authority");
        l.add(lst, false, 5, new int[]{6}, "<name:loc>", "name server");
        l.add(null, false, 6, new int[]{7}, "<str>", "email of author");
        l.add(null, false, 7, new int[]{8}, "<num>", "sequence number");
        l.add(null, false, 8, new int[]{9}, "<num>", "refresh interval");
        l.add(null, false, 9, new int[]{10}, "<num>", "retry interval");
        l.add(null, false, 10, new int[]{11}, "<num>", "expire interval");
        l.add(null, false, 11, new int[]{-1}, "<num>", "minimum ttl value");
        l.add(null, false, 4, new int[]{5}, "hinfo", "specify a host information");
        l.add(null, false, 5, new int[]{6}, "<str>", "oerating system");
        l.add(null, false, 6, new int[]{-1}, "<str>", "hardware platform");
        l.add(null, false, 4, new int[]{5}, "cname", "specify a canonical name");
        l.add(lst, false, 5, new int[]{-1}, "<name:loc>", "name of host");
        l.add(null, false, 4, new int[]{5}, "rp", "specify a responsible person");
        l.add(lst, false, 5, new int[]{6}, "<name:loc>", "mail server");
        l.add(null, false, 6, new int[]{-1}, "<str>", "email of author");
        l.add(null, false, 4, new int[]{5}, "srv", "specify a responsible person");
        l.add(null, false, 5, new int[]{6}, "<num>", "priority");
        l.add(null, false, 6, new int[]{7}, "<num>", "weight");
        l.add(null, false, 7, new int[]{8}, "<num>", "port");
        l.add(lst, false, 8, new int[]{-1}, "<name:loc>", "server");
        l.add(null, false, 4, new int[]{5}, "mx", "specify a mailbox server");
        l.add(null, false, 5, new int[]{6}, "<num>", "preference");
        l.add(lst, false, 6, new int[]{-1}, "<name:loc>", "mail server");
        l.add(null, false, 4, new int[]{5}, "txt", "specify a description");
        l.add(null, false, 5, new int[]{5, -1}, "<str>", "description");
        l.add(null, false, 4, new int[]{5}, "ns", "specify a name server");
        l.add(lst, false, 5, new int[]{-1}, "<name:loc>", "name server");
        l.add(null, false, 4, new int[]{5}, "ptr", "specify a pointer");
        l.add(null, false, 5, new int[]{-1}, "<str>", "name of address");
        l.add(null, false, 4, new int[]{5}, "ip4a", "specify an ip4 address");
        l.add(null, false, 5, new int[]{-1}, "<addr>", "address of server");
        l.add(null, false, 4, new int[]{5}, "ip6a", "specify an ip6 address");
        l.add(null, false, 5, new int[]{-1}, "<addr>", "address of server");
        l.add(null, false, 4, new int[]{5}, "ip4i", "specify an ip4 interface");
        l.add(null, false, 5, new int[]{-1}, "<name:ifc>", "name of interface");
        l.add(null, false, 4, new int[]{5}, "ip6i", "specify an ip6 interface");
        l.add(null, false, 5, new int[]{-1}, "<name:ifc>", "name of interface");
    }

    public boolean srvCfgStr(cmds cmd) {
        cmds old = cmd.copyBytes(false);
        String s = cmd.word();
        boolean negated = s.equals(cmds.negated);
        if (negated) {
            s = cmd.word();
        }
        if (s.length() < 1) {
            return false;
        }
        if (s.equals("logging")) {
            logging = !negated;
            return false;
        }
        if (s.equals("resolver")) {
            servDnsResolv res = new servDnsResolv(cmd.word());
            if (res.fromString(cmd)) {
                return false;
            }
            if (negated) {
                resolvs.del(res);
                return false;
            }
            resolvs.put(res);
            return false;
        }
        if (s.equals("recursion")) {
            s = cmd.word();
            if (s.equals("via")) {
                servDnsResolv res = new servDnsResolv(cmd.word());
                if (res.fromString(cmd)) {
                    return false;
                }
                if (negated) {
                    rcrsvia.del(res);
                    return false;
                }
                rcrsvia.put(res);
                return false;
            }
            if (s.equals("enable")) {
                recursEna = !negated;
                return false;
            }
            if (s.equals("disable")) {
                recursEna = negated;
                return false;
            }
            if (s.startsWith("access-")) {
                s = s.substring(7, s.length());
                s += " " + cmd.getRemaining();
                s = s.trim();
                cmd = new cmds("info", s);
                recursAcl = secInfoUtl.doCfgStr(recursAcl, cmd, negated);
                return false;
            }
            if (s.equals("6to4prefix")) {
                if (negated) {
                    recurs6to4 = null;
                    return false;
                }
                recurs6to4 = new addrIP();
                recurs6to4.fromString(cmd.word());
                return false;
            }
            if (s.equals("6to4nothing")) {
                recurs6to4 = null;
                return false;
            }
            cmd.badCmd();
            return false;
        }
        if (s.equals("zone")) {
            lastZone = cmd.word();
            old = cmd.copyBytes(false);
            s = cmd.word();
        }
        if (lastZone.length() < 1) {
            return true;
        }
        packDnsZone zon = new packDnsZone(lastZone);
        packDnsZone prv = zones.add(zon);
        if (prv != null) {
            zon = prv;
        }
        if (s.equals("reverse")) {
            s = cmd.word();
            prv = zones.find(new packDnsZone(s));
            if (prv == null) {
                return true;
            }
            zon.addZone(prv.reverseZone());
            return false;
        }
        if (s.equals("download")) {
            clntDns clnt = new clntDns();
            cfgProxy prx = cfgAll.proxyFind(cmd.word(), false);
            if (prx == null) {
                return true;
            }
            clnt.curPrx = prx;
            addrIP adr = new addrIP();
            adr.fromString(cmd.word());
            zon = clnt.doZoneXfer(adr, zon, false);
            if (zon == null) {
                return true;
            }
            zones.put(zon);
            return false;
        }
        if (s.equals("redownload")) {
            clntDns clnt = new clntDns();
            cfgProxy prx = cfgAll.proxyFind(cmd.word(), false);
            if (prx == null) {
                return true;
            }
            clnt.curPrx = prx;
            addrIP adr = new addrIP();
            adr.fromString(cmd.word());
            zon = clnt.doZoneXfer(adr, zon, true);
            if (zon == null) {
                return true;
            }
            zones.put(zon);
            return false;
        }
        if (s.equals("clear")) {
            zon.clear();
            return false;
        }
        if (negated && s.equals("defttl")) {
            zones.del(zon);
            return false;
        }
        if (negated) {
            packDnsRec ntry = new packDnsRec();
            if (ntry.fromUserStr(cmd)) {
                return true;
            }
            return zon.delBin(ntry);
        }
        return zon.addUser(old.getRemaining());
    }

}

class servDnsResolv implements Comparable<servDnsResolv> {

    public final String name;

    public cfgProxy proxy;

    public List<addrIP> addr;

    public servDnsResolv(String nam) {
        if (nam.equals(".")) {
            nam = "";
        }
        name = nam.toLowerCase();
    }

    public int compareTo(servDnsResolv o) {
        return name.compareTo(o.name);
    }

    public String toString() {
        String a = " " + proxy.name;
        for (int i = 0; i < addr.size(); i++) {
            a += " " + addr.get(i);
        }
        if (name.length() < 1) {
            return "." + a;
        } else {
            return name + a;
        }
    }

    public boolean fromString(cmds cmd) {
        proxy = cfgAll.proxyFind(cmd.word(), false);
        if (proxy == null) {
            return true;
        }
        addr = new ArrayList<addrIP>();
        for (;;) {
            String a = cmd.word();
            if (a.length() < 1) {
                break;
            }
            addrIP adr = new addrIP();
            if (adr.fromString(a)) {
                continue;
            }
            addr.add(adr);
        }
        return addr.size() < 1;
    }

}

class servDnsDoer implements Runnable {

    private servDns parent;

    private pipeSide pipe;

    private prtGenConn conn;

    private boolean recurse;

    public servDnsDoer(servDns lower, pipeSide stream, prtGenConn id) {
        parent = lower;
        pipe = stream;
        conn = id;
        new Thread(this).start();
    }

    public void sendReply(packDns pckD) {
        if (debugger.servDnsTraf) {
            logger.debug("tx " + pckD);
        }
        packHolder pckB = new packHolder(true, true);
        pckB.clear();
        pckD.createHeader(pckB);
        pckB.merge2beg();
        if (pipe.isBlockMode()) {
            pckB.pipeSend(pipe, 0, pckB.dataSize(), 2);
        } else {
            packSize blck = new packSize(pipe, 2, true, 1, 0);
            blck.sendPacket(pckB);
        }
    }

    private void addAnswer(packDns pck, packDnsZone zon, String s, int typ) {
        packDnsRec rr = zon.findUser(s, typ);
        if (rr == null) {
            return;
        }
        pck.answers.add(rr);
    }

    private boolean doSlaves(servDnsResolv ntry, List<packDnsRec> res, int typ, String nam) {
        clntDns clnt = new clntDns();
        clnt.curPrx = ntry.proxy;
        if (clnt.doResolvList(ntry.addr, nam, false, typ) != 0) {
            return false;
        }
        if (typ == packDnsRec.typeANY) {
            clnt.getAnswers(res);
            return true;
        }
        packDnsRec rr = clnt.findAnswer(typ);
        if (rr == null) {
            return false;
        }
        rr = rr.copyBytes();
        rr.name = nam;
        res.add(rr);
        return true;
    }

    private boolean doSlaver(servDnsResolv ntry, List<packDnsRec> res, int typ, String nam) {
        clntDns clnt = new clntDns();
        clnt.curPrx = ntry.proxy;
        packDnsZone zon = clnt.doRecursive(ntry.addr, nam, typ);
        if (zon == null) {
            return false;
        }
        packDnsRec rec = zon.findUser(nam, typ);
        if (rec == null) {
            rec = zon.findUser(nam, packDnsRec.typeCNAME);
            if (rec == null) {
                return false;
            }
            res.add(rec);
            rec = zon.findUser(rec.res.get(bits.random(0, rec.res.size())).target, typ);
            if (rec == null) {
                return false;
            }
            res.add(rec);
            return true;
        }
        res.add(rec);
        return true;
    }

    private boolean doResolve(List<packDnsRec> res, int typ, String nam) { // true if authoritative
        packDnsZone zon = parent.zones.find(new packDnsZone(nam));
        servDnsResolv rslvr = parent.resolvs.find(new servDnsResolv(nam));
        servDnsResolv rcrsr = parent.rcrsvia.find(new servDnsResolv(nam));
        String old = nam;
        String a = "";
        for (; zon == null;) {
            if (rslvr != null) {
                doSlaves(rslvr, res, typ, old);
                return true;
            }
            if (rcrsr != null) {
                doSlaver(rcrsr, res, typ, old);
                return true;
            }
            int i = nam.indexOf(".");
            if (i >= 0) {
                a = nam.substring(0, i);
                nam = nam.substring(i + 1, nam.length());
            } else {
                a = nam;
                nam = ".";
            }
            zon = parent.zones.find(new packDnsZone(nam));
            rslvr = parent.resolvs.find(new servDnsResolv(nam));
            rcrsr = parent.rcrsvia.find(new servDnsResolv(nam));
            if (i < 0) {
                break;
            }
        }
        if (zon == null) {
            if (rslvr != null) {
                doSlaves(rslvr, res, typ, old);
                return true;
            }
            if (rcrsr != null) {
                doSlaver(rcrsr, res, typ, old);
                return true;
            }
            if (!recurse) {
                return false;
            }
            if ((parent.recurs6to4 != null) && (typ == packDnsRec.typeA)) {
                return false;
            }
            clntDns clnt = new clntDns();
            clnt.doResolvList(cfgAll.nameServerAddr, old, false, typ);
            if (typ == packDnsRec.typeANY) {
                clnt.getAnswers(res);
                return false;
            }
            packDnsRec rr = clnt.findAnswer(typ);
            if (rr != null) {
                rr = rr.copyBytes();
                rr.name = old;
                res.add(rr);
                return false;
            }
            if (parent.recurs6to4 == null) {
                return false;
            }
            if (typ != packDnsRec.typeAAAA) {
                return false;
            }
            clnt = new clntDns();
            if (clnt.doResolvList(cfgAll.nameServerAddr, old, false, packDnsRec.typeA) != 0) {
                return false;
            }
            rr = clnt.findAnswer(packDnsRec.typeA);
            if (rr == null) {
                return false;
            }
            rr = rr.copyBytes();
            for (int i = 0; i < rr.res.size(); i++) {
                addrIP adr = rr.res.get(i).addr;
                bits.byteFill(adr.getBytes(), 0, addrIP.size - addrIPv4.size, 0);
                adr.setOr(adr, parent.recurs6to4);
            }
            rr.typ = packDnsRec.typeAAAA;
            rr.name = old;
            res.add(rr);
            return false;
        }
        if (a.length() > 0) {
            a += ".";
        }
        nam = a + zon.name;
        packDnsRec rep = zon.findUser(old, typ);
        if (rep == null) {
            rep = zon.findUser(old, packDnsRec.typeCNAME);
        }
        if (rep == null) {
            rep = zon.findWild("*." + zon.name, old, typ);
        }
        if (rep == null) {
            rep = zon.findWild("*." + zon.name, old, packDnsRec.typeCNAME);
        }
        if (rep == null) {
            rep = zon.findUser(nam, packDnsRec.typeSOA);
        }
        if (rep == null) {
            rep = zon.findWild("*." + zon.name, old, packDnsRec.typeSOA);
        }
        if (rep == null) {
            rep = zon.findUser(nam, packDnsRec.typeNS);
        }
        if (rep == null) {
            rep = zon.findWild("*." + zon.name, old, packDnsRec.typeNS);
        }
        if (rep == null) {
            rep = zon.findUser(zon.name, packDnsRec.typeSOA);
        }
        if (rep == null) {
            return true;
        }
        res.add(rep);
        return true;
    }

    private boolean doer() {
        packHolder pckB = null;
        packDns pckD = new packDns();
        if (pipe.isBlockMode()) {
            pckB = pipe.readPacket(true);
        } else {
            packSize blck = new packSize(pipe, 2, true, 1, 0);
            pckB = blck.recvPacket();
        }
        if (pckB == null) {
            return true;
        }
        if (pckD.parseHeader(pckB)) {
            logger.info("got bad packet");
            return false;
        }
        if (debugger.servDnsTraf) {
            logger.debug("rx " + pckD);
        }
        recurse = parent.recursEna;
        if (recurse && (parent.recursAcl != null)) {
            secInfoCls cls = new secInfoCls(null, null, null, parent.srvVrf.getFwd(conn.peerAddr), conn.peerAddr, prtTcp.protoNum, conn.iface.addr);
            secInfoWrk wrk = new secInfoWrk(parent.recursAcl, cls);
            wrk.doWork(false);
            if (wrk.need2drop()) {
                recurse = false;
            }
        }
        int i = pckD.opcode;
        pckD.opcode = packDns.opcodeQuery;
        pckD.result = packDns.resultSupport;
        pckD.response = true;
        pckD.recAvail = recurse;
        if (i != packDns.opcodeQuery) {
            sendReply(pckD);
            return false;
        }
        if (pckD.queries.size() < 1) {
            sendReply(pckD);
            return false;
        }
        pckD.result = packDns.resultName;
        pckD.addition.clear();
        pckD.answers.clear();
        pckD.servers.clear();
        packDnsRec req = pckD.queries.get(0);
        if (parent.logging) {
            logger.info(conn.peerAddr + " queried " + req);
        }
        switch (req.typ) {
            case packDnsRec.typeAXFR:
                packDnsZone zon = parent.zones.find(new packDnsZone(req.name));
                if (zon == null) {
                    sendReply(pckD);
                    return false;
                }
                if (!zon.axfr) {
                    sendReply(pckD);
                    return false;
                }
                packDnsRec soa = zon.findUser(zon.name, packDnsRec.typeSOA);
                if (soa == null) {
                    sendReply(pckD);
                    return false;
                }
                pckD.result = packDns.resultSuccess;
                pckD.queries.clear();
                pckD.answers.clear();
                pckD.answers.add(soa);
                sendReply(pckD);
                for (i = 0; i < zon.size(); i++) {
                    if (pipe.isClosed() != 0) {
                        break;
                    }
                    packDnsRec ntry = zon.get(i);
                    if (ntry.compareTo(soa) == 0) {
                        continue;
                    }
                    pckD.answers.clear();
                    pckD.answers.add(ntry);
                    sendReply(pckD);
                }
                pckD.answers.clear();
                pckD.answers.add(soa);
                sendReply(pckD);
                return false;
            case packDnsRec.typeANY:
                zon = parent.zones.find(new packDnsZone(req.name));
                if (zon == null) {
                    i = req.name.indexOf(".");
                    if (i > 0) {
                        zon = parent.zones.find(new packDnsZone(req.name.substring(i + 1, req.name.length())));
                    }
                }
                if (zon == null) {
                    pckD.authoritative = doResolve(pckD.answers, req.typ, req.name);
                    sendReply(pckD);
                    return false;
                }
                addAnswer(pckD, zon, req.name, packDnsRec.typeA);
                addAnswer(pckD, zon, req.name, packDnsRec.typeAAAA);
                addAnswer(pckD, zon, req.name, packDnsRec.typeCNAME);
                addAnswer(pckD, zon, req.name, packDnsRec.typeHINFO);
                addAnswer(pckD, zon, req.name, packDnsRec.typeTXT);
                addAnswer(pckD, zon, req.name, packDnsRec.typeSRV);
                addAnswer(pckD, zon, req.name, packDnsRec.typeNS);
                addAnswer(pckD, zon, req.name, packDnsRec.typeMX);
                addAnswer(pckD, zon, req.name, packDnsRec.typeSOA);
                pckD.authoritative = true;
                pckD.result = packDns.resultSuccess;
                sendReply(pckD);
                return false;
        }
        pckD.authoritative = doResolve(pckD.answers, req.typ, req.name);
        if (pckD.answers.size() < 1) {
            sendReply(pckD);
            return false;
        }
        pckD.result = packDns.resultSuccess;
        int typ = req.typ;
        for (int o = 0; o < 16; o++) {
            int p = pckD.answers.size();
            req = pckD.answers.get(p - 1);
            if (req.typ != packDnsRec.typeCNAME) {
                break;
            }
            doResolve(pckD.answers, typ, req.res.get(0).target);
            if (p >= pckD.answers.size()) {
                break;
            }
        }
        sendReply(pckD);
        return false;
    }

    public void run() {
        try {
            pipe.wait4ready(10000);
            for (;;) {
                if (doer()) {
                    break;
                }
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
        pipe.setClose();
    }

}
