package net.freertr.serv;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrIPv4;
import net.freertr.cfg.cfgAceslst;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgProxy;
import net.freertr.clnt.clntDns;
import net.freertr.pack.packDns;
import net.freertr.pack.packDnsRec;
import net.freertr.pack.packDnsZone;
import net.freertr.pack.packHolder;
import net.freertr.pack.packSize;
import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeSide;
import net.freertr.prt.prtGenConn;
import net.freertr.prt.prtServS;
import net.freertr.tab.tabAceslstN;
import net.freertr.tab.tabGen;
import net.freertr.tab.tabListing;
import net.freertr.user.userFilter;
import net.freertr.user.userHelping;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.debugger;
import net.freertr.util.logger;

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
    protected tabListing<tabAceslstN<addrIP>, addrIP> recursAcl;

    /**
     * 6to4 prefix
     */
    protected addrIP recurs6to4;

    /**
     * defaults text
     */
    public final static String[] defaultL = {
        "server dns .*! port " + packDns.portNum,
        "server dns .*! protocol " + proto2string(protoAll),
        "server dns .*! recursion access-all",
        "server dns .*! recursion 6to4nothing",
        "server dns .*! recursion disable",
        "server dns .*! no logging"
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public tabGen<userFilter> srvDefFlt() {
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
            lst.add(beg + "recursion access-class " + recursAcl.listName);
        } else {
            lst.add(beg + "recursion access-all");
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
        for (int i = 0; i < zones.size(); i++) {
            packDnsZone zon = zones.get(i);
            lst.addAll(zon.saveZone(beg + "zone " + zon.name));
        }
    }

    public void srvHelp(userHelping l) {
        l.add(null, "1  .   logging                   log queries");
        l.add(null, "1  2   recursion                 recursive parameters");
        l.add(null, "2  .     enable                  allow recursion");
        l.add(null, "2  .     disable                 forbid recursion");
        l.add(null, "2  .     access-all              clear access list");
        l.add(null, "2  3     access-class            set access list");
        l.add(null, "3  .       <name:acl>            access list to use");
        l.add(null, "2  3     6to4prefix              setup 6to4 prefix");
        l.add(null, "3  .       <addr>                address to prepend");
        l.add(null, "2  .     6to4nothing             clear 6to4 prefix");
        l.add(null, "1  2   resolver                  define resolver");
        l.add(null, "2  3     <str>                   zone name");
        l.add(null, "3  3,.     <addr>                address of resolver");
        l.add(null, "1  2   zone                      name of a zone");
        List<String> lst = new ArrayList<String>();
        for (int i = 0; i < zones.size(); i++) {
            lst.add(zones.get(i).name);
        }
        l.add(lst, "2  1,.   <name:loc>              zone name");
        l.add(null, "1  3   defttl                    specify time to live");
        l.add(null, "3  .     <num>                   time to live");
        l.add(null, "1  3   axfr                      specify zone transfer");
        l.add(null, "3  .     enable                  allow");
        l.add(null, "3  .     disable                 prohibit");
        l.add(null, "1  .   clear                     clear all records from zone");
        l.add(null, "1  3   reverse                   generate reverse zone");
        l.add(null, "3  .     <str>                   name of zone");
        l.add(null, "1  3   download                  download zone with dns axfr if changed");
        l.add(null, "3  4     <name:prx>              proxy to use");
        l.add(null, "4  .       <str>                 name of server");
        l.add(null, "1  3   redownload                download zone with dns axfr anyway");
        l.add(null, "3  4     <name:prx>              proxy to use");
        l.add(null, "4  .       <str>                 name of server");
        l.add(null, "1  3   rr                        specify a record");
        lst = new ArrayList<String>();
        packDnsZone zon = new packDnsZone(lastZone);
        zon = zones.find(zon);
        if (zon != null) {
            lst = zon.subDomains();
        }
        l.add(lst, "3  4     <name:loc>              domain name");
        l.add(null, "4  5       soa                   specify a start of authority");
        l.add(lst, "5  6         <name:loc>          name server");
        l.add(null, "6  7           <str>             email of author");
        l.add(null, "7  8             <num>           sequence number");
        l.add(null, "8  9               <num>         refresh interval");
        l.add(null, "9  10                 <num>      retry interval");
        l.add(null, "10 11                   <num>    expire interval");
        l.add(null, "11 .                      <num>  minimum ttl value");
        l.add(null, "4  5       hinfo                 specify a host information");
        l.add(null, "5  6         <str>               oerating system");
        l.add(null, "6  .           <str>             hardware platform");
        l.add(null, "4  5       cname                 specify a canonical name");
        l.add(lst, "5  .         <name:loc>          name of host");
        l.add(null, "4  5       rp                    specify a responsible person");
        l.add(lst, "5  6         <name:loc>          mail server");
        l.add(null, "6  .           <str>             email of author");
        l.add(null, "4  5       srv                   specify a responsible person");
        l.add(null, "5  6         <num>               priority");
        l.add(null, "6  7           <num>             weight");
        l.add(null, "7  8             <num>           port");
        l.add(lst, "8  .               <name:loc>    server");
        l.add(null, "4  5       mx                    specify a mailbox server");
        l.add(null, "5  6         <num>               preference");
        l.add(lst, "6  .           <name:loc>        mail server");
        l.add(null, "4  5       txt                   specify a description");
        l.add(null, "5  5,.       <str>               description");
        l.add(null, "4  5       ns                    specify a name server");
        l.add(lst, "5  .         <name:loc>          name server");
        l.add(null, "4  5       ptr                   specify a pointer");
        l.add(null, "5  .         <str>               name of address");
        l.add(null, "4  5       ip4a                  specify an ip4 address");
        l.add(null, "5  .         <addr>              address of server");
        l.add(null, "4  5       ip6a                  specify an ip6 address");
        l.add(null, "5  .         <addr>              address of server");
        l.add(null, "4  5       ip4i                  specify an ip4 interface");
        l.add(null, "5  .         <name:ifc>          name of interface");
        l.add(null, "4  5       ip6i                  specify an ip6 interface");
        l.add(null, "5  .         <name:ifc>          name of interface");
    }

    public boolean srvCfgStr(cmds cmd) {
        cmds old = cmd.copyBytes(false);
        String s = cmd.word();
        boolean negated = s.equals("no");
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
            if (s.equals("enable")) {
                recursEna = !negated;
                return false;
            }
            if (s.equals("disable")) {
                recursEna = negated;
                return false;
            }
            if (s.equals("access-class")) {
                if (negated) {
                    recursAcl = null;
                    return false;
                }
                cfgAceslst ntry = cfgAll.aclsFind(cmd.word(), false);
                if (ntry == null) {
                    cmd.error("no such access list");
                    return false;
                }
                recursAcl = ntry.aceslst;
                return false;
            }
            if (s.equals("access-all")) {
                recursAcl = null;
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
            zones.del(new packDnsZone(zon.name));
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

class servDnsResolv implements Comparator<servDnsResolv> {

    public final String name;

    public List<addrIP> addr;

    public servDnsResolv(String nam) {
        name = nam.toLowerCase();
    }

    public int compare(servDnsResolv o1, servDnsResolv o2) {
        return o1.name.compareTo(o2.name);
    }

    public String toString() {
        String a = "";
        for (int i = 0; i < addr.size(); i++) {
            a += " " + addr.get(i);
        }
        return name + a;
    }

    public boolean fromString(cmds cmd) {
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

    private boolean doResolve(List<packDnsRec> res, int typ, String nam) { // true if authoritative
        packDnsZone zon = parent.zones.find(new packDnsZone(nam));
        servDnsResolv rslvr = parent.resolvs.find(new servDnsResolv(nam));
        String old = nam;
        String a = "";
        for (; zon == null;) {
            if (rslvr != null) {
                doSlaves(rslvr, res, typ, old);
                return true;
            }
            int i = nam.indexOf(".");
            if (i >= 0) {
                a = nam.substring(0, i);
                nam = nam.substring(i + 1, nam.length());
            } else {
                a = nam;
                nam = "";
            }
            zon = parent.zones.find(new packDnsZone(nam));
            rslvr = parent.resolvs.find(new servDnsResolv(nam));
            if (i < 0) {
                break;
            }
        }
        if (zon == null) {
            if (rslvr != null) {
                doSlaves(rslvr, res, typ, old);
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
        if (parent.recursAcl != null) {
            recurse &= parent.recursAcl.matches(conn);
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
                    if (ntry.compare(ntry, soa) == 0) {
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
        for (;;) {
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
