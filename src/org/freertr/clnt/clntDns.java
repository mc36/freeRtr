package org.freertr.clnt;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgProxy;
import org.freertr.pack.packDns;
import org.freertr.pack.packDnsRec;
import org.freertr.pack.packDnsRes;
import org.freertr.pack.packDnsZone;
import org.freertr.pack.packHolder;
import org.freertr.pack.packSize;
import org.freertr.pipe.pipeSide;
import org.freertr.serv.servGeneric;
import org.freertr.user.userFormat;
import org.freertr.util.bits;
import org.freertr.util.debugger;
import org.freertr.util.logger;
import org.freertr.util.syncInt;

/**
 * domain name protocol (rfc1035) client
 *
 * @author matecsaba
 */
public class clntDns {

    /**
     * startup counter
     */
    public final static syncInt cntrStart = new syncInt(0);

    /**
     * error counter
     */
    public final static syncInt cntrError = new syncInt(0);

    /**
     * stop counter
     */
    public final static syncInt cntrStop = new syncInt(0);

    /**
     * working proxy
     */
    public cfgProxy curPrx;

    /**
     * dns question
     */
    private packDns query;

    /**
     * dns response
     */
    private packDns reply;

    /**
     * local positive cache
     */
    private static packDnsZone loPcache = new packDnsZone("");

    /**
     * local negative cache
     */
    private static packDnsZone loNcache = new packDnsZone("");

    /**
     * setup instance
     */
    public clntDns() {
        curPrx = cfgAll.nameServerProxy;
        if (curPrx == null) {
            curPrx = cfgAll.clientProxy;
        }
    }

    /**
     * resolve one host to address
     *
     * @param conole console to use
     * @param host hostname
     * @param prt protocol to prefer, 0=default
     * @return ip address, null if not found
     */
    public static addrIP resolveAddr(pipeSide console, String host, int prt) {
        addrIP addr = new addrIP();
        if (!addr.fromString(host)) {
            return addr;
        }
        if (prt == 0) {
            prt = clntDns.getPriPref();
        }
        console.strPut("resolving " + host + " for ipv" + prt);
        addrIP adr = clntDns.justResolv(host, prt);
        if (adr == null) {
            console.linePut(" failed!");
            return null;
        }
        console.linePut(" ok!");
        return adr;
    }

    /**
     * do resolver work
     *
     * @param host server name
     * @param prt protocol to prefer, 0=default, 4=ip4, 6=ip6
     * @return address of remove, null on error
     */
    public static addrIP justResolv(String host, int prt) {
        clntDns clnt = new clntDns();
        if (clnt.doResolvAddr(cfgAll.nameServerAddr, host, prt)) {
            return null;
        }
        return clnt.getAddr(prt);
    }

    /**
     * purge local cache
     *
     * @param full set true to full clear
     */
    public static void purgeLocalCache(boolean full) {
        if (full) {
            if (debugger.clntDnsTraf) {
                logger.debug("cleared");
            }
            loPcache.clear();
            loNcache.clear();
            return;
        }
        int del = purgeLocalCache(loPcache);
        del += purgeLocalCache(loNcache);
        if (debugger.clntDnsTraf) {
            logger.debug(del + " purged");
        }
    }

    private static int purgeLocalCache(packDnsZone loCache) {
        long t = bits.getTime();
        int del = 0;
        for (int i = loCache.size() - 1; i >= 0; i--) {
            packDnsRec ntry = loCache.get(i);
            if ((t - ntry.added) < (ntry.ttl * 1000)) {
                continue;
            }
            loCache.delBin(ntry);
            del++;
        }
        return del;
    }

    /**
     * display local cache
     *
     * @param neg true if negative cache needed
     * @return cache content
     */
    public static userFormat showLocalCache(boolean neg) {
        if (neg) {
            return loNcache.toUserStr(true);
        } else {
            return loPcache.toUserStr(true);
        }
    }

    private int doResolvSingle(addrIP srv, String nam, int typ) {
        reply = new packDns();
        packDnsRec cac = loNcache.findUser(nam, typ);
        if (cac != null) {
            if (debugger.clntDnsTraf) {
                logger.debug("negative hit " + cac);
            }
            return 2;
        }
        cac = loPcache.findUser(nam, typ);
        for (int lop = 0; lop < 32; lop++) {
            if (cac != null) {
                if (debugger.clntDnsTraf) {
                    logger.debug("cache hit " + cac);
                }
                reply.answers.add(cac);
                return 0;
            }
            cac = loPcache.findUser(nam, packDnsRec.typeCNAME);
            if (cac == null) {
                break;
            }
            if (debugger.clntDnsTraf) {
                logger.debug("cache redir " + cac);
            }
            reply.answers.add(cac);
            nam = cac.res.get(bits.random(0, cac.res.size())).target;
            cac = loPcache.findUser(nam, typ);
        }
        cntrStart.add(1);
        if (curPrx == null) {
            cntrError.add(1);
            return 1;
        }
        if (srv == null) {
            cntrError.add(1);
            return 1;
        }
        query = new packDns();
        packHolder pck = new packHolder(true, true);
        packDnsRec rr = new packDnsRec();
        packDnsRes rs = new packDnsRes();
        rr.res.add(rs);
        rr.name = nam;
        rr.typ = typ;
        rr.ttl = 180;
        query.queries.add(rr);
        query.id = bits.randomW();
        query.opcode = packDns.opcodeQuery;
        query.recDsrd = true;
        query.result = packDns.resultSuccess;
        for (int retry = 0; retry < 3; retry++) {
            pipeSide conn = curPrx.proxy.doConnect(servGeneric.protoUdp, srv, packDns.portNum, "dns");
            if (conn == null) {
                continue;
            }
            query.createHeader(pck);
            pck.pipeSend(conn, 0, pck.dataSize(), 2);
            conn.setTime(10000);
            pck.clear();
            if (debugger.clntDnsTraf) {
                logger.debug("tx " + srv + " " + query);
            }
            int i = pck.pipeRecv(conn, 0, 0, 143);
            conn.setClose();
            if (i < 1) {
                continue;
            }
            if (reply.parseHeader(pck)) {
                continue;
            }
            if (query.id != reply.id) {
                continue;
            }
            if (!reply.response) {
                continue;
            }
            if (debugger.clntDnsTraf) {
                logger.debug("rx " + srv + " " + reply);
            }
            loPcache.addList(reply.answers);
            loPcache.addList(reply.addition);
            loPcache.addList(reply.servers);
            if (reply.result == packDns.resultName) {
                loNcache.addBin(rr);
                cntrError.add(1);
                return 2;
            }
            if (reply.result != packDns.resultSuccess) {
                cntrError.add(1);
                return 1;
            }
            if (findAnswer(typ) != null) {
                cntrError.add(1);
                return 0;
            }
            if (findAnswer(packDnsRec.typeCNAME) != null) {
                cntrError.add(1);
                return 3;
            }
            loNcache.addBin(rr);
            cntrStop.add(1);
            return 2;
        }
        cntrError.add(1);
        return 1;
    }

    /**
     * recursively resolve a query
     *
     * @param srv server to use
     * @param nam name to query
     * @param typ type of record
     * @return false on success, true on error
     */
    public packDnsZone doRecursive(List<addrIP> srv, String nam, int typ) {
        List<addrIP> orig = srv;
        packDnsZone res = new packDnsZone("results");
        String dom = "";
        String odm = "!";
        for (;;) {
            boolean fin = dom.equals(odm);
            odm = dom;
            String a;
            int i = nam.lastIndexOf(".");
            if (i < 1) {
                a = nam;
                nam = "";
            } else {
                a = nam.substring(i + 1, nam.length());
                nam = nam.substring(0, i);
            }
            if ((dom.length() > 0) && (a.length() > 0)) {
                a += ".";
            }
            dom = a + dom;
            int ned = fin ? typ : packDnsRec.typeNS;
            i = doResolvList(srv, dom, false, ned);
            packDnsRec rec = findAnswer(packDnsRec.typeCNAME);
            if (rec != null) {
                res.addBin(rec);
            }
            if (i == 3) {
                dom = "";
                odm = "!";
                nam = rec.res.get(bits.random(0, rec.res.size())).target;
                srv = orig;
                continue;
            }
            rec = findAnswer(ned);
            if ((i != 0) || (rec == null)) {
                if (fin) {
                    return null;
                }
                if (nam.length() < 1) {
                    continue;
                }
                return null;
            }
            res.addBin(rec);
            if (fin) {
                break;
            }
            if (doResolvAddr(cfgAll.nameServerAddr, rec.res.get(bits.random(0, rec.res.size())).target, curPrx.proxy.prefer)) {
                return null;
            }
            addrIP adr = getAddr(curPrx.proxy.prefer);
            if (adr == null) {
                return null;
            }
            srv = new ArrayList<addrIP>();
            srv.add(adr);
        }
        return res;
    }

    /**
     * download one zone
     *
     * @param srv server to use
     * @param zon zone to update
     * @param forced true if download anyway
     * @return up to date zone, null on error
     */
    public packDnsZone doZoneXfer(addrIP srv, packDnsZone zon, boolean forced) {
        query = new packDns();
        reply = new packDns();
        packDnsRec rrF = new packDnsRec();
        packDnsRec rrL = new packDnsRec();
        packHolder pck = new packHolder(true, true);
        packDnsRes rs = new packDnsRes();
        rrF.res.add(rs);
        rrF.name = zon.name;
        rrF.typ = packDnsRec.typeAXFR;
        query.queries.add(rrF);
        query.id = bits.randomW();
        query.opcode = packDns.opcodeQuery;
        query.recDsrd = true;
        query.result = packDns.resultSuccess;
        pipeSide conn = curPrx.proxy.doConnect(servGeneric.protoTcp, srv, packDns.portNum, "dns");
        if (conn == null) {
            return null;
        }
        packSize blck = new packSize(conn, 2, true, 1, 0);
        query.createHeader(pck);
        blck.sendPacket(pck);
        conn.setTime(60000);
        if (debugger.clntDnsTraf) {
            logger.debug("tx " + query);
        }
        packDnsZone zon2 = new packDnsZone(zon.name);
        zon2.defttl = zon.defttl;
        zon2.axfr = zon.axfr;
        rrF = null;
        rrL = null;
        boolean end = false;
        for (;;) {
            reply = new packDns();
            pck = blck.recvPacket();
            if (pck == null) {
                conn.setClose();
                return null;
            }
            if (reply.parseHeader(pck)) {
                conn.setClose();
                return null;
            }
            if (debugger.clntDnsTraf) {
                logger.debug("rx " + reply);
            }
            for (int i = 0; i < reply.answers.size(); i++) {
                packDnsRec rrC = reply.answers.get(i);
                if (rrF == null) {
                    rrF = rrC;
                    continue;
                }
                rrL = rrC;
                zon2.addBin(rrC);
                end |= (rrC.typ == packDnsRec.typeSOA) && rrC.name.equals(zon.name);
            }
            if (end) {
                break;
            }
        }
        conn.setClose();
        if ((rrF == null) || (rrL == null)) {
            return null;
        }
        if ((rrF.typ != packDnsRec.typeSOA) || (rrL.typ != packDnsRec.typeSOA)) {
            return null;
        }
        if (rrF.res.get(0).sequence != rrL.res.get(0).sequence) {
            return null;
        }
        packDnsRec rrO = zon.findUser(zon.name, packDnsRec.typeSOA);
        boolean dl = rrO == null;
        if (!dl) {
            dl = rrF.res.get(0).sequence != rrO.res.get(0).sequence;
        }
        dl |= forced;
        if (!dl) {
            return zon;
        } else {
            return zon2;
        }
    }

    /**
     * do one resolving work
     *
     * @param srv list of servers to use
     * @param nam name to query
     * @param dom try appending domain
     * @param typ type of query
     * @return 0 on success, 1 on error, 2 on notfound, 3 on redirect
     */
    public int doResolvList(List<addrIP> srv, String nam, boolean dom, int typ) {
        if (srv == null) {
            return 1;
        }
        if (cfgAll.domainName == null) {
            dom = false;
        }
        for (int o = 0; o < srv.size(); o++) {
            int i = doResolvSingle(srv.get(o), nam, typ);
            switch (i) {
                case 0:
                    return 0;
                case 1:
                    continue;
                case 3:
                    return 3;
            }
            if (!dom) {
                return 2;
            }
            i = doResolvSingle(srv.get(o), nam + "." + cfgAll.domainName, typ);
            switch (i) {
                case 0:
                    return 0;
                case 1:
                    continue;
                case 3:
                    return 3;
            }
            return 2;
        }
        return 1;
    }

    /**
     * get primary preference
     *
     * @return preferred protocol
     */
    public static int getPriPref() {
        if (cfgAll.preferIpv6) {
            return 6;
        } else {
            return 4;
        }
    }

    /**
     * get primary type
     *
     * @param prefer preferred protocol, 0 if default
     * @return dns record type
     */
    public static int getTypPri(int prefer) {
        switch (prefer) {
            case 4:
                return packDnsRec.typeA;
            case 6:
                return packDnsRec.typeAAAA;
            default:
                if (cfgAll.preferIpv6) {
                    return packDnsRec.typeAAAA;
                } else {
                    return packDnsRec.typeA;
                }
        }
    }

    /**
     * get secondary type
     *
     * @param prefer preferred protocol, 0 if default
     * @return dns record type
     */
    public static int getTypSec(int prefer) {
        if (getTypPri(prefer) == packDnsRec.typeA) {
            return packDnsRec.typeAAAA;
        } else {
            return packDnsRec.typeA;
        }
    }

    private boolean checkReply(int cod) {
        if (cod != 0) {
            return true;
        }
        if (reply == null) {
            return true;
        }
        return reply.answers.size() < 1;
    }

    /**
     * do one resolving work
     *
     * @param srv list of servers to use
     * @param nam name to query
     * @param prefer preferred ip protocol; 0=default, 4=ip4, 6=ip6
     * @return false on success, true on error
     */
    public boolean doResolvAddr(List<addrIP> srv, String nam, int prefer) {
        addrIP adr = new addrIP();
        if (!adr.fromString(nam)) {
            packDnsRec rr = new packDnsRec();
            packDnsRes rs = new packDnsRes();
            rs.addr = adr;
            rr.res.add(rs);
            rr.typ = getTypPri(prefer);
            reply = new packDns();
            reply.answers.add(rr);
            return false;
        }
        int i = doResolvList(srv, nam, true, getTypPri(prefer));
        if (i == 1) {
            return true;
        }
        if (!checkReply(i)) {
            return false;
        }
        i = doResolvList(srv, nam, true, getTypSec(prefer));
        return checkReply(i);
    }

    /**
     * get answers
     *
     * @param lst list to append
     * @return number of entries added
     */
    public int getAnswers(List<packDnsRec> lst) {
        if (reply == null) {
            return 0;
        }
        lst.addAll(reply.answers);
        return reply.answers.size();
    }

    /**
     * find one answer
     *
     * @param typ type of record to find
     * @return resource record, null if not found
     */
    public packDnsRec findAnswer(int typ) {
        if (reply == null) {
            return null;
        }
        packDnsRec r = findAnswer(reply.answers, typ);
        if (r != null) {
            return r;
        }
        r = findAnswer(reply.servers, typ);
        if (r != null) {
            return r;
        }
        return findAnswer(reply.addition, typ);
    }

    private static packDnsRec findAnswer(List<packDnsRec> lst, int typ) {
        if (lst == null) {
            return null;
        }
        for (int i = 0; i < lst.size(); i++) {
            packDnsRec rr = lst.get(i);
            if (rr == null) {
                continue;
            }
            if (rr.typ != typ) {
                continue;
            }
            return rr;
        }
        return null;
    }

    /**
     * get ip address
     *
     * @param prefer preferred ip protocol; 0=default, 4=ip4, 6=ip6
     * @return ip address, null if not found
     */
    public addrIP getAddr(int prefer) {
        packDnsRec rr;
        rr = findAnswer(getTypPri(prefer));
        if (rr == null) {
            rr = findAnswer(getTypSec(prefer));
        }
        if (rr == null) {
            return null;
        }
        return rr.res.get(bits.random(0, rr.res.size())).addr;
    }

    /**
     * get name server
     *
     * @return name server, null if not found
     */
    public String getNS() {
        packDnsRec rr = findAnswer(packDnsRec.typeNS);
        if (rr == null) {
            return null;
        }
        return rr.res.get(bits.random(0, rr.res.size())).target;
    }

    /**
     * get mail server
     *
     * @return mail server, null if not found
     */
    public String getMX() {
        packDnsRec rr = findAnswer(packDnsRec.typeMX);
        if (rr == null) {
            return null;
        }
        return rr.res.get(bits.random(0, rr.res.size())).target;
    }

    /**
     * get text
     *
     * @return mail server, null if not found
     */
    public String getTXT() {
        packDnsRec rr = findAnswer(packDnsRec.typeTXT);
        if (rr == null) {
            return null;
        }
        return rr.res.get(bits.random(0, rr.res.size())).target;
    }

    /**
     * get name of host
     *
     * @return name of host, null if not found
     */
    public String getPTR() {
        packDnsRec rr = findAnswer(packDnsRec.typePTR);
        if (rr == null) {
            return null;
        }
        return rr.res.get(bits.random(0, rr.res.size())).target;
    }

}
