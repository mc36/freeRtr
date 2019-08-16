package clnt;

import addr.addrIP;
import cfg.cfgAll;
import cfg.cfgProxy;
import java.util.ArrayList;
import java.util.List;
import pack.packDns;
import pack.packDnsRec;
import pack.packDnsZone;
import pack.packHolder;
import pack.packSize;
import pipe.pipeSide;
import serv.servGeneric;
import user.userFormat;
import user.userTerminal;
import util.bits;
import util.debugger;
import util.logger;

/**
 * domain name protocol (rfc1035) client
 *
 * @author matecsaba
 */
public class clntDns {

    private cfgProxy curPrx; // working proxy

    private packDns query; // dns question

    private packDns reply; // dns response

    private static packDnsZone loCache = new packDnsZone(""); // local cache

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
     * purge local cache
     *
     * @param full set true to full clear
     */
    public static void purgeLocalCache(boolean full) {
        if (full) {
            if (debugger.clntDnsTraf) {
                logger.debug("cleared");
            }
            loCache.clear();
            return;
        }
        long t = bits.getTime();
        int del = 0;
        for (int i = loCache.size() - 1; i >= 0; i--) {
            packDnsRec ntry = loCache.get(i);
            if (((t - ntry.added) / 1000) < ntry.ttl) {
                continue;
            }
            loCache.delBin(ntry);
            del++;
        }
        if (debugger.clntDnsTraf) {
            logger.debug(del + " purged");
        }
    }

    /**
     * display local cache
     *
     * @return cache content
     */
    public static userFormat showLocalCache() {
        return loCache.toUserStr();
    }

    /**
     * resolve a query
     *
     * @param srv server to use
     * @param nam name to query
     * @param typ type of record
     * @return false on success, true on error
     */
    public boolean doResolvOne(addrIP srv, String nam, int typ) {
        reply = new packDns();
        packDnsRec cac = loCache.findUser(nam, typ);
        for (int lop = 0; lop < 32; lop++) {
            if (cac != null) {
                if (debugger.clntDnsTraf) {
                    logger.debug("cache hit " + cac);
                }
                reply.answers.add(cac);
                return false;
            }
            cac = loCache.findUser(nam, packDnsRec.typeCNAME);
            if (cac == null) {
                break;
            }
            if (debugger.clntDnsTraf) {
                logger.debug("cache redir " + cac);
            }
            nam = cac.target;
            cac = loCache.findUser(nam, typ);
        }
        if (curPrx == null) {
            return true;
        }
        if (srv == null) {
            return true;
        }
        query = new packDns();
        packHolder pck = new packHolder(true, true);
        packDnsRec rr = new packDnsRec();
        rr.name = nam;
        rr.typ = typ;
        query.queries.add(rr);
        query.id = bits.randomW();
        query.opcode = packDns.opcodeQuery;
        query.recDsrd = true;
        query.result = packDns.resultSuccess;
        for (int retry = 0; retry < 3; retry++) {
            pipeSide conn = curPrx.doConnect(servGeneric.protoUdp, srv, packDns.portNum, "dns");
            if (conn == null) {
                continue;
            }
            query.createHeader(pck);
            pck.pipeSend(conn, 0, pck.dataSize(), 2);
            conn.timeout = 10000;
            pck.clear();
            if (debugger.clntDnsTraf) {
                logger.debug("tx " + srv + " " + query);
            }
            int i = pck.pipeRecv(conn, 0, 0, 3);
            conn.setClose();
            if (i < 1) {
                continue;
            }
            pck.putSkip(i);
            pck.merge2beg();
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
            loCache.addList(reply.answers);
            loCache.addList(reply.addition);
            loCache.addList(reply.servers);
            return false;
        }
        return true;
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
        packDnsZone res = new packDnsZone("results");
        String dom = "";
        for (;;) {
            if (nam.length() < 1) {
                break;
            }
            String a;
            int i = nam.lastIndexOf(".");
            if (i < 1) {
                a = nam;
                nam = "";
            } else {
                a = nam.substring(i + 1, nam.length());
                nam = nam.substring(0, i);
            }
            if (dom.length() > 0) {
                a += ".";
            }
            dom = a + dom;
            int ned = (nam.length() > 0) ? packDnsRec.typeNS : typ;
            if (doResolvList(srv, dom, ned)) {
                return null;
            }
            packDnsRec rec = findAnswer(ned);
            if (rec == null) {
                return null;
            }
            res.addBin(rec);
            if (nam.length() < 1) {
                break;
            }
            addrIP adr = userTerminal.justResolv(rec.target, curPrx.proxy.prefer);
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
        rrF.name = zon.name;
        rrF.typ = packDnsRec.typeAXFR;
        query.queries.add(rrF);
        query.id = bits.randomW();
        query.opcode = packDns.opcodeQuery;
        query.recDsrd = true;
        query.result = packDns.resultSuccess;
        pipeSide conn = curPrx.doConnect(servGeneric.protoTcp, srv, packDns.portNum, "dns");
        if (conn == null) {
            return null;
        }
        packSize blck = new packSize(conn, 2, true, 1, 0);
        query.createHeader(pck);
        blck.sendPacket(pck);
        conn.timeout = 60000;
        if (debugger.clntDnsTraf) {
            logger.debug("tx " + query);
        }
        packDnsZone zon2 = new packDnsZone(zon.name);
        zon2.defttl = zon.defttl;
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
        if ((!end) || (rrF == null) || (rrL == null)) {
            return null;
        }
        if ((rrF.typ != packDnsRec.typeSOA) || (rrL.typ != packDnsRec.typeSOA)) {
            return null;
        }
        if (rrF.sequence != rrL.sequence) {
            return null;
        }
        packDnsRec rrO = zon.findUser(zon.name, packDnsRec.typeSOA);
        boolean dl = rrO == null;
        if (!dl) {
            dl = rrF.sequence != rrO.sequence;
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
     * @param typ type of query
     * @return false on success, true on error
     */
    public boolean doResolvList(List<addrIP> srv, String nam, int typ) {
        if (srv == null) {
            return true;
        }
        for (int i = 0; i < srv.size(); i++) {
            if (doResolvOne(srv.get(i), nam, typ) == true) {
                continue;
            }
            return false;
        }
        return true;
    }

    private int getTypPri(int prefer) {
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

    private int getTypBck(int prefer) {
        if (getTypPri(prefer) == packDnsRec.typeA) {
            return packDnsRec.typeAAAA;
        } else {
            return packDnsRec.typeA;
        }
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
            rr.addr = adr;
            rr.typ = getTypPri(prefer);
            reply = new packDns();
            reply.answers.add(rr);
            return false;
        }
        if (doResolvList(srv, nam, getTypPri(prefer))) {
            return doResolvList(srv, nam, getTypBck(prefer));
        }
        if (reply.answers.size() < 1) {
            return doResolvList(srv, nam, getTypBck(prefer));
        }
        return false;
    }

    /**
     * get answers
     *
     * @param lst list to append
     * @return number of entries added
     */
    public int getAnswers(List<packDnsRec> lst) {
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
            rr = findAnswer(getTypBck(prefer));
        }
        if (rr == null) {
            return null;
        }
        return rr.addr;
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
        return rr.target;
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
        return rr.target;
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
        return rr.target;
    }

}
