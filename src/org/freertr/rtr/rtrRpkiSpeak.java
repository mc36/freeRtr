package org.freertr.rtr;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import org.freertr.addr.addrIPv4;
import org.freertr.addr.addrIPv6;
import org.freertr.addr.addrPrefix;
import org.freertr.enc.encJson;
import org.freertr.ip.ipCor4;
import org.freertr.ip.ipCor6;
import org.freertr.pack.packHolder;
import org.freertr.pipe.pipeSide;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabRpkiAspa;
import org.freertr.tab.tabRpkiKey;
import org.freertr.tab.tabRpkiRoa;
import org.freertr.util.bits;
import org.freertr.util.counter;
import org.freertr.util.debugger;
import org.freertr.util.logger;

/**
 * resource public key infrastructure (rfc6810) speaker
 *
 * @author matecsaba
 */
public class rtrRpkiSpeak {

    /**
     * port number
     */
    public final static int portNum = 323;

    /**
     * size of header
     */
    public final static int size = 8;

    /**
     * serial notify
     */
    public final static int msgSerialNotify = 0;

    /**
     * serial query
     */
    public final static int msgSerialQuery = 1;

    /**
     * reset query
     */
    public final static int msgResetQuery = 2;

    /**
     * cache response
     */
    public final static int msgCacheReply = 3;

    /**
     * ipv4 prefix
     */
    public final static int msgIpv4addr = 4;

    /**
     * ipv6 prefix
     */
    public final static int msgIpv6addr = 6;

    /**
     * end of data
     */
    public final static int msgEndData = 7;

    /**
     * cache reset
     */
    public final static int msgCacheReset = 8;

    /**
     * router key
     */
    public final static int msgRouterKey = 9;

    /**
     * error report
     */
    public final static int msgErrorReport = 10;

    /**
     * aspa pdu
     */
    public final static int msgAspaPdu = 11;

    /**
     * type
     */
    public int typ;

    /**
     * version
     */
    public int ver;

    /**
     * session id
     */
    public int sess;

    /**
     * serial number
     */
    public int serial;

    /**
     * withdraw
     */
    public boolean withdraw;

    /**
     * route origin authorization
     */
    public tabRpkiRoa roa;

    /**
     * subject key identifier
     */
    public tabRpkiKey key;

    /**
     * as path authorization
     */
    public tabRpkiAspa aspa;

    private final packHolder pck;

    private final pipeSide conn;

    /**
     * counter
     */
    public final counter cntr;

    /**
     * create instance
     *
     * @param tmp buffer to use
     * @param pip connection to use
     * @param cnt counter to use
     */
    public rtrRpkiSpeak(packHolder tmp, pipeSide pip, counter cnt) {
        pck = tmp;
        conn = pip;
        cntr = cnt;
    }

    /**
     * dump packet
     *
     * @return string
     */
    public String dump() {
        return "typ=" + type2string(typ) + " sess=" + sess + " serial=" + serial + " wd=" + withdraw + " roa=" + roa + " aspa=" + aspa;
    }

    /**
     * convert type to string
     *
     * @param i type
     * @return string
     */
    public final static String type2string(int i) {
        switch (i) {
            case msgSerialNotify:
                return "serialNotify";
            case msgSerialQuery:
                return "serialQuery";
            case msgResetQuery:
                return "resetQuery";
            case msgCacheReply:
                return "cacheReply";
            case msgIpv4addr:
                return "ipv4prefix";
            case msgIpv6addr:
                return "ipv6prefix";
            case msgEndData:
                return "end";
            case msgCacheReset:
                return "cacheReset";
            case msgRouterKey:
                return "routerKey";
            case msgErrorReport:
                return "error";
            case msgAspaPdu:
                return "aspa";
            default:
                return "unknown=" + i;
        }
    }

    /**
     * receive packet
     *
     * @return false on success, true on error
     */
    public boolean recvPack() {
        pck.clear();
        if (pck.pipeRecv(conn, 0, size, 144) != size) {
            return true;
        }
        ver = pck.getByte(0); // version
        typ = pck.getByte(1); // type
        sess = pck.msbGetW(2); // session id
        int len = pck.msbGetD(4); // length
        if (len < size) {
            return true;
        }
        len -= size;
        pck.clear();
        if (len > 0) {
            if (pck.pipeRecv(conn, 0, len, 144) != len) {
                return true;
            }
        }
        cntr.rx(pck);
        switch (typ) {
            case msgSerialNotify:
                serial = pck.msbGetD(0); // serial
                break;
            case msgSerialQuery:
                serial = pck.msbGetD(0); // serial
                break;
            case msgResetQuery:
                break;
            case msgCacheReply:
                break;
            case msgIpv4addr:
                roa = new tabRpkiRoa();
                withdraw = (pck.getByte(0) & 1) == 0; // flags
                roa.max = pck.getByte(2); // max
                addrIPv4 adr4 = new addrIPv4();
                pck.getAddr(adr4, 4); // address
                addrPrefix<addrIPv4> pref4 = new addrPrefix<addrIPv4>(adr4, pck.getByte(1));
                roa.prefix = addrPrefix.ip4toIP(pref4);
                roa.distan = pck.msbGetD(8); // as
                break;
            case msgIpv6addr:
                roa = new tabRpkiRoa();
                withdraw = (pck.getByte(0) & 1) == 0; // flags
                roa.max = pck.getByte(2); // max
                addrIPv6 adr6 = new addrIPv6();
                pck.getAddr(adr6, 4); // address
                addrPrefix<addrIPv6> pref6 = new addrPrefix<addrIPv6>(adr6, pck.getByte(1));
                roa.prefix = addrPrefix.ip6toIP(pref6);
                roa.distan = pck.msbGetD(20); // as
                break;
            case msgEndData:
                serial = pck.msbGetD(0); // serial
                break;
            case msgCacheReset:
                break;
            case msgRouterKey:
                key = new tabRpkiKey();
                withdraw = (sess & 0x100) == 0; // flags
                key.ski = new byte[20];
                pck.getCopy(key.ski, 0, 0, key.ski.length);
                pck.getSkip(key.ski.length);
                key.asn = pck.msbGetD(0);
                pck.getSkip(4);
                key.key = pck.getCopy();
                break;
            case msgErrorReport:
                break;
            case msgAspaPdu:
                aspa = new tabRpkiAspa();
                aspa.provs = new ArrayList<Integer>();
                withdraw = (sess & 0x100) == 0; // flags
                aspa.cust = pck.msbGetD(0);
                for (;;) {
                    pck.getSkip(4);
                    if (pck.dataSize() < 4) {
                        break;
                    }
                    aspa.provs.add(pck.msbGetD(0));
                }
                Collections.sort(aspa.provs);
                break;
            default:
                return true;
        }
        if (debugger.rtrRpkiTraf) {
            logger.debug("rx " + dump());
        }
        return false;
    }

    /**
     * send packet
     */
    public void sendPack() {
        pck.clear();
        int oldSess = sess;
        switch (typ) {
            case msgSerialNotify:
                pck.msbPutD(0, serial); // serial
                pck.putSkip(4);
                break;
            case msgSerialQuery:
                pck.msbPutD(0, serial); // serial
                pck.putSkip(4);
                break;
            case msgResetQuery:
                break;
            case msgCacheReply:
                break;
            case msgIpv4addr:
                if (withdraw) {
                    pck.putByte(0, 0);
                } else {
                    pck.putByte(0, 1);
                }
                addrPrefix<addrIPv4> pref4 = addrPrefix.ip2ip4(roa.prefix);
                pck.putByte(1, pref4.maskLen); // prefix length
                pck.putByte(2, roa.max); // max
                pck.putByte(3, 0); // reserved
                pck.putAddr(4, pref4.network); // address
                pck.msbPutD(8, roa.distan); // as
                pck.putSkip(12);
                break;
            case msgIpv6addr:
                if (withdraw) {
                    pck.putByte(0, 0);
                } else {
                    pck.putByte(0, 1);
                }
                addrPrefix<addrIPv6> pref6 = addrPrefix.ip2ip6(roa.prefix);
                pck.putByte(1, pref6.maskLen); // prefix length
                pck.putByte(2, roa.max); // max
                pck.putByte(3, 0); // reserved
                pck.putAddr(4, pref6.network); // address
                pck.msbPutD(20, roa.distan); // as
                pck.putSkip(24);
                break;
            case msgEndData:
                pck.msbPutD(0, serial); // serial
                pck.putSkip(4);
                break;
            case msgCacheReset:
                break;
            case msgRouterKey:
                if (withdraw) {
                    sess = 0;
                } else {
                    sess = 0x100;
                }
                pck.putCopy(key.ski, 0, 0, key.ski.length);
                pck.putSkip(key.ski.length);
                pck.msbPutD(0, key.asn);
                pck.putSkip(4);
                pck.putCopy(key.key, 0, 0, key.key.length);
                pck.putSkip(key.key.length);
                break;
            case msgErrorReport:
                break;
            case msgAspaPdu:
                if (withdraw) {
                    sess = 0;
                } else {
                    sess = 0x100;
                }
                pck.msbPutD(0, aspa.cust);
                pck.putSkip(4);
                for (int i = 0; i < aspa.provs.size(); i++) {
                    pck.msbPutD(0, aspa.provs.get(i));
                    pck.putSkip(4);
                }
                break;
            default:
                return;
        }
        pck.merge2beg();
        pck.putByte(0, ver); // version
        pck.putByte(1, typ); // type
        pck.msbPutW(2, sess); // session id
        pck.msbPutD(4, pck.dataSize() + size); // length
        pck.putSkip(size);
        pck.merge2beg();
        pck.pipeSend(conn, 0, pck.dataSize(), 2);
        cntr.tx(pck);
        sess = oldSess;
        if (debugger.rtrRpkiTraf) {
            logger.debug("tx " + dump());
        }
    }

    /**
     * send one table
     *
     * @param tp type to use
     * @param tab table to send
     */
    public void sendOneTableRoa(int tp, tabGen<tabRpkiRoa> tab) {
        if (tab == null) {
            return;
        }
        if (debugger.rtrRpkiEvnt) {
            logger.debug("sending " + tab.size());
        }
        for (int i = 0; i < tab.size(); i++) {
            tabRpkiRoa ntry = tab.get(i);
            if (ntry == null) {
                continue;
            }
            roa = ntry.copyBytes();
            typ = tp;
            for (int o = 0; o < roa.asns.size(); o++) {
                roa.distan = roa.asns.get(o);
                sendPack();
            }
        }
    }

    /**
     * send one table
     *
     * @param tab table to send
     */
    public void sendOneTableAspa(tabGen<tabRpkiAspa> tab) {
        if (tab == null) {
            return;
        }
        if (debugger.rtrRpkiEvnt) {
            logger.debug("sending " + tab.size());
        }
        for (int i = 0; i < tab.size(); i++) {
            tabRpkiAspa ntry = tab.get(i);
            if (ntry == null) {
                continue;
            }
            aspa = ntry.copyBytes();
            typ = rtrRpkiSpeak.msgAspaPdu;
            sendPack();
        }
    }

    /**
     * send one table
     *
     * @param tab table to send
     */
    public void sendOneTableKey(tabGen<tabRpkiKey> tab) {
        if (tab == null) {
            return;
        }
        if (debugger.rtrRpkiEvnt) {
            logger.debug("sending " + tab.size());
        }
        for (int i = 0; i < tab.size(); i++) {
            tabRpkiKey ntry = tab.get(i);
            if (ntry == null) {
                continue;
            }
            key = ntry.copyBytes();
            typ = rtrRpkiSpeak.msgRouterKey;
            sendPack();
        }
    }

    /**
     * send one rpki computed table
     *
     * @param rtr rpki router
     */
    public void sendOneRpki(rtrRpki rtr) {
        if (rtr == null) {
            return;
        }
        sendOneTableRoa(rtrRpkiSpeak.msgIpv4addr, rtr.getFinalTabRoa(ipCor4.protocolVersion));
        sendOneTableRoa(rtrRpkiSpeak.msgIpv6addr, rtr.getFinalTabRoa(ipCor6.protocolVersion));
        sendOneTableAspa(rtr.getFinalTabAspa());
        sendOneTableKey(rtr.getFinalTabKey());
    }

    /**
     * send one json table
     *
     * @param fn file to send
     */
    public void sendOneJson(String fn) {
        if (fn == null) {
            return;
        }
        List<String> txt = bits.txt2buf(fn);
        if (txt == null) {
            return;
        }
        if (debugger.rtrRpkiEvnt) {
            logger.debug("sending " + txt.size());
        }
        encJson j = new encJson();
        roa = new tabRpkiRoa();
        key = new tabRpkiKey();
        aspa = new tabRpkiAspa();
        for (int i = 0; i < txt.size(); i++) {
            j.clear();
            j.fromString(txt.get(i));
            if (roa.fromJson(j)) {
                if (aspa.fromJson(j)) {
                    if (key.fromJson(j)) {
                        continue;
                    }
                    typ = rtrRpkiSpeak.msgRouterKey;
                    sendPack();
                    continue;
                }
                typ = rtrRpkiSpeak.msgAspaPdu;
                sendPack();
                continue;
            }
            if (roa.prefix.network.isIPv4()) {
                typ = rtrRpkiSpeak.msgIpv4addr;
            } else {
                typ = rtrRpkiSpeak.msgIpv6addr;
            }
            sendPack();
        }
    }

    /**
     * get rpki process sequence number
     *
     * @param rtr process to read
     * @return sequence number or 0 if nothing
     */
    public final static int getRpkiSeq(rtrRpki rtr) {
        if (rtr == null) {
            return 0;
        }
        return rtr.getSeqNum();
    }

    /**
     * get json file sequence number
     *
     * @param fn file to send
     * @return sequence number or 0 if nothing
     */
    public final static int getJsonSeq(String fn) {
        if (fn == null) {
            return 0;
        }
        try {
            long res = new File(fn).lastModified();
            res = (res >>> 32) ^ res;
            return (int) res;
        } catch (Exception e) {
            return 0;
        }
    }

    /**
     * do one server round
     *
     * @param seq sequence to use
     * @param ses session to use
     * @param tab4 ipv4 table
     * @param tab6 ipv6 table
     * @param tabA aspa table
     * @param tabK key table
     * @param rtr rpki to send
     * @param jsn json to send
     * @return true on error, false on success
     */
    public boolean doOneServRnd(int seq, int ses, tabGen<tabRpkiRoa> tab4, tabGen<tabRpkiRoa> tab6, tabGen<tabRpkiAspa> tabA, tabGen<tabRpkiKey> tabK, rtrRpki rtr, String jsn) {
        if (recvPack()) {
            return true;
        }
        if (debugger.rtrRpkiTraf) {
            logger.debug("rx " + dump());
        }
        int csq = getJsonSeq(jsn) + getRpkiSeq(rtr) + seq;
        switch (typ) {
            case rtrRpkiSpeak.msgSerialQuery:
                if (serial != csq) {
                    typ = rtrRpkiSpeak.msgCacheReset;
                    sendPack();
                    return false;
                }
                typ = rtrRpkiSpeak.msgCacheReply;
                sess = ses;
                sendPack();
                typ = rtrRpkiSpeak.msgEndData;
                sendPack();
                return false;
            case rtrRpkiSpeak.msgResetQuery:
                typ = rtrRpkiSpeak.msgCacheReply;
                sess = ses;
                sendPack();
                sendOneJson(jsn);
                sendOneRpki(rtr);
                sendOneTableRoa(rtrRpkiSpeak.msgIpv4addr, tab4);
                sendOneTableRoa(rtrRpkiSpeak.msgIpv6addr, tab6);
                sendOneTableAspa(tabA);
                sendOneTableKey(tabK);
                typ = rtrRpkiSpeak.msgEndData;
                serial = csq;
                sendPack();
                return false;
            default:
                return false;
        }
    }

}
