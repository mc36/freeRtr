package org.freertr.rtr;

import org.freertr.addr.addrIPv4;
import org.freertr.addr.addrIPv6;
import org.freertr.addr.addrPrefix;
import org.freertr.ip.ipCor4;
import org.freertr.ip.ipCor6;
import org.freertr.pack.packHolder;
import org.freertr.pipe.pipeSide;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabRoautNtry;
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
     * type
     */
    public int typ;

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
    public tabRoautNtry roa;

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
        return "typ=" + type2string(typ) + " sess=" + sess + " serial=" + serial + " wd=" + withdraw + " roa=" + roa;
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
        if (pck.getByte(0) != 0) { // version
            return true;
        }
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
                roa = new tabRoautNtry();
                withdraw = (pck.getByte(0) & 1) == 0; // flags
                roa.max = pck.getByte(2); // max
                addrIPv4 adr4 = new addrIPv4();
                pck.getAddr(adr4, 4); // address
                addrPrefix<addrIPv4> pref4 = new addrPrefix<addrIPv4>(adr4, pck.getByte(1));
                roa.prefix = addrPrefix.ip4toIP(pref4);
                roa.asn = pck.msbGetD(8); // as
                break;
            case msgIpv6addr:
                roa = new tabRoautNtry();
                withdraw = (pck.getByte(0) & 1) == 0; // flags
                roa.max = pck.getByte(2); // max
                addrIPv6 adr6 = new addrIPv6();
                pck.getAddr(adr6, 4); // address
                addrPrefix<addrIPv6> pref6 = new addrPrefix<addrIPv6>(adr6, pck.getByte(1));
                roa.prefix = addrPrefix.ip6toIP(pref6);
                roa.asn = pck.msbGetD(20); // as
                break;
            case msgEndData:
                serial = pck.msbGetD(0); // serial
                break;
            case msgCacheReset:
                break;
            case msgRouterKey:
                break;
            case msgErrorReport:
                break;
            default:
                return true;
        }
        if (debugger.rtrRpkiTraf) {
            logger.debug("rx " + pck.dump());
        }
        return false;
    }

    /**
     * send packet
     */
    public void sendPack() {
        pck.clear();
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
                pck.msbPutD(8, roa.asn); // as
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
                pck.msbPutD(20, roa.asn); // as
                pck.putSkip(24);
                break;
            case msgEndData:
                pck.msbPutD(0, serial); // serial
                pck.putSkip(4);
                break;
            case msgCacheReset:
                break;
            case msgRouterKey:
                break;
            case msgErrorReport:
                break;
            default:
                return;
        }
        pck.merge2beg();
        pck.putByte(0, 0); // version
        pck.putByte(1, typ); // type
        pck.msbPutW(2, sess); // session id
        pck.msbPutD(4, pck.dataSize() + size); // length
        pck.putSkip(size);
        pck.merge2beg();
        pck.pipeSend(conn, 0, pck.dataSize(), 2);
        cntr.tx(pck);
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
    public void sendOneTable(int tp, tabGen<tabRoautNtry> tab) {
        if (tab == null) {
            return;
        }
        if (debugger.rtrRpkiEvnt) {
            logger.info("sending " + tab.size());
        }
        for (int i = 0; i < tab.size(); i++) {
            tabRoautNtry ntry = tab.get(i);
            if (ntry == null) {
                continue;
            }
            typ = tp;
            roa = ntry;
            sendPack();
            if (debugger.servRpkiTraf) {
                logger.debug("tx " + dump());
            }
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
        sendOneTable(rtrRpkiSpeak.msgIpv4addr, rtr.getFinalTab(ipCor4.protocolVersion));
        sendOneTable(rtrRpkiSpeak.msgIpv6addr, rtr.getFinalTab(ipCor6.protocolVersion));
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
     * do one server round
     *
     * @param seq sequence to use
     * @param ses session to use
     * @param tab4 ipv4 table
     * @param tab6 ipv6 table
     * @param rtr rpki to send
     * @return true on error, false on success
     */
    public boolean doOneServRnd(int seq, int ses, tabGen<tabRoautNtry> tab4, tabGen<tabRoautNtry> tab6, rtrRpki rtr) {
        if (recvPack()) {
            return true;
        }
        int csq = getRpkiSeq(rtr) + seq;
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
                sendOneRpki(rtr);
                sendOneTable(rtrRpkiSpeak.msgIpv4addr, tab4);
                sendOneTable(rtrRpkiSpeak.msgIpv6addr, tab6);
                typ = rtrRpkiSpeak.msgEndData;
                serial = csq;
                sendPack();
                return false;
            default:
                return false;
        }
    }

}
