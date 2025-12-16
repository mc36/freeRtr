package org.freertr.serv;

import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.cfg.cfgAll;
import org.freertr.pack.packHolder;
import org.freertr.pack.packNtp;
import org.freertr.pack.packTwamp;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGenConn;
import org.freertr.prt.prtServP;
import org.freertr.tab.tabGen;
import org.freertr.user.userFilter;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.counter;
import org.freertr.util.state;

/**
 * two way measurement protocol (rfc5357) server
 *
 * @author matecsaba
 */
public class servTwamp extends servGeneric implements prtServP {

    /**
     * create instance
     */
    public servTwamp() {
    }

    /**
     * timeout
     */
    public int timeout = 10000;

    /**
     * list of connections
     */
    public tabGen<servTwampConn> conns = new tabGen<servTwampConn>();

    /**
     * defaults text
     */
    public final static userFilter[] defaultF = {
        new userFilter("server twamp .*", cmds.tabulator + "port " + packTwamp.port, null),
        new userFilter("server twamp .*", cmds.tabulator + "protocol " + proto2string(protoAllDgrm), null),
        new userFilter("server twamp .*", cmds.tabulator + "timeout 10000", null)
    };

    public userFilter[] srvDefFlt() {
        return defaultF;
    }

    /**
     * find one connection
     *
     * @param id connection id
     * @param create set true to create if none found
     * @return connection entry
     */
    public servTwampConn connFind(prtGenConn id, boolean create) {
        servTwampConn ntry = new servTwampConn(id, this);
        if (!create) {
            return conns.find(ntry);
        }
        servTwampConn old = conns.add(ntry);
        if (old != null) {
            return old;
        }
        return ntry;
    }

    /**
     * delete one connection
     *
     * @param id connection id
     * @return connection entry
     */
    public servTwampConn connDel(prtGenConn id) {
        servTwampConn ntry = new servTwampConn(id, this);
        return conns.del(ntry);
    }

    public void srvShRun(String beg, List<String> l, int filter) {
        l.add(beg + "timeout " + timeout);
    }

    public boolean srvCfgStr(cmds cmd) {
        String s = cmd.word();
        if (s.equals("timeout")) {
            timeout = bits.str2num(cmd.word());
            return false;
        }
        return true;
    }

    public void srvHelp(userHelp l) {
        l.add(null, false, 1, new int[]{2}, "timeout", "timeout of client");
        l.add(null, false, 2, new int[]{-1}, "<num>", "milliseconds");
    }

    public String srvName() {
        return "twamp";
    }

    public int srvPort() {
        return packTwamp.port;
    }

    public int srvProto() {
        return protoAllDgrm;
    }

    public boolean srvInit() {
        return genDgrmStart(this, 0);
    }

    public boolean srvDeinit() {
        return genericStop(0);
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        id.timeout = timeout;
        connFind(id, true);
        return false;
    }

    /**
     * connection ready
     *
     * @param id connection
     */
    public void datagramReady(prtGenConn id) {
    }

    /**
     * stop connection
     *
     * @param id connection
     */
    public void datagramClosed(prtGenConn id) {
        connDel(id);
    }

    /**
     * work connection
     *
     * @param id connection
     */
    public void datagramWork(prtGenConn id) {
        servTwampConn ntry = connFind(id, false);
        if (ntry == null) {
            id.setClosing();
            return;
        }
    }

    /**
     * received error
     *
     * @param id connection
     * @param pck packet
     * @param rtr reporting router
     * @param err error happened
     * @param lab error label
     * @return false on success, true on error
     */
    public boolean datagramError(prtGenConn id, packHolder pck, addrIP rtr, counter.reasons err, int lab) {
        return false;
    }

    /**
     * notified that state changed
     *
     * @param id id number to reference connection
     * @param stat state
     * @return return false if successful, true if error happened
     */
    public boolean datagramState(prtGenConn id, state.states stat) {
        return false;
    }

    /**
     * received packet
     *
     * @param id connection
     * @param pck packet
     * @return false on success, true on error
     */
    public boolean datagramRecv(prtGenConn id, packHolder pck) {
        servTwampConn ntry = connFind(id, false);
        if (ntry == null) {
            id.setClosing();
            return false;
        }
        ntry.doRecv(pck);
        return false;
    }

}

class servTwampConn implements Comparable<servTwampConn> {

    public prtGenConn conn;

    public servTwamp lower;

    public int seq;

    public int compareTo(servTwampConn o) {
        return conn.compareTo(o.conn);
    }

    public servTwampConn(prtGenConn id, servTwamp parent) {
        conn = id;
        lower = parent;
    }

    public String toString() {
        return "twamp with " + conn.peerAddr;
    }

    public void doRecv(packHolder pck) {
        packTwamp twm = new packTwamp();
        if (twm.parseHeader(pck)) {
            return;
        }
        seq++;
        twm.seqTx = twm.sequence;
        twm.timesTx = twm.timestmp;
        twm.errTx = twm.errEst;
        twm.sequence = seq;
        twm.timestmp = packNtp.encode(bits.getTime() + cfgAll.timeServerOffset);
        twm.errEst = packTwamp.errMag;
        twm.timesRx = twm.timestmp;
        twm.createHeader(pck);
        conn.sendTTL = twm.ttlTx;
        conn.send2net(pck);
    }

}
