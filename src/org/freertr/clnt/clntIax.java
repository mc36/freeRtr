package org.freertr.clnt;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.cfg.cfgDial;
import org.freertr.cfg.cfgIfc;
import org.freertr.cfg.cfgVrf;
import org.freertr.enc.encCallConn;
import org.freertr.enc.encCallHnd;
import org.freertr.enc.encCallOne;
import org.freertr.enc.encCodec;
import org.freertr.enc.encCodecG711aLaw;
import org.freertr.enc.encCodecG711uLaw;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipFwdIface;
import org.freertr.ip.ipFwdTab;
import org.freertr.pack.packHolder;
import org.freertr.pack.packIax;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGen;
import org.freertr.tab.tabGen;
import org.freertr.util.bits;
import org.freertr.util.debugger;
import org.freertr.util.logger;
import org.freertr.util.syncInt;

/**
 * inter asterisk exchange (rfc5456) client
 *
 * @author matecsaba
 */
public class clntIax implements Runnable, encCallHnd {

    /**
     * create instance
     */
    public clntIax() {
    }

    /**
     * upper
     */
    public cfgDial upper;

    /**
     * client or server
     */
    public boolean client;

    /**
     * local port
     */
    public int portLoc = 0;

    /**
     * remote port
     */
    public int portRem = packIax.port;

    /**
     * codec, true=alaw, false=ulaw
     */
    public boolean aLaw = true;

    /**
     * keepalive interval
     */
    public int keepalive = 0;

    /**
     * register interval
     */
    public int register = 0;

    /**
     * vrf of target
     */
    public cfgVrf vrf = null;

    /**
     * source interface
     */
    public cfgIfc srcIfc = null;

    /**
     * target
     */
    public String trgDom;

    /**
     * username
     */
    public String usr;

    /**
     * password
     */
    public String pwd;

    /**
     * udp handler
     */
    protected prtGen udp;

    /**
     * forwarder interface
     */
    protected ipFwdIface srcFwd;

    /**
     * connection to peer
     */
    protected pipeSide conn;

    private final tabGen<clntIaxCall> cls = new tabGen<clntIaxCall>();

    private ipFwd fwd;

    private addrIP trgAdr;

    /**
     * keepalive
     */
    protected clntIaxKeep timKeep;

    /**
     * need to run
     */
    protected boolean need2run;

    private int seqRx;

    private int seqTx;

    /**
     * start work
     */
    public void startWork() {
        need2run = true;
        seqRx = 0;
        seqTx = 0;
        logger.startThread(this);
        if ((keepalive + register) < 1) {
            return;
        }
        timKeep = new clntIaxKeep(this);
        timKeep.start();
    }

    /**
     * stop work
     */
    public void stopWork() {
        need2run = false;
        timKeep = null;
        if (conn != null) {
            conn.setClose();
        }
        conn = null;
        for (int i = 0; i < cls.size(); i++) {
            try {
                cls.get(i).setClose();
            } catch (Exception e) {
            }
        }
    }

    /**
     * get codec
     *
     * @return codec
     */
    protected encCodec getCodec() {
        if (aLaw) {
            return new encCodecG711aLaw();
        } else {
            return new encCodecG711uLaw();
        }
    }

    private int findLocCid(int o) {
        for (int i = 0; i < cls.size(); i++) {
            if (cls.get(i).lid == o) {
                return i;
            }
        }
        return -1;
    }

    private int getLocCid() {
        for (;;) {
            int o = bits.random(0x10, 0x7ff0);
            if (findLocCid(o) >= 0) {
                continue;
            }
            return o;
        }
    }

    public void run() {
        if (debugger.clntIaxTraf) {
            logger.debug("started");
        }
        for (;;) {
            bits.sleep(1000);
            if (!need2run) {
                break;
            }
            try {
                doWork();
            } catch (Exception e) {
                logger.traceback(e);
            }
        }
        if (debugger.clntIaxTraf) {
            logger.debug("stopped");
        }
    }

    /**
     * delete the call
     *
     * @param leg call id
     */
    protected void delCall(clntIaxCall leg) {
        cls.del(leg);
    }

    /**
     * send register
     *
     * @param wau authentication request
     * @param pau authentication request
     */
    protected void sendReg(String wau, String pau) {
        if (conn == null) {
            return;
        }



    ////////////
    }

    /**
     * send keepalive
     */
    protected void sendKeep() {
        if (conn == null) {
            return;
        }



    ///////////////
    }

    /**
     * get number of in calls
     *
     * @param dir direction, true=in, false=out
     * @return number of calls
     */
    public int numCalls(boolean dir) {
        int o = 0;
        for (int i = 0; i < cls.size(); i++) {
            if (cls.get(i).dir == dir) {
                o++;
            }
        }
        return o;
    }

    /**
     * get number of out messages
     *
     * @param dir direction, true=in, false=out
     * @return number of messages
     */
    public int numMsgs(boolean dir) {
        return 0;
    }

    /**
     * check if ready
     *
     * @return true if yes, false if no
     */
    public boolean isReady() {
        if (conn == null) {
            return false;
        }
        if (conn.isClosed() != 0) {
            return false;
        }
        return conn.isReady() == 3;
    }

    /**
     * get call list
     *
     * @param dir direction, true=in, false=out
     * @return list
     */
    public List<String> listCalls(boolean dir) {
        List<String> res = new ArrayList<String>();
        for (int i = 0; i < cls.size(); i++) {
            clntIaxCall ntry = cls.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry.dir != dir) {
                continue;
            }
            res.add(ntry.rid + "-" + ntry.lid + "|" + ntry.src + "|" + ntry.trg + "|" + bits.timePast(ntry.started));
        }
        return res;
    }

    /**
     * send message
     *
     * @param calling calling number
     * @param called called number
     * @param msg message
     * @return false on success, true on error
     */
    public boolean sendMsg(String calling, String called, List<String> msg) {
        return true;
    }

    /**
     * make the call
     *
     * @param calling calling number
     * @param called called number
     * @return call id, null if error
     */
    public String makeCall(String calling, String called) {
        return null;



    /////////
    }

    /**
     * stop the call
     *
     * @param id call id
     */
    public void stopCall(String id) {
        if (id == null) {
            return;
        }



    /////////////
    }


    /**
     * get call
     *
     * @param cid call id
     * @return rtp
     */
    public encCallOne getCall(String cid) {
        if (cid == null) {
            return null;
        }
        //////
        return null;
    }

    /**
     * do one round
     */
    public void doWork() {
        if (conn != null) {
            conn.setClose();
        }
        conn = null;
        trgAdr = clntDns.justResolv(trgDom, 0);
        if (trgAdr == null) {
            return;
        }
        fwd = vrf.getFwd(trgAdr);
        udp = vrf.getUdp(trgAdr);
        if (srcIfc != null) {
            srcFwd = srcIfc.getFwdIfc(trgAdr);
        } else {
            srcFwd = ipFwdTab.findSendingIface(fwd, trgAdr);
        }
        if (srcFwd == null) {
            return;
        }
        conn = udp.streamConnect(new pipeLine(32768, true), srcFwd, portLoc, trgAdr, portRem, "iax", -1, null, -1, -1);
        if (conn == null) {
            return;
        }
        conn.setTime(180000);
        conn.lineRx = pipeSide.modTyp.modeCRtryLF;
        conn.lineTx = pipeSide.modTyp.modeCRLF;
        conn.setReady();
        conn.wait4ready(30000);
        if (conn.isReady() != 3) {
            conn.setClose();
            conn = null;
            return;
        }
        packIax iax = new packIax(conn);
        packHolder pck = new packHolder(true, true);
        for (;;) {
            if (!need2run) {
                break;
            }
            if (conn == null) {
                break;
            }
            if (iax.isClosed() != 0) {
                break;
            }
            if (iax.recvPack(pck) < 0) {
                continue;
            }
            clntIaxCall ntry = new clntIaxCall(this, iax.sid);
            clntIaxCall old = cls.find(ntry);
            if (iax.typ == packIax.typ_voc) {
                if (old != null) {
                    if ((iax.tid == -1) || (old.lid == iax.tid)) {
                        byte[] buf = pck.getCopy();
                        old.bufT.nonBlockPut(buf, 0, buf.length);
                        continue;
                    }
                }
            }
            if (debugger.clntIaxTraf) {
                iax.dump("rx");
            }
            iax.parseTlvs(pck);
            if (old != null) {
                old.seqI = iax.seqO;
                old.seqO = iax.seqI;
            }
            packIax rep = null;
            switch ((iax.typ << 8) | iax.sub) {
                case (packIax.typ_iax << 8) | packIax.iam_rrq: // reg req
                    if (client) {
                        break;
                    }
                    //// auth
                    rep = new packIax(conn);
                    rep.typ = packIax.typ_iax;
                    rep.sub = packIax.iam_rak;
                    rep.sid = 0;
                    rep.tid = iax.sid;
                    if (register > 0) {
                        rep.frsh = register / 1000;
                    } else {
                        rep.frsh = iax.frsh;
                    }
                    rep.user = iax.user;
                    break;
                case (packIax.typ_iax << 8) | packIax.iam_rrl: // reg rel
                    rep = new packIax(conn);
                    rep.typ = packIax.typ_iax;
                    rep.sub = packIax.iam_rak;
                    rep.sid = 0;
                    rep.tid = iax.sid;
                    break;
                case (packIax.typ_iax << 8) | packIax.iam_ack: // ack
                    if (old == null) {
                        continue;
                    }
                    if (old.que.get() == ((iax.seqI - 1) & 0xffff)) {
                        old.que.set(-1);
                    }
                    continue;
                case (packIax.typ_iax << 8) | packIax.iam_new: // new call
                    //// auth
                    if (old == null) {
                        old = ntry;
                        if (upper == null) {
                            rep = new packIax(conn);
                            rep.typ = packIax.typ_iax;
                            rep.sub = packIax.iam_rej;
                            rep.sid = 0;
                            rep.tid = iax.sid;
                            break;
                        }
                        old.dir = true;
                        old.src = iax.calling;
                        old.trg = iax.called;
                        old.lid = getLocCid();
                        old.start();
                        cls.put(old);
                    }
                    rep = new packIax(conn);
                    rep.typ = packIax.typ_iax;
                    rep.sub = packIax.iam_acc;
                    rep.times = (int) (bits.getTime() - old.started);
                    rep.codecD = upper.getCodec().getIAXtype();
                    rep.sid = old.lid;
                    rep.tid = old.rid;
                    break;
                case (packIax.typ_iax << 8) | packIax.iam_acc: // accept call
                    if (old == null) {
                        break;
                    }
                    rep = new packIax(conn);
                    rep.typ = packIax.typ_iax;
                    rep.sub = packIax.iam_ack;
                    rep.times = iax.times;
                    rep.sid = old.lid;
                    rep.tid = old.rid;
                    break;
                case (packIax.typ_iax << 8) | packIax.iam_pin: // ping call
                    if (old == null) {
                        break;
                    }
                    rep = new packIax(conn);
                    rep.typ = packIax.typ_iax;
                    rep.sub = packIax.iam_pon;
                    rep.times = (int) (bits.getTime() - old.started);
                    rep.sid = iax.tid;
                    rep.tid = iax.sid;
                    break;
                case (packIax.typ_iax << 8) | packIax.iam_pon: // pong call
                    if (old == null) {
                        break;
                    }
                    rep = new packIax(conn);
                    rep.typ = packIax.typ_iax;
                    rep.sub = packIax.iam_ack;
                    rep.times = iax.times;
                    rep.sid = old.lid;
                    rep.tid = old.rid;
                    break;
            }
            if (rep == null) {
                rep = new packIax(conn);
                rep.typ = packIax.typ_iax;
                rep.sub = packIax.iam_inv;
                rep.sid = 0;
                rep.tid = iax.sid;
            }
            rep.seqI = iax.seqO + 1;
            rep.seqO = iax.seqI;
            if (debugger.clntIaxTraf) {
                rep.dump("tx");
            }
            rep.placeTlvs(pck);
            rep.sendPack(pck);
        }
        if (conn != null) {
            conn.setClose();
        }
        conn = null;
        if (debugger.clntIaxTraf) {
            logger.debug("restarting");
        }
    }

}

class clntIaxKeep implements Runnable {

    private final clntIax lower;

    public clntIaxKeep(clntIax parent) {
        lower = parent;
    }

    public void start() {
        logger.startThread(this);
    }

    public void run() {
        try {
            long lastKep = 0;
            long lastReg = 0;
            for (;;) {
                bits.sleep(1000);
                if (lower.timKeep != this) {
                    break;
                }
                if (!lower.need2run) {
                    break;
                }
                long tim = bits.getTime();
                if ((lower.keepalive > 0) && ((tim - lastKep) > lower.keepalive)) {
                    lower.sendKeep();
                    lastKep = tim;
                }
                if ((lower.register > 0) && ((tim - lastReg) > lower.register)) {
                    lower.sendReg(null, null);
                    lastReg = tim;
                }
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

}

class clntIaxCall implements Runnable, Comparable<clntIaxCall>, encCallOne {

    public final clntIax lower;

    public final int rid;

    public final long started;

    public boolean need2run;

    public boolean dir;

    public int seqO;

    public int seqI;

    public syncInt que;

    public int lid;

    public String src;

    public String trg;

    public String newSrc;

    public String newTrg;

    public cfgDial peer;

    public String rcd;

    public packIax snd;

    public encCallOne call;

    public encCallConn conner;

    public encCodec codec;

    public pipeLine bufP;

    public pipeSide bufR;

    public pipeSide bufT;

    public boolean full;

    public clntIaxCall(clntIax prnt, int rem) {
        lower = prnt;
        rid = rem;
        started = bits.getTime();
    }

    public int compareTo(clntIaxCall o) {
        if (rid < o.rid) {
            return -1;
        }
        if (rid > o.rid) {
            return +1;
        }
        return 0;
    }

    public void start() {
        need2run = true;
        que = new syncInt(-1);
        snd = new packIax(lower.conn);
        bufP = new pipeLine(65535, true);
        bufR = bufP.getSide();
        bufT = bufP.getSide();
        full = false;
        logger.startThread(this);
    }

    public void run() {
        try {
            doer();
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

    public void doCtr(int typ, boolean ned) {
        need2run = ned;
        packIax iax = new packIax(lower.conn);
        packHolder pck = new packHolder(true, true);
        for (int i = 0; i < 8; i++) {
            iax.clear();
            pck.clear();
            iax.typ = packIax.typ_ctr;
            iax.sub = typ;
            iax.seqI = seqI;
            iax.seqO = seqO;
            iax.sid = lid;
            iax.tid = rid;
            iax.times = (int) (bits.getTime() - started);
            que.set(seqO);
            iax.sendPack(pck);
            if (debugger.clntIaxTraf) {
                iax.dump("tx");
            }
            bits.sleep(3000);
            if (que.get() < 0) {
                break;
            }
        }
    }

    public void doer() {
        newSrc = lower.upper.incomeSrc(src);
        newTrg = lower.upper.incomeTrg(trg);
        peer = lower.upper.incomeCall(newSrc, newTrg);
        if (peer == null) {
            doCtr(packIax.ctr_hng, false);
            lower.upper.stoppedCall(dir, newSrc, newTrg, started);
            lower.delCall(this);
            return;
        }
        rcd = peer.makeCall(newSrc, newTrg);
        if (rcd == null) {
            doCtr(packIax.ctr_hng, false);
            lower.upper.stoppedCall(dir, newSrc, newTrg, started);
            lower.delCall(this);
            return;
        }
        doCtr(packIax.ctr_ans, true);
        codec = lower.getCodec();
        call = peer.getCall(rcd);
        conner = new encCallConn(this, call, codec, peer.getCodec());
        for (;;) {
            if (conner.isClosed() != 0) {
                break;
            }
            if (lower.conn == null) {
                break;
            }
            if (!need2run) {
                break;
            }
            bits.sleep(1000);
        }
        conner.setClose();
        peer.stopCall(rcd);
        doCtr(packIax.ctr_hng, false);
        lower.upper.stoppedCall(dir, newSrc, newTrg, started);
        lower.delCall(this);
    }

    public void setClose() {
        need2run = false;
    }

    public int isClosed() {
        if (need2run) {
            return 0;
        } else {
            return 3;
        }
    }

    public void sendPack(packHolder pck) {
        if (!need2run) {
            return;
        }
        snd.clear();
        pck.merge2beg();
        snd.times = (int) (bits.getTime() - started);
        snd.sid = lid;
        if (full) {
            snd.tid = -1;
            snd.sendPack(pck);
            return;
        }
        snd.tid = rid;
        snd.typ = packIax.typ_voc;
        snd.sub = codec.getIAXtype();
        snd.seqI = seqI;
        snd.seqO = seqO;
        full = true;
        seqO++;
        snd.sendPack(pck);
    }

    public int recvPack(packHolder pck, boolean blocking, boolean enforce) {
        int i;
        if (blocking) {
            i = 143;
        } else {
            i = 142;
        }
        return pck.pipeRecv(bufR, 0, -1, i);
    }

}
