package org.freertr.clnt;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.cfg.cfgDial;
import org.freertr.cfg.cfgIfc;
import org.freertr.cfg.cfgVrf;
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

    private final tabGen<clntIaxCall> outs = new tabGen<clntIaxCall>();

    private final tabGen<clntIaxCall> ins = new tabGen<clntIaxCall>();

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
        for (int i = 0; i < outs.size(); i++) {
            try {
                outs.get(i).setClose();
            } catch (Exception e) {
            }
        }
        for (int i = 0; i < ins.size(); i++) {
            try {
                ins.get(i).setClose();
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
     * @param dir direction, true=in, false=out
     * @param leg call id
     */
    protected void delCall(boolean dir, clntIaxCall leg) {
        if (dir) {
            ins.del(leg);
        } else {
            outs.del(leg);
        }
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
        if (dir) {
            return ins.size();
        } else {
            return outs.size();
        }
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
        tabGen<clntIaxCall> lst;
        if (dir) {
            lst = ins;
        } else {
            lst = outs;
        }
        for (int i = 0; i < lst.size(); i++) {
            clntIaxCall ntry = lst.get(i);
            if (ntry == null) {
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
        conn = udp.streamConnect(new pipeLine(32768, false), srcFwd, portLoc, trgAdr, portRem, "iax", -1, null, -1, -1);
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
        packIax sip = new packIax(conn);
        long lastRetry = 0;
        packHolder pck = new packHolder(true, true);
        for (;;) {
            if (!need2run) {
                break;
            }
            if (conn == null) {
                break;
            }
            if (sip.isClosed() != 0) {
                break;
            }
            if (sip.recvPack(pck) < 1) {
                continue;
            }
            if (debugger.clntIaxTraf) {
                sip.dump("rx");
            }

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

class clntIaxCall implements Runnable, Comparable<clntIaxCall> {

    public final clntIax lower;

    public final int rid;

    public final long started;

    public String src;

    public String trg;

    public int lid;

    public clntIaxCall(clntIax prnt, int rem) {
        lower = prnt;
        rid = rem;
        started = bits.getTime();
    }

    public void run() {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
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

    public void setClose() {



////////////
    }

}
