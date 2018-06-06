package clnt;

import addr.addrIP;
import cfg.cfgAll;
import cfg.cfgDial;
import cfg.cfgIfc;
import cfg.cfgVrf;
import ip.ipFwd;
import ip.ipFwdIface;
import ip.ipFwdTab;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Timer;
import java.util.TimerTask;
import pack.packRtp;
import pack.packSip;
import pipe.pipeLine;
import pipe.pipeSide;
import prt.prtAccept;
import prt.prtGen;
import snd.sndCodec;
import snd.sndCodecG711aLaw;
import snd.sndCodecG711uLaw;
import snd.sndConnect;
import tab.tabGen;
import user.userTerminal;
import util.bits;
import util.cmds;
import util.debugger;
import util.logger;
import util.uniResLoc;

/**
 * session initiation protocol (rfc3261) client
 *
 * @author matecsaba
 */
public class clntSip implements Runnable {

    /**
     * upper
     */
    public cfgDial upper;

    /**
     * protocol: 1=udp, 2=listen, 3=connect
     */
    public int protocol = 1;

    /**
     * local port
     */
    public int portLoc = 0;

    /**
     * remote port
     */
    public int portRem = packSip.port;

    /**
     * endpoint
     */
    public String endpt;

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
     * subscribe interval
     */
    public int subscribe = 0;

    /**
     * options interval
     */
    public int options = 0;

    /**
     * number of tries
     */
    public int retry = 8;

    /**
     * timeout in seconds
     */
    public int timeout = 5;

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
     * tcp handler
     */
    protected prtGen tcp;

    /**
     * forwarder interface
     */
    protected ipFwdIface srcFwd;

    /**
     * connection to peer
     */
    protected pipeSide conn;

    private final tabGen<clntSipOut> outs = new tabGen<clntSipOut>();

    private final tabGen<clntSipIn> ins = new tabGen<clntSipIn>();

    private final tabGen<clntSipMsg> msgs = new tabGen<clntSipMsg>();

    private ipFwd fwd;

    private addrIP trgAdr;

    private boolean need2run;

    private int seq;

    private Timer timKeep;

    private Timer timReg;

    private Timer timSub;

    private Timer timOpt;

    public void run() {
        if (debugger.clntSipTraf) {
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
        if (debugger.clntSipTraf) {
            logger.debug("stopped");
        }
    }

    /**
     * start work
     */
    public void startWork() {
        need2run = true;
        seq = bits.randomD();
        new Thread(this).start();
        if (keepalive > 0) {
            timKeep = new Timer();
            timKeep.schedule(new clntSipKeep(this), 500, keepalive);
        }
        if (register > 0) {
            timReg = new Timer();
            timReg.schedule(new clntSipReg(this), 500, register);
        }
        if (subscribe > 0) {
            timSub = new Timer();
            timSub.schedule(new clntSipSub(this), 500, subscribe);
        }
        if (options > 0) {
            timOpt = new Timer();
            timOpt.schedule(new clntSipOpt(this), 500, options);
        }
    }

    /**
     * stop work
     */
    public void stopWork() {
        need2run = false;
        if (conn != null) {
            conn.setClose();
        }
        conn = null;
        for (int i = 0; i < outs.size(); i++) {
            try {
                outs.get(i).callRtp.setClose();
            } catch (Exception e) {
            }
        }
        for (int i = 0; i < ins.size(); i++) {
            try {
                ins.get(i).data.setClose();
            } catch (Exception e) {
            }
        }
        try {
            timKeep.cancel();
        } catch (Exception e) {
        }
        try {
            timReg.cancel();
        } catch (Exception e) {
        }
        try {
            timSub.cancel();
        } catch (Exception e) {
        }
        try {
            timOpt.cancel();
        } catch (Exception e) {
        }
    }

    /**
     * get endpoint id
     *
     * @return id
     */
    protected String getEndpt() {
        return "<sip:" + endpt + "@" + uniResLoc.addr2str(trgAdr, 0) + ">";
    }

    /**
     * get via id
     *
     * @return id
     */
    protected String getVia() {
        String a;
        if (protocol == 1) {
            a = "UDP";
        } else {
            a = "TCP";
        }
        return "SIP/2.0/" + a + " " + uniResLoc.addr2str(srcFwd.addr, portLoc) + ";rport;branch=" + bits.randomD();
    }

    /**
     * get contact id
     *
     * @return id
     */
    protected String getCont() {
        return "<sip:" + endpt + "@" + uniResLoc.addr2str(srcFwd.addr, portLoc) + ">";
    }

    /**
     * get codec
     *
     * @return codec
     */
    protected sndCodec getCodec() {
        if (aLaw) {
            return new sndCodecG711aLaw();
        } else {
            return new sndCodecG711uLaw();
        }
    }

    private int findOutPort(int o) {
        for (int i = 0; i < outs.size(); i++) {
            if (outs.get(i).callPort == o) {
                return i;
            }
        }
        return -1;
    }

    private int findInPort(int o) {
        for (int i = 0; i < ins.size(); i++) {
            if (ins.get(i).callPort == o) {
                return i;
            }
        }
        return -1;
    }

    private int getDataPort() {
        for (;;) {
            int o = (bits.randomW() & 0x7ffe) + 0x4000;
            if (findOutPort(o) >= 0) {
                continue;
            }
            if (findInPort(o) >= 0) {
                continue;
            }
            return o;
        }
    }

    /**
     * delete the call
     *
     * @param leg call id
     */
    protected void delCall(clntSipIn leg) {
        ins.del(leg);
    }

    /**
     * delete the call
     *
     * @param leg call id
     */
    protected void delCall(clntSipOut leg) {
        outs.del(leg);
    }

    /**
     * send subscribe
     *
     * @param wau authentication request
     * @param pau authentication request
     */
    protected void sendSub(String wau, String pau) {
        if (conn == null) {
            return;
        }
        packSip sip = new packSip(conn);
        seq++;
        sip.makeReq("SUBSCRIBE", "sip:" + endpt + "@" + uniResLoc.addr2str(trgAdr, 0), packSip.updateTag(getEndpt()), getEndpt(), getCont(), getVia(), "" + bits.randomD(), seq, subscribe / 250);
        sip.addAuthor("", wau, usr, pwd);
        sip.addAuthor("Proxy-", pau, usr, pwd);
        if (debugger.clntSipTraf) {
            sip.dump("tx");
        }
        sip.writeDown();
    }

    /**
     * send options
     *
     * @param wau authentication request
     * @param pau authentication request
     */
    protected void sendOpt(String wau, String pau) {
        if (conn == null) {
            return;
        }
        packSip sip = new packSip(conn);
        seq++;
        sip.makeReq("OPTIONS", "sip:" + endpt + "@" + uniResLoc.addr2str(trgAdr, 0), packSip.updateTag(getEndpt()), getEndpt(), getCont(), getVia(), "" + bits.randomD(), seq, 0);
        sip.addAuthor("", wau, usr, pwd);
        sip.addAuthor("Proxy-", pau, usr, pwd);
        if (debugger.clntSipTraf) {
            sip.dump("tx");
        }
        sip.writeDown();
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
        packSip sip = new packSip(conn);
        seq++;
        sip.makeReq("REGISTER", "sip:" + uniResLoc.addr2str(trgAdr, 0), packSip.updateTag(getEndpt()), getEndpt(), getCont(), getVia(), "" + bits.randomD(), seq, register / 250);
        sip.addAuthor("", wau, usr, pwd);
        sip.addAuthor("Proxy-", pau, usr, pwd);
        if (debugger.clntSipTraf) {
            sip.dump("tx");
        }
        sip.writeDown();
    }

    /**
     * send keepalive
     */
    protected void sendKeep() {
        if (conn == null) {
            return;
        }
        packSip sip = new packSip(conn);
        if (debugger.clntSipTraf) {
            logger.debug("keepalive sent");
        }
        sip.writeKeep();
    }

    /**
     * get number of in calls
     *
     * @return number of calls
     */
    public int numCallsIn() {
        return ins.size();
    }

    /**
     * get number of out calls
     *
     * @return number of calls
     */
    public int numCallsOut() {
        return outs.size();
    }

    /**
     * get number of out messages
     *
     * @return number of messages
     */
    public int numMsgsOut() {
        return msgs.size();
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
        if (dir) {
            for (int i = 0; i < ins.size(); i++) {
                clntSipIn ntry = ins.get(i);
                if (ntry == null) {
                    continue;
                }
                res.add(ntry.cid + "|" + ntry.src + "|" + ntry.trg + "|" + bits.timePast(ntry.started));
            }
        } else {
            for (int i = 0; i < outs.size(); i++) {
                clntSipOut ntry = outs.get(i);
                if (ntry == null) {
                    continue;
                }
                res.add(ntry.callId + "|" + ntry.callSrc + "|" + ntry.callTrg + "|" + bits.timePast(ntry.started));
            }
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
        if (conn == null) {
            return true;
        }
        clntSipMsg leg;
        for (;;) {
            leg = new clntSipMsg(this, null);
            if (ins.find(new clntSipIn(this, leg.callId)) != null) {
                continue;
            }
            if (outs.find(new clntSipOut(this, leg.callId)) != null) {
                continue;
            }
            if (msgs.add(leg) == null) {
                break;
            }
        }
        leg.callTrg = called;
        leg.callSrc = calling;
        leg.callMsg = msg;
        seq += 5;
        leg.callSeq = seq;
        seq += 5;
        boolean res = leg.doSend();
        msgs.del(leg);
        return res;
    }

    /**
     * make the call
     *
     * @param calling calling number
     * @param called called number
     * @return call id, null if error
     */
    public String makeCall(String calling, String called) {
        clntSipOut leg;
        for (;;) {
            leg = new clntSipOut(this, null);
            if (ins.find(new clntSipIn(this, leg.callId)) != null) {
                continue;
            }
            if (msgs.find(new clntSipMsg(this, leg.callId)) != null) {
                continue;
            }
            if (outs.add(leg) == null) {
                break;
            }
        }
        leg.callTrg = called;
        leg.callSrc = calling;
        seq += 5;
        leg.callSeq = seq;
        seq += 5;
        leg.callPort = getDataPort();
        boolean res = leg.makeCall();
        if (res) {
            leg.stopCall(false);
            if (upper != null) {
                upper.stoppedCall(true, leg.callSrc, leg.callTrg, leg.started);
            }
            return null;
        }
        return leg.callId;
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
        clntSipIn legI = new clntSipIn(this, id);
        legI = ins.find(legI);
        if (legI != null) {
            if (legI.data != null) {
                legI.data.setClose();
            }
            return;
        }
        clntSipOut legO = new clntSipOut(this, id);
        legO = outs.find(legO);
        if (legO == null) {
            return;
        }
        if (legO.stopping) {
            return;
        }
        legO.stopCall(true);
        if (upper != null) {
            upper.stoppedCall(true, legO.callSrc, legO.callTrg, legO.started);
        }
    }

    /**
     * get call
     *
     * @param id call id
     * @return rtp
     */
    public packRtp getCall(String id) {
        if (id == null) {
            return null;
        }
        clntSipIn legI = new clntSipIn(this, id);
        legI = ins.find(legI);
        if (legI != null) {
            return legI.data;
        }
        clntSipOut legO = new clntSipOut(this, id);
        legO = outs.find(legO);
        if (legO == null) {
            return null;
        }
        return legO.callRtp;
    }

    /**
     * do one round
     */
    public void doWork() {
        if (conn != null) {
            conn.setClose();
        }
        conn = null;
        trgAdr = userTerminal.justResolv(trgDom, 0);
        if (trgAdr == null) {
            return;
        }
        fwd = vrf.getFwd(trgAdr);
        udp = vrf.getUdp(trgAdr);
        tcp = vrf.getTcp(trgAdr);
        if (srcIfc != null) {
            srcFwd = srcIfc.getFwdIfc(trgAdr);
        } else {
            srcFwd = ipFwdTab.findSendingIface(fwd, trgAdr);
        }
        if (srcFwd == null) {
            return;
        }
        if (portLoc == 0) {
            portLoc = bits.randomW();
        }
        switch (protocol) {
            case 1:
                conn = udp.streamConnect(new pipeLine(32768, false), srcFwd, portLoc, trgAdr, portRem, "sip", null, -1);
                break;
            case 2:
                bits.sleep(bits.random(1000, 5000));
                prtAccept ac = new prtAccept(tcp, new pipeLine(32768, false), srcFwd, portLoc, trgAdr, 0, portRem, "sip", null, -1);
                ac.wait4conn(30000);
                conn = ac.getConn(true);
                break;
            case 3:
                bits.sleep(bits.random(1000, 5000));
                conn = tcp.streamConnect(new pipeLine(32768, false), srcFwd, portLoc, trgAdr, portRem, "sip", null, -1);
                break;
        }
        if (conn == null) {
            return;
        }
        conn.timeout = 180000;
        conn.lineRx = pipeSide.modTyp.modeCRtryLF;
        conn.lineTx = pipeSide.modTyp.modeCRLF;
        conn.setReady();
        conn.wait4ready(30000);
        if (conn.isReady() != 3) {
            conn.setClose();
            conn = null;
            return;
        }
        if (protocol > 1) {
            logger.warn("neighbor " + trgDom + " up");
        }
        packSip sip = new packSip(conn);
        long lastRetry = 0;
        for (;;) {
            if (!need2run) {
                break;
            }
            if (conn == null) {
                break;
            }
            if (sip.readUp()) {
                break;
            }
            if (debugger.clntSipTraf) {
                sip.dump("rx");
            }
            String a = sip.headerGet("Call-Id", 1);
            clntSipOut legO = new clntSipOut(this, a);
            clntSipIn legI = new clntSipIn(this, a);
            clntSipMsg legM = new clntSipMsg(this, a);
            packSip tx = sip.byteCopy(null);
            cmds cmd = new cmds("sip", sip.command);
            a = cmd.word().toLowerCase();
            if (a.equals("register") || a.equals("subscribe")) {
                tx.makeOk(sip, null, 120);
                tx.copyHeader(sip, "Contact");
                if (debugger.clntSipTraf) {
                    tx.dump("tx");
                }
                tx.writeDown();
                continue;
            }
            if (a.equals("options")) {
                tx.makeOk(sip, null, 0);
                if (debugger.clntSipTraf) {
                    tx.dump("tx");
                }
                tx.writeDown();
                continue;
            }
            if (a.equals("notify")) {
                tx.makeOk(sip, null, 0);
                if (debugger.clntSipTraf) {
                    tx.dump("tx");
                }
                tx.writeDown();
                continue;
            }
            legO = outs.find(legO);
            if (legO != null) {
                legO.callRep = sip.byteCopy(null);
                if (legO.callCnt == null) {
                    continue;
                }
                if (!a.equals("bye") && !a.equals("cancel")) {
                    continue;
                }
                tx.makeOk(sip, null, 0);
                if (debugger.clntSipTraf) {
                    tx.dump("tx");
                }
                tx.writeDown();
                if (legO.stopping) {
                    continue;
                }
                legO.stopCall(false);
                if (upper != null) {
                    upper.stoppedCall(true, legO.callSrc, legO.callTrg, legO.started);
                }
                continue;
            }
            legI = ins.find(legI);
            if (legI != null) {
                legI.callRep = sip.byteCopy(null);
                continue;
            }
            legM = msgs.find(legM);
            if (legM != null) {
                legM.callRep = sip.byteCopy(null);
                continue;
            }
            if (a.length() < 1) {
                continue;
            }
            if (a.equals("message")) {
                if (upper == null) {
                    tx.makeErr(sip, null, "not allowed");
                    if (debugger.clntSipTraf) {
                        tx.dump("tx");
                    }
                    tx.writeDown();
                    continue;
                }
                clntSipMsg msg = new clntSipMsg(this, null);
                msg.callSrc = sip.headerGet("From", 1);
                msg.callTrg = sip.headerGet("To", 1);
                msg.callRep = sip.byteCopy(null);
                msg.startWork();
                continue;
            }
            if (a.startsWith("sip/")) {
                a = cmd.word();
                if (!a.equals("401") && !a.equals("407")) {
                    continue;
                }
                String wau = sip.headerGet("WWW-Authenticate", 1);
                String pau = sip.headerGet("Proxy-Authenticate", 1);
                cmd = new cmds("sip", sip.headerGet("CSeq", 1));
                cmd.word();
                a = cmd.word().toLowerCase();
                long tim = bits.getTime();
                if (a.equals("register")) {
                    if ((tim - lastRetry) < 500) {
                        continue;
                    }
                    sendReg(wau, pau);
                    lastRetry = tim;
                    continue;
                }
                if (a.equals("subscribe")) {
                    if ((tim - lastRetry) < 500) {
                        continue;
                    }
                    sendSub(wau, pau);
                    lastRetry = tim;
                    continue;
                }
                if (a.equals("options")) {
                    if ((tim - lastRetry) < 500) {
                        continue;
                    }
                    sendOpt(wau, pau);
                    lastRetry = tim;
                    continue;
                }
                continue;
            }
            if (a.equals("ack")) {
                continue;
            }
            if (a.equals("bye") || a.equals("cancel")) {
                tx.makeOk(sip, null, 0);
                if (debugger.clntSipTraf) {
                    tx.dump("tx");
                }
                tx.writeDown();
                continue;
            }
            if (!a.equals("invite")) {
                tx.makeErr(sip, null, "bad method");
                if (debugger.clntSipTraf) {
                    tx.dump("tx");
                }
                tx.writeDown();
                continue;
            }
            if (upper == null) {
                tx.makeErr(sip, null, "not allowed");
                if (debugger.clntSipTraf) {
                    tx.dump("tx");
                }
                tx.writeDown();
                continue;
            }
            addrIP adr = new addrIP();
            int prt = sip.sdpGetMediaEP(adr);
            if (prt < 1) {
                tx.makeErr(sip, null, "no endpoint");
                if (debugger.clntSipTraf) {
                    tx.dump("tx");
                }
                tx.writeDown();
                continue;
            }
            a = sip.headerGet("Call-Id", 1);
            legI = new clntSipIn(this, a);
            legI.adr = adr;
            legI.prt = prt;
            legI.callPort = getDataPort();
            legI.callRep = sip.byteCopy(null);
            new Thread(legI).start();
            ins.add(legI);
        }
        if (conn != null) {
            conn.setClose();
        }
        conn = null;
        if (protocol > 1) {
            logger.error("neighbor " + trgDom + " down");
        }
        if (debugger.clntSipTraf) {
            logger.debug("restarting");
        }
    }

}

class clntSipKeep extends TimerTask {

    private clntSip lower;

    public clntSipKeep(clntSip parent) {
        lower = parent;
    }

    public void run() {
        try {
            lower.sendKeep();
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

}

class clntSipReg extends TimerTask {

    private clntSip lower;

    public clntSipReg(clntSip parent) {
        lower = parent;
    }

    public void run() {
        try {
            lower.sendReg(null, null);
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

}

class clntSipSub extends TimerTask {

    private clntSip lower;

    public clntSipSub(clntSip parent) {
        lower = parent;
    }

    public void run() {
        try {
            lower.sendSub(null, null);
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

}

class clntSipOpt extends TimerTask {

    private clntSip lower;

    public clntSipOpt(clntSip parent) {
        lower = parent;
    }

    public void run() {
        try {
            lower.sendOpt(null, null);
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

}

class clntSipOut implements Comparator<clntSipOut> {

    public final clntSip lower;

    public final String callId;

    public final long started;

    public int callPort;

    public String callSrc;

    public String callTrg;

    public String callWauth;

    public String callPauth;

    public String callCnt;

    public int callSeq;

    public packSip callRep;

    public packRtp callRtp;

    public boolean stopping;

    public clntSipOut(clntSip prnt, String id) {
        if (id == null) {
            id = bits.randomD() + "-" + cfgAll.hostName;
        }
        callId = id;
        lower = prnt;
        started = bits.getTime();
    }

    public int compare(clntSipOut o1, clntSipOut o2) {
        return o1.callId.compareTo(o2.callId);
    }

    public boolean makeCall() {
        callSrc = packSip.updateTag(callSrc);
        callWauth = null;
        callPauth = null;
        callCnt = null;
        callRep = null;
        callRtp = null;
        packSip callAck = null;
        boolean need2inv = true;
        for (int o = 0; o < lower.retry; o++) {
            if (lower.conn == null) {
                continue;
            }
            if (need2inv) {
                packSip sip = new packSip(lower.conn);
                sip.makeReq("INVITE", null, callSrc, callTrg, lower.getCont(), lower.getVia(), callId, callSeq, 0);
                sip.addAuthor("", callWauth, lower.usr, lower.pwd);
                sip.addAuthor("Proxy-", callPauth, lower.usr, lower.pwd);
                sip.makeSdp(lower.srcFwd.addr, callPort, lower.getCodec());
                if (debugger.clntSipTraf) {
                    sip.dump("tx");
                }
                sip.writeDown();
            }
            for (int i = 0; i < lower.timeout; i++) {
                bits.sleep(1000);
                if (callRep != null) {
                    break;
                }
            }
            if (callRep == null) {
                continue;
            }
            packSip sip = callRep.byteCopy(null);
            callRep = null;
            cmds cmd = new cmds("sip", sip.command);
            if (!cmd.word().toLowerCase().startsWith("sip/")) {
                continue;
            }
            String a = cmd.word();
            if (a.equals("401")) {
                callWauth = sip.headerGet("WWW-Authenticate", 1);
                need2inv = true;
                callSeq++;
                continue;
            }
            if (a.equals("407")) {
                callPauth = sip.headerGet("Proxy-Authenticate", 1);
                need2inv = true;
                callSeq++;
                continue;
            }
            if (a.startsWith("1")) {
                need2inv = false;
                continue;
            }
            if (a.startsWith("2")) {
                callAck = sip.byteCopy(null);
            }
            break;
        }
        if (callAck == null) {
            return true;
        }
        addrIP remA = new addrIP();
        int remP = callAck.sdpGetMediaEP(remA);
        if (remP < 0) {
            return true;
        }
        callRtp = new packRtp();
        if (callRtp.startConnect(lower.udp, new pipeLine(65536, true), lower.srcFwd, callPort, remA, remP)) {
            return true;
        }
        callTrg = callAck.headerGet("To", 1);
        callCnt = uniResLoc.fromEmail(callAck.headerGet("Contact", 1));
        callAck.makeReq("ACK", null, callSrc, callTrg, lower.getCont(), lower.getVia(), callId, callSeq, 0);
        callAck.addAuthor("", callWauth, lower.usr, lower.pwd);
        callAck.addAuthor("Proxy-", callPauth, lower.usr, lower.pwd);
        if (debugger.clntSipTraf) {
            callAck.dump("tx");
        }
        callAck.writeDown();
        return false;
    }

    public void stopCall(boolean bye) {
        stopping = true;
        if (callRtp != null) {
            callRtp.setClose();
        }
        if (!bye) {
            lower.delCall(this);
            return;
        }
        callRep = null;
        for (int o = 0; o < lower.retry; o++) {
            if (lower.conn == null) {
                continue;
            }
            packSip sip = new packSip(lower.conn);
            String a;
            if (callRtp != null) {
                a = "BYE";
            } else {
                a = "CANCEL";
            }
            sip.makeReq(a, callCnt, callSrc, callTrg, lower.getCont(), lower.getVia(), callId, callSeq + 1, 0);
            sip.addAuthor("", callWauth, lower.usr, lower.pwd);
            sip.addAuthor("Proxy-", callPauth, lower.usr, lower.pwd);
            if (debugger.clntSipTraf) {
                sip.dump("tx");
            }
            sip.writeDown();
            for (int i = 0; i < lower.timeout; i++) {
                bits.sleep(1000);
                if (callRep != null) {
                    lower.delCall(this);
                    return;
                }
            }
        }
        lower.delCall(this);
    }

}

class clntSipIn implements Runnable, Comparator<clntSipIn> {

    public final clntSip lower;

    public final String cid;

    public final long started;

    public addrIP adr;

    public int prt;

    public int csq;

    public String via;

    public String src;

    public String trg;

    public String cnt;

    public int callPort;

    public packSip callRep;

    public String newSrc;

    public String newTrg;

    public cfgDial peer;

    public String rcd;

    public packRtp data;

    public sndConnect conner;

    public clntSipIn(clntSip prnt, String id) {
        lower = prnt;
        cid = id;
        started = bits.getTime();
    }

    public int compare(clntSipIn o1, clntSipIn o2) {
        return o1.cid.compareTo(o2.cid);
    }

    public void run() {
        try {
            doer();
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

    public void sendTry(packSip sip) {
        packSip tx = sip.byteCopy(null);
        tx.makeNumeric("100 trying", sip, lower.getCont());
        if (debugger.clntSipTraf) {
            tx.dump("tx");
        }
        tx.writeDown();
    }

    public void sendRng(packSip sip) {
        packSip tx = sip.byteCopy(null);
        tx.makeNumeric("180 ringing", sip, lower.getCont());
        if (debugger.clntSipTraf) {
            tx.dump("tx");
        }
        tx.writeDown();
    }

    public void sendSdp(packSip sip) {
        packSip tx = sip.byteCopy(null);
        tx.makeOk(sip, lower.getCont(), 0);
        tx.makeSdp(lower.srcFwd.addr, callPort, lower.getCodec());
        if (debugger.clntSipTraf) {
            tx.dump("tx");
        }
        tx.writeDown();
    }

    public void sendBye() {
        callRep = null;
        for (int o = 0; o < lower.retry; o++) {
            if (lower.conn == null) {
                continue;
            }
            packSip tx = new packSip(lower.conn);
            tx.makeReq("BYE", cnt, trg, src, null, via, cid, csq + 1, 0);
            if (debugger.clntSipTraf) {
                tx.dump("tx");
            }
            tx.writeDown();
            for (int i = 0; i < lower.timeout; i++) {
                bits.sleep(1000);
                if (callRep != null) {
                    return;
                }
            }
        }
    }

    public void doer() {
        packSip sip = callRep.byteCopy(null);
        callRep = null;
        String a = sip.headerGet("CSeq", 1) + " ";
        csq = a.indexOf(" ");
        csq = bits.str2num(a.substring(0, csq).trim());
        via = sip.headerGet("Via", 1);
        src = sip.headerGet("From", 1);
        trg = sip.headerGet("To", 1);
        cnt = uniResLoc.fromEmail(sip.headerGet("Contact", 1));
        sendTry(sip);
        trg = packSip.updateTag(trg);
        sip.headerSet("To", 1, trg);
        sendRng(sip);
        newSrc = lower.upper.incomeSrc(src);
        newTrg = lower.upper.incomeTrg(trg);
        peer = lower.upper.incomeCall(newSrc, newTrg);
        if (peer == null) {
            packSip tx = sip.byteCopy(null);
            tx.makeErr(sip, null, "no such number");
            if (debugger.clntSipTraf) {
                tx.dump("tx");
            }
            tx.writeDown();
            lower.upper.stoppedCall(false, newSrc, newTrg, started);
            lower.delCall(this);
            return;
        }
        rcd = peer.makeCall(newSrc, newTrg);
        if (rcd == null) {
            packSip tx = sip.byteCopy(null);
            tx.makeErr(sip, null, "failed to make call");
            if (debugger.clntSipTraf) {
                tx.dump("tx");
            }
            tx.writeDown();
            lower.upper.stoppedCall(false, newSrc, newTrg, started);
            lower.delCall(this);
            return;
        }
        sendSdp(sip);
        data = new packRtp();
        if (data.startConnect(lower.udp, new pipeLine(65536, true), lower.srcFwd, callPort, adr, prt)) {
            sendBye();
            peer.stopCall(rcd);
            lower.upper.stoppedCall(false, newSrc, newTrg, started);
            lower.delCall(this);
            return;
        }
        conner = new sndConnect(data, peer.getCall(rcd), lower.getCodec(), peer.getCodec());
        for (;;) {
            if (conner.isClosed() != 0) {
                break;
            }
            if (lower.conn == null) {
                break;
            }
            if (callRep == null) {
                bits.sleep(1000);
                continue;
            }
            sip = callRep.byteCopy(null);
            callRep = null;
            a = sip.command.trim();
            int i = a.indexOf(" ");
            if (i < 0) {
                continue;
            }
            a = a.substring(0, i).trim().toLowerCase();
            if (a.equals("invite")) {
                sendTry(sip);
                sendRng(sip);
                sendSdp(sip);
                continue;
            }
            if (!a.equals("bye") && !a.equals("cancel")) {
                continue;
            }
            packSip tx = sip.byteCopy(null);
            tx.makeOk(sip, null, 0);
            if (debugger.clntSipTraf) {
                tx.dump("tx");
            }
            tx.writeDown();
            conner.setClose();
            peer.stopCall(rcd);
            lower.upper.stoppedCall(false, newSrc, newTrg, started);
            lower.delCall(this);
            return;
        }
        conner.setClose();
        peer.stopCall(rcd);
        sendBye();
        lower.upper.stoppedCall(false, newSrc, newTrg, started);
        lower.delCall(this);
    }

}

class clntSipMsg implements Runnable, Comparator<clntSipMsg> {

    public final clntSip lower;

    public final String callId;

    public final long started;

    public packSip callRep;

    public String callSrc;

    public String callTrg;

    public int callSeq;

    public String callWauth;

    public String callPauth;

    public List<String> callMsg;

    public clntSipMsg(clntSip prnt, String id) {
        if (id == null) {
            id = bits.randomD() + "-" + cfgAll.hostName;
        }
        lower = prnt;
        callId = id;
        started = bits.getTime();
    }

    public int compare(clntSipMsg o1, clntSipMsg o2) {
        return o1.callId.compareTo(o2.callId);
    }

    public void startWork() {
        new Thread(this).start();
    }

    public boolean doSend() {
        callSrc = packSip.updateTag(callSrc);
        callWauth = null;
        callPauth = null;
        callRep = null;
        boolean need2msg = true;
        for (int o = 0; o < lower.retry; o++) {
            if (lower.conn == null) {
                continue;
            }
            if (need2msg) {
                packSip sip = new packSip(lower.conn);
                sip.makeReq("MESSAGE", null, callSrc, callTrg, lower.getCont(), lower.getVia(), callId, callSeq, 0);
                sip.header.add("Content-Type: text/plain");
                sip.content.addAll(callMsg);
                sip.addAuthor("", callWauth, lower.usr, lower.pwd);
                sip.addAuthor("Proxy-", callPauth, lower.usr, lower.pwd);
                if (debugger.clntSipTraf) {
                    sip.dump("tx");
                }
                sip.writeDown();
            }
            for (int i = 0; i < lower.timeout; i++) {
                bits.sleep(1000);
                if (callRep != null) {
                    break;
                }
            }
            if (callRep == null) {
                continue;
            }
            packSip sip = callRep.byteCopy(null);
            callRep = null;
            cmds cmd = new cmds("sip", sip.command);
            if (!cmd.word().toLowerCase().startsWith("sip/")) {
                continue;
            }
            String a = cmd.word();
            if (a.equals("401")) {
                callWauth = sip.headerGet("WWW-Authenticate", 1);
                need2msg = true;
                callSeq++;
                continue;
            }
            if (a.equals("407")) {
                callPauth = sip.headerGet("Proxy-Authenticate", 1);
                need2msg = true;
                callSeq++;
                continue;
            }
            if (a.startsWith("1")) {
                need2msg = false;
                continue;
            }
            if (a.startsWith("2")) {
                return false;
            }
            break;
        }
        return true;
    }

    public void run() {
        try {
            doRecv();
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

    public void doRecv() {
        packSip tx = callRep.byteCopy(null);
        tx.makeNumeric("100 trying", callRep, lower.getCont());
        if (debugger.clntSipTraf) {
            tx.dump("tx");
        }
        tx.writeDown();
        callSrc = lower.upper.incomeSrc(callSrc);
        callTrg = lower.upper.incomeTrg(callTrg);
        cfgDial per = lower.upper.incomeMsg(callSrc, callTrg);
        if (per == null) {
            tx.makeErr(callRep, null, "no such number");
            if (debugger.clntSipTraf) {
                tx.dump("tx");
            }
            tx.writeDown();
            return;
        }
        if (per.sendMsg(callSrc, callTrg, callRep.content)) {
            tx.makeErr(callRep, null, "not went out");
            if (debugger.clntSipTraf) {
                tx.dump("tx");
            }
            tx.writeDown();
            return;
        }
        tx.makeOk(callRep, null, 0);
        if (debugger.clntSipTraf) {
            tx.dump("tx");
        }
        tx.writeDown();
    }

}
