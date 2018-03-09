package clnt;

import addr.addrIP;
import cfg.cfgDial;
import cfg.cfgIfc;
import cfg.cfgVrf;
import ip.ipFwd;
import ip.ipFwdIface;
import ip.ipFwdTab;
import java.util.Timer;
import java.util.TimerTask;
import pack.packRtp;
import pack.packSip;
import pipe.pipeLine;
import pipe.pipeSide;
import prt.prtGen;
import snd.sndConnect;
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

    private ipFwd fwd;

    private prtGen udp;

    private ipFwdIface srcFwd;

    private addrIP trgAdr;

    private String callSrc;

    private String callTrg;

    private String callId;

    private String callAuth;

    private String callCnt;

    private int callSeq;

    private packSip callRep;

    private packRtp callRtp;

    private boolean need2run;

    private pipeSide conn;

    private int seq;

    private Timer timKeep;

    private Timer timReg;

    private Timer timSub;

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
        try {
            callRtp.setClose();
        } catch (Exception e) {
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
    }

    private String getEndpt() {
        return "<sip:" + endpt + "@" + uniResLoc.addr2str(trgAdr, 0) + ">";
    }

    private String getVia() {
        return "SIP/2.0/UDP " + uniResLoc.addr2str(srcFwd.addr, portLoc) + ";rport;branch=" + bits.randomD();
    }

    private String getCont() {
        return "<sip:" + endpt + "@" + uniResLoc.addr2str(srcFwd.addr, portLoc) + ">";
    }

    private int getDataPort() {
        return (portLoc - 3) & 0xfffe;
    }

    /**
     * send subscribe
     *
     * @param auth authentication request
     */
    public void sendSub(String auth) {
        if (conn == null) {
            return;
        }
        packSip sip = new packSip(conn);
        seq++;
        sip.makeReq("SUBSCRIBE", "sip:" + endpt + "@" + uniResLoc.addr2str(trgAdr, 0), getEndpt() + ";tag=" + bits.randomD(), getEndpt(), getCont(), getVia(), "" + bits.randomD(), seq, subscribe / 250);
        sip.addAuthor(auth, usr, pwd);
        if (debugger.clntSipTraf) {
            sip.dump("tx");
        }
        sip.writeDown();
    }

    /**
     * send register
     *
     * @param auth authentication request
     */
    public void sendReg(String auth) {
        if (conn == null) {
            return;
        }
        packSip sip = new packSip(conn);
        seq++;
        sip.makeReq("REGISTER", "sip:" + uniResLoc.addr2str(trgAdr, 0), getEndpt() + ";tag=" + bits.randomD(), getEndpt(), getCont(), getVia(), "" + bits.randomD(), seq, register / 250);
        sip.addAuthor(auth, usr, pwd);
        if (debugger.clntSipTraf) {
            sip.dump("tx");
        }
        sip.writeDown();
    }

    /**
     * send keepalive
     */
    public void sendKeep() {
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
     * check if calling
     *
     * @return true if yes, false if not
     */
    public boolean isCalling() {
        return callTrg != null;
    }

    /**
     * make the call
     *
     * @param calling calling number
     * @param called called number
     * @return false if ok, true if error
     */
    public boolean makeCall(String calling, String called) {
        callTrg = called;
        if (calling.indexOf(";tag=") < 0) {
            calling += ";tag=" + bits.randomD();
        }
        callSrc = calling;
        callId = "" + bits.randomD();
        callAuth = null;
        callCnt = null;
        callRep = null;
        callRtp = null;
        callSeq = 0;
        packSip callAck = null;
        boolean need2inv = true;
        for (int o = 0; o < 8; o++) {
            if (conn == null) {
                continue;
            }
            if (need2inv) {
                packSip sip = new packSip(conn);
                seq++;
                callSeq = seq;
                sip.makeReq("INVITE", null, callSrc, callTrg, getCont(), getVia(), callId, callSeq, 0);
                sip.addAuthor(callAuth, usr, pwd);
                sip.makeSdp(srcFwd.addr, getDataPort(), aLaw);
                if (debugger.clntSipTraf) {
                    sip.dump("tx");
                }
                sip.writeDown();
            }
            for (int i = 0; i < 5; i++) {
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
                callAuth = sip.headerGet("WWW-Authenticate", 1);
                need2inv = true;
                o--;
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
            stopCall();
            return true;
        }
        addrIP remA = new addrIP();
        int remP = callAck.sdpGetMediaEP(remA);
        if (remP < 0) {
            stopCall();
            return true;
        }
        callRtp = new packRtp();
        if (callRtp.startConnect(udp, new pipeLine(65536, true), srcFwd, getDataPort(), remA, remP)) {
            return true;
        }
        callTrg = callAck.headerGet("To", 1);
        callCnt = uniResLoc.fromEmail(callAck.headerGet("Contact", 1));
        callAck.makeReq("ACK", callCnt, callSrc, callTrg, getCont(), getVia(), callId, callSeq, 0);
        callAck.addAuthor(callAuth, usr, pwd);
        if (debugger.clntSipTraf) {
            callAck.dump("tx");
        }
        callAck.writeDown();
        return false;
    }

    /**
     * stop the call
     */
    public void stopCall() {
        String a;
        if (callRtp != null) {
            callRtp.setClose();
            a = "BYE";
        } else {
            a = "CANCEL";
        }
        if (callId != null) {
            packSip sip = new packSip(conn);
            seq++;
            sip.makeReq(a, callCnt, callSrc, callTrg, getCont(), getVia(), callId, seq, 0);
            sip.addAuthor(callAuth, usr, pwd);
            if (debugger.clntSipTraf) {
                sip.dump("tx");
            }
            sip.writeDown();
        }
        callSeq = 0;
        callCnt = null;
        callAuth = null;
        callRtp = null;
        callId = null;
        callSrc = null;
        callTrg = null;
    }

    /**
     * get call
     *
     * @return rtp
     */
    public packRtp getCall() {
        return callRtp;
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
        conn = udp.streamConnect(new pipeLine(32768, false), srcFwd, portLoc, trgAdr, portRem, "sip", null, -1);
        if (conn == null) {
            return;
        }
        conn.timeout = 180000;
        conn.lineRx = pipeSide.modTyp.modeCRtryLF;
        conn.lineTx = pipeSide.modTyp.modeCRLF;
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
            if (callId != null) {
                if (callId.equals(sip.headerGet("Call-Id", 1))) {
                    callRep = sip.byteCopy(null);
                    if (callCnt == null) {
                        continue;
                    }
                    String a = sip.command.trim();
                    int i = a.indexOf(" ");
                    if (i < 0) {
                        continue;
                    }
                    a = a.substring(0, i).trim().toLowerCase();
                    if (!a.equals("bye")) {
                        continue;
                    }
                    packSip tx = sip.byteCopy(null);
                    tx.makeOk(sip, null, 0);
                    if (debugger.clntSipTraf) {
                        tx.dump("tx");
                    }
                    tx.writeDown();
                    stopCall();
                    continue;
                }
            }
            cmds cmd = new cmds("sip", sip.command);
            String a = cmd.word().toLowerCase();
            if (a.length() < 1) {
                continue;
            }
            if (a.startsWith("sip/")) {
                a = cmd.word();
                if (!a.equals("401")) {
                    continue;
                }
                String auth = sip.headerGet("WWW-Authenticate", 1);
                cmd = new cmds("sip", sip.headerGet("CSeq", 1));
                cmd.word();
                a = cmd.word().toLowerCase();
                long tim = bits.getTime();
                if (a.equals("register")) {
                    if ((tim - lastRetry) < 500) {
                        continue;
                    }
                    sendReg(auth);
                    lastRetry = tim;
                    continue;
                }
                if (a.equals("subscribe")) {
                    if ((tim - lastRetry) < 500) {
                        continue;
                    }
                    sendSub(auth);
                    lastRetry = tim;
                    continue;
                }
                continue;
            }
            if (a.equals("ack") || a.equals("bye") || a.equals("cancel")) {
                continue;
            }
            packSip tx = sip.byteCopy(null);
            if (!a.equals("invite")) {
                tx.makeErr(sip, null, "bad method");
                if (debugger.clntSipTraf) {
                    tx.dump("tx");
                }
                tx.writeDown();
                continue;
            }
            if (isCalling()) {
                tx.makeErr(sip, null, "busy");
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
            String via = sip.headerGet("Via", 1);
            String src = sip.headerGet("From", 1);
            String trg = sip.headerGet("To", 1);
            String cid = sip.headerGet("Call-Id", 1);
            String cnt = uniResLoc.fromEmail(sip.headerGet("Contact", 1));
            tx.makeNumeric("100 trying", sip, getCont());
            if (debugger.clntSipTraf) {
                tx.dump("tx");
            }
            tx.writeDown();
            trg += ";tag=" + bits.randomD();
            sip.headerSet("To", 1, trg);
            tx.makeNumeric("180 ringing", sip, getCont());
            if (debugger.clntSipTraf) {
                tx.dump("tx");
            }
            tx.writeDown();
            String newSrc = upper.incomeSrc(src);
            String newTrg = upper.incomeTrg(trg);
            cfgDial peer = upper.incomeCall(newSrc, newTrg);
            if (peer == null) {
                tx.makeErr(sip, null, "no such number");
                if (debugger.clntSipTraf) {
                    tx.dump("tx");
                }
                tx.writeDown();
                continue;
            }
            callTrg = "incoming";
            if (peer.makeCall(newSrc, newTrg)) {
                callTrg = null;
                tx.makeErr(sip, null, "failed to make call");
                if (debugger.clntSipTraf) {
                    tx.dump("tx");
                }
                tx.writeDown();
                continue;
            }
            tx.makeOk(sip, getCont(), 0);
            tx.makeSdp(srcFwd.addr, getDataPort(), peer.getCodec());
            if (debugger.clntSipTraf) {
                tx.dump("tx");
            }
            tx.writeDown();
            packRtp data = new packRtp();
            if (data.startConnect(udp, new pipeLine(65536, true), srcFwd, getDataPort(), adr, prt)) {
                callTrg = null;
                tx.makeReq("BYE", cnt, trg, src, null, via, cid, bits.randomD(), 0);
                if (debugger.clntSipTraf) {
                    tx.dump("tx");
                }
                tx.writeDown();
                peer.stopCall();
                continue;
            }
            sndConnect conner = new sndConnect(data, peer.getCall());
            for (;;) {
                if (conner.isClosed() != 0) {
                    break;
                }
                if (sip.isClosed() != 0) {
                    break;
                }
                if (sip.ready2rx() < 1) {
                    bits.sleep(1000);
                    continue;
                }
                if (sip.readUp()) {
                    return;
                }
                if (debugger.clntSipTraf) {
                    sip.dump("rx");
                }
                a = sip.command.trim();
                int i = a.indexOf(" ");
                if (i < 0) {
                    continue;
                }
                a = a.substring(0, i).trim().toLowerCase();
                if (a.equals("bye") || a.equals("cancel")) {
                    tx.makeOk(sip, null, 0);
                    if (debugger.clntSipTraf) {
                        tx.dump("tx");
                    }
                    tx.writeDown();
                    break;
                }
            }
            conner.setClose();
            tx.makeReq("BYE", cnt, trg, src, null, via, cid, bits.randomD(), 0);
            if (debugger.clntSipTraf) {
                tx.dump("tx");
            }
            tx.writeDown();
            peer.stopCall();
            callTrg = null;
        }
        if (conn != null) {
            conn.setClose();
        }
        conn = null;
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
            lower.sendReg(null);
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
            lower.sendSub(null);
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

}
