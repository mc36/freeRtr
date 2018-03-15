package serv;

import addr.addrIP;
import java.util.Comparator;
import java.util.List;
import pack.packRtp;
import pack.packSip;
import pipe.pipeDiscard;
import pipe.pipeLine;
import pipe.pipeSide;
import prt.prtGenConn;
import prt.prtServS;
import snd.sndCodec;
import snd.sndCodecG711aLaw;
import snd.sndCodecG711uLaw;
import snd.sndScript;
import tab.tabGen;
import user.userFilter;
import user.userHelping;
import user.userScript;
import util.bits;
import util.cmds;
import util.debugger;
import util.logger;
import util.uniResLoc;

/**
 * voice server
 *
 * @author matecsaba
 */
public class servVoice extends servGeneric implements prtServS {

    /**
     * codec, true=alaw, false=ulaw
     */
    protected boolean aLaw = true;

    /**
     * voice script name
     */
    protected String voiceScript;

    /**
     * message script name
     */
    protected String messageScript;

    /**
     * defaults text
     */
    public final static String defaultL[] = {
        "server voice .*! port " + packSip.port,
        "server voice .*! protocol " + proto2string(protoAllDgrm),
        "server voice .*! codec alaw",
        "server voice .*! no voice-script",
        "server voice .*! no message-script",};

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public tabGen<userFilter> srvDefFlt() {
        return defaultF;
    }

    public String srvName() {
        return "voice";
    }

    public int srvPort() {
        return packSip.port;
    }

    public int srvProto() {
        return protoAllDgrm;
    }

    public boolean srvInit() {
        return genStrmStart(this, new pipeLine(32768, false), 0);
    }

    public boolean srvDeinit() {
        return genericStop(0);
    }

    public void srvShRun(String beg, List<String> lst) {
        String a;
        if (aLaw) {
            a = "alaw";
        } else {
            a = "ulaw";
        }
        lst.add(beg + "codec " + a);
        cmds.cfgLine(lst, voiceScript == null, beg, "voice-script", voiceScript);
        cmds.cfgLine(lst, messageScript == null, beg, "message-script", messageScript);
    }

    public boolean srvCfgStr(cmds cmd) {
        String a = cmd.word();
        if (a.equals("codec")) {
            a = cmd.word();
            if (a.equals("alaw")) {
                aLaw = true;
            }
            if (a.equals("ulaw")) {
                aLaw = false;
            }
            return false;
        }
        if (a.equals("voice-script")) {
            voiceScript = cmd.getRemaining();
            return false;
        }
        if (a.equals("message-script")) {
            messageScript = cmd.getRemaining();
            return false;
        }
        if (!a.equals("no")) {
            return true;
        }
        a = cmd.word();
        if (a.equals("voice-script")) {
            voiceScript = null;
            return false;
        }
        if (a.equals("message-script")) {
            messageScript = null;
            return false;
        }
        return true;
    }

    public void srvHelp(userHelping l) {
        l.add("1 2  codec                          set codec to use");
        l.add("2 .    alaw                         g711 a law");
        l.add("2 .    ulaw                         g711 u law");
        l.add("1 2  voice-script                   set tcl script to run on incoming call");
        l.add("2 .    <name>                       file name");
        l.add("1 2  message-script                 set tcl script to run on incoming message");
        l.add("2 .    <name>                       file name");
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.timeout = 180000;
        pipe.lineRx = pipeSide.modTyp.modeCRtryLF;
        pipe.lineTx = pipeSide.modTyp.modeCRLF;
        new servVoiceDoer(this, pipe, id);
        return false;
    }

    /**
     * get data port number
     *
     * @return port number
     */
    protected int getDataPort() {
        return (srvPort - 3) & 0xfffe;
    }

}

class servVoiceDoer implements Runnable {

    public servVoice lower;

    public pipeSide ctrl;

    public prtGenConn conn;

    public tabGen<servVoiceConn> conns = new tabGen<servVoiceConn>();

    public servVoiceDoer(servVoice parent, pipeSide stream, prtGenConn id) {
        lower = parent;
        ctrl = stream;
        conn = id;
        new Thread(this).start();
    }

    public String getContact() {
        return "<sip:voice@" + uniResLoc.addr2str(conn.iface.addr, conn.portLoc) + ">";
    }

    public sndCodec getCodec() {
        if (lower.aLaw) {
            return new sndCodecG711aLaw();
        } else {
            return new sndCodecG711uLaw();
        }
    }

    private void doer() {
        for (;;) {
            packSip rx = new packSip(ctrl);
            if (rx.readUp()) {
                return;
            }
            if (debugger.servVoiceTraf) {
                rx.dump("rx");
            }
            packSip tx = new packSip(ctrl);
            cmds cmd = new cmds("sip", rx.command);
            String a = cmd.word().toLowerCase();
            if (a.length() < 1) {
                continue;
            }
            if (a.equals("register") || a.equals("subscribe")) {
                tx.makeOk(rx, getContact(), 120);
                if (debugger.servVoiceTraf) {
                    tx.dump("tx");
                }
                tx.writeDown();
                continue;
            }
            if (a.equals("options")) {
                tx.makeOk(rx, getContact(), 0);
                if (debugger.servVoiceTraf) {
                    tx.dump("tx");
                }
                tx.writeDown();
                continue;
            }
            if (a.equals("message")) {
                tx.makeOk(rx, getContact(), 0);
                if (debugger.servVoiceTraf) {
                    tx.dump("tx");
                }
                tx.writeDown();
                if (lower.messageScript == null) {
                    continue;
                }
                List<String> scr = bits.txt2buf(lower.messageScript);
                if (scr == null) {
                    continue;
                }
                pipeLine pip = new pipeLine(32768, false);
                pipeSide pipeS = pip.getSide();
                pipeSide pipeC = pip.getSide();
                pipeS.setReady();
                pipeC.setReady();
                pipeC.timeout = 120000;
                pipeC.lineTx = pipeSide.modTyp.modeCRLF;
                pipeC.lineRx = pipeSide.modTyp.modeCRtryLF;
                pipeC.linePut("from=" + uniResLoc.fromEmail(packSip.removeTag(rx.headerGet("From", 1))));
                pipeC.linePut("to=" + uniResLoc.fromEmail(packSip.removeTag(rx.headerGet("To", 1))));
                for (int i = 0; i < rx.content.size(); i++) {
                    pipeC.linePut("text=" + rx.content.get(i));
                }
                pipeC.linePut(".");
                pipeDiscard.discard(pipeC);
                pipeS.timeout = 120000;
                pipeS.lineTx = pipeSide.modTyp.modeCRLF;
                pipeS.lineRx = pipeSide.modTyp.modeCRorLF;
                userScript t = new userScript(pipeS, "");
                t.addLines(scr);
                t.allowConfig = true;
                t.allowExec = true;
                new servVoiceScr(t, pipeS);
                continue;
            }
            if (a.equals("notify")) {
                tx.makeOk(rx, getContact(), 0);
                if (debugger.servVoiceTraf) {
                    tx.dump("tx");
                }
                tx.writeDown();
                continue;
            }
            servVoiceConn cn = new servVoiceConn(this, rx.headerGet("Call-Id", 1));
            servVoiceConn old = conns.find(cn);
            if (old != null) {
                old.callRep = rx.byteCopy(null);
                continue;
            }
            if (a.equals("ack")) {
                continue;
            }
            if (a.equals("bye") || a.equals("cancel")) {
                tx.makeOk(rx, getContact(), 0);
                if (debugger.servVoiceTraf) {
                    tx.dump("tx");
                }
                tx.writeDown();
                continue;
            }
            if (!a.equals("invite")) {
                continue;
            }
            cn.callInv = rx.byteCopy(null);
            cn.startWork();
            conns.put(cn);
        }
    }

    public void run() {
        if (debugger.servVoiceTraf) {
            logger.debug("started");
        }
        try {
            doer();
        } catch (Exception e) {
            logger.traceback(e);
        }
        ctrl.setClose();
        if (debugger.servVoiceTraf) {
            logger.debug("stopped");
        }
    }

}

class servVoiceScr implements Runnable {

    private userScript s;

    private pipeSide p;

    public servVoiceScr(userScript t, pipeSide c) {
        s = t;
        p = c;
        new Thread(this).start();
    }

    public void run() {
        try {
            s.cmdAll();
        } catch (Exception e) {
            logger.traceback(e);
        }
        p.setClose();
    }

}

class servVoiceConn implements Runnable, Comparator<servVoiceConn> {

    public final String callId;

    public servVoiceDoer lower;

    public packSip callRep;

    public packSip callInv;

    public packRtp data;

    public pipeSide pipeC;

    public pipeSide pipeS;

    public servVoiceConn(servVoiceDoer parent, String cid) {
        lower = parent;
        callId = cid;
    }

    public void startWork() {
        data = new packRtp();
        pipeLine pip = new pipeLine(32768, false);
        pipeS = pip.getSide();
        pipeC = pip.getSide();
        pipeS.setReady();
        pipeC.setReady();
        new Thread(this).start();
    }

    public int compare(servVoiceConn o1, servVoiceConn o2) {
        return o1.callId.compareTo(o2.callId);
    }

    public void run() {
        try {
            doer();
        } catch (Exception e) {
            logger.traceback(e);
        }
        data.setClose();
        pipeC.setClose();
        pipeS.setClose();
        lower.conns.del(this);
    }

    public void ans() {
        packSip tx = new packSip(lower.ctrl);
        tx.makeNumeric("100 trying", callInv, lower.getContact());
        if (debugger.servVoiceTraf) {
            tx.dump("tx");
        }
        tx.writeDown();
        String trg = callInv.headerGet("To", 1);
        trg = packSip.updateTag(trg);
        callInv.headerSet("To", 1, trg);
        tx.makeNumeric("180 ringing", callInv, lower.getContact());
        if (debugger.servVoiceTraf) {
            tx.dump("tx");
        }
        tx.writeDown();
        tx.makeOk(callInv, lower.getContact(), 0);
        tx.makeSdp(lower.conn.iface.addr, lower.lower.getDataPort(), lower.getCodec());
        if (debugger.servVoiceTraf) {
            tx.dump("tx");
        }
        tx.writeDown();
    }

    public void bye() {
        packSip tx = new packSip(lower.ctrl);
        String a = callInv.headerGet("CSeq", 1) + " ";
        int csq = a.indexOf(" ");
        csq = bits.str2num(a.substring(0, csq).trim());
        String via = callInv.headerGet("Via", 1);
        String src = callInv.headerGet("From", 1);
        String trg = callInv.headerGet("To", 1);
        String cid = callInv.headerGet("Call-Id", 1);
        String cnt = uniResLoc.fromEmail(callInv.headerGet("Contact", 1));
        tx.makeReq("BYE", cnt, trg, src, lower.getContact(), via, cid, csq + 1, 0);
        if (debugger.servVoiceTraf) {
            tx.dump("tx");
        }
        tx.writeDown();
    }

    public void doer() {
        ans();
        addrIP adr = new addrIP();
        int prt = callInv.sdpGetMediaEP(adr);
        if (prt < 1) {
            bye();
            return;
        }
        if (data.startConnect(lower.lower.srvVrf.getUdp(adr), new pipeLine(32768, true), lower.conn.iface, lower.lower.getDataPort(), adr, prt)) {
            bye();
            return;
        }
        List<String> scr = bits.txt2buf(lower.lower.voiceScript);
        if (scr == null) {
            data.setClose();
            pipeS.setClose();
            pipeC.setClose();
        } else {
            pipeC.timeout = 120000;
            pipeC.lineTx = pipeSide.modTyp.modeCRLF;
            pipeC.lineRx = pipeSide.modTyp.modeCRtryLF;
            new sndScript(pipeC, lower.getCodec(), data, uniResLoc.fromEmail(packSip.removeTag(callInv.headerGet("From", 1))), uniResLoc.fromEmail(packSip.removeTag(callInv.headerGet("To", 1))));
            pipeS.timeout = 120000;
            pipeS.lineTx = pipeSide.modTyp.modeCRLF;
            pipeS.lineRx = pipeSide.modTyp.modeCRorLF;
            userScript t = new userScript(pipeS, "");
            t.addLines(scr);
            t.allowConfig = true;
            t.allowExec = true;
            new servVoiceScr(t, pipeS);
        }
        for (;;) {
            if (lower.ctrl.isClosed() != 0) {
                break;
            }
            if (data.isClosed() != 0) {
                break;
            }
            if (pipeC.isClosed() != 0) {
                break;
            }
            if (pipeS.isClosed() != 0) {
                break;
            }
            if (callRep == null) {
                bits.sleep(1000);
                continue;
            }
            packSip rx = callRep.byteCopy(null);
            callRep = null;
            cmds cmd = new cmds("sip", rx.command);
            String a = cmd.word().toLowerCase();
            if (a.equals("invite")) {
                ans();
                continue;
            }
            if (a.equals("bye") || a.equals("cancel")) {
                packSip tx = new packSip(lower.ctrl);
                tx.makeOk(rx, lower.getContact(), 0);
                if (debugger.servVoiceTraf) {
                    tx.dump("tx");
                }
                tx.writeDown();
                return;
            }
        }
        bye();
    }

}
