package serv;

import addr.addrIP;
import java.util.List;
import pack.packRtp;
import pack.packSip;
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
     * script name
     */
    protected String script;

    /**
     * defaults text
     */
    public final static String defaultL[] = {
        "server voice .*! port " + packSip.port,
        "server voice .*! protocol " + proto2string(protoAllDgrm),
        "server voice .*! codec alaw",
        "server voice .*! no script"
    };

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
        cmds.cfgLine(lst, script == null, beg, "script", script);
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
        if (a.equals("script")) {
            script = cmd.getRemaining();
            return false;
        }
        if (!a.equals("no")) {
            return true;
        }
        a = cmd.word();
        if (a.equals("script")) {
            script = null;
            return false;
        }
        return true;
    }

    public void srvHelp(userHelping l) {
        l.add("1 2  codec                          set codec to use");
        l.add("2 .    alaw                         g711 a law");
        l.add("2 .    ulaw                         g711 u law");
        l.add("1 2  script                         set tcl script to run");
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

    private servVoice lower;

    private pipeSide ctrl;

    private packRtp data;

    private prtGenConn conn;

    private pipeSide pipeC;

    private pipeSide pipeS;

    public servVoiceDoer(servVoice parent, pipeSide stream, prtGenConn id) {
        lower = parent;
        ctrl = stream;
        conn = id;
        data = new packRtp();
        pipeLine pip = new pipeLine(32768, false);
        pipeS = pip.getSide();
        pipeC = pip.getSide();
        pipeS.setReady();
        pipeC.setReady();
        new Thread(this).start();
    }

    private String getContact() {
        return "<sip:voice@" + uniResLoc.addr2str(conn.iface.addr, conn.portLoc) + ">";
    }

    private sndCodec getCodec() {
        if (lower.aLaw) {
            return new sndCodecG711aLaw();
        } else {
            return new sndCodecG711uLaw();
        }
    }

    private int replyMessage(packSip rx) {
        packSip tx = new packSip(ctrl);
        String s = rx.command.trim();
        int i = s.indexOf(" ");
        if (i < 1) {
            return 0;
        }
        s = s.substring(0, i).trim().toLowerCase();
        if (s.equals("ack")) {
            return 0;
        }
        if (s.equals("register") || s.equals("subscribe")) {
            tx.makeOk(rx, getContact(), 120);
            if (debugger.servVoiceTraf) {
                tx.dump("tx");
            }
            tx.writeDown();
            return 0;
        }
        if (s.equals("options")) {
            tx.makeOk(rx, getContact(), 0);
            if (debugger.servVoiceTraf) {
                tx.dump("tx");
            }
            tx.writeDown();
            return 0;
        }
        if (s.equals("bye")) {
            tx.makeOk(rx, getContact(), 0);
            if (debugger.servVoiceTraf) {
                tx.dump("tx");
            }
            tx.writeDown();
            return 1;
        }
        if (s.equals("cancel")) {
            tx.makeOk(rx, getContact(), 0);
            if (debugger.servVoiceTraf) {
                tx.dump("tx");
            }
            tx.writeDown();
            return 1;
        }
        if (s.equals("message")) {
            tx.makeOk(rx, getContact(), 0);
            if (debugger.servVoiceTraf) {
                tx.dump("tx");
            }
            tx.writeDown();
            return 0;
        }
        if (s.equals("notify")) {
            tx.makeOk(rx, getContact(), 0);
            if (debugger.servVoiceTraf) {
                tx.dump("tx");
            }
            tx.writeDown();
            return 0;
        }
        if (s.equals("invite")) {
            tx.makeNumeric("100 trying", rx, getContact());
            if (debugger.servVoiceTraf) {
                tx.dump("tx");
            }
            tx.writeDown();
            String trg = rx.headerGet("To", 1);
            if (trg.indexOf(";tag=") < 0) {
                trg += ";tag=" + bits.randomD();
            }
            rx.headerSet("To", 1, trg);
            tx.makeNumeric("180 ringing", rx, getContact());
            if (debugger.servVoiceTraf) {
                tx.dump("tx");
            }
            tx.writeDown();
            tx.makeOk(rx, getContact(), 0);
            tx.makeSdp(conn.iface.addr, lower.getDataPort(), getCodec());
            if (debugger.servVoiceTraf) {
                tx.dump("tx");
            }
            tx.writeDown();
            return 2;
        }
        return 0;
    }

    private void doer() {
        packSip rx = new packSip(ctrl);
        if (rx.readUp()) {
            return;
        }
        if (debugger.servVoiceTraf) {
            rx.dump("rx");
        }
        if (replyMessage(rx) != 2) {
            return;
        }
        packSip sip = rx.byteCopy(ctrl);
        addrIP adr = new addrIP();
        int prt = rx.sdpGetMediaEP(adr);
        if (prt < 1) {
            return;
        }
        if (data.startConnect(lower.srvVrf.getUdp(adr), new pipeLine(32768, true), conn.iface, lower.getDataPort(), adr, prt)) {
            return;
        }
        sndCodec codec = getCodec();
        List<String> scr = bits.txt2buf(lower.script);
        if (scr == null) {
            data.setClose();
            pipeS.setClose();
            pipeC.setClose();
        } else {
            pipeC.timeout = 120000;
            pipeC.lineTx = pipeSide.modTyp.modeCRLF;
            pipeC.lineRx = pipeSide.modTyp.modeCRtryLF;
            new sndScript(pipeC, codec, data, uniResLoc.fromEmail(sip.headerGet("From", 1)), uniResLoc.fromEmail(sip.headerGet("To", 1)));
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
            if (data.isClosed() != 0) {
                break;
            }
            if (pipeC.isClosed() != 0) {
                break;
            }
            if (pipeS.isClosed() != 0) {
                break;
            }
            if (rx.ready2rx() < 1) {
                bits.sleep(1000);
                continue;
            }
            if (rx.readUp()) {
                break;
            }
            if (debugger.servVoiceTraf) {
                rx.dump("rx");
            }
            if (replyMessage(rx) == 1) {
                break;
            }
        }
        packSip tx = new packSip(ctrl);
        String a = rx.headerGet("CSeq", 1) + " ";
        int csq = a.indexOf(" ");
        csq = bits.str2num(a.substring(0, csq).trim());
        String via = sip.headerGet("Via", 1);
        String src = sip.headerGet("From", 1);
        String trg = sip.headerGet("To", 1);
        String cid = sip.headerGet("Call-Id", 1);
        String cnt = uniResLoc.fromEmail(rx.headerGet("Contact", 1));
        tx.makeReq("BYE", cnt, trg, src, getContact(), via, cid, csq + 1, 0);
        if (debugger.servVoiceTraf) {
            tx.dump("tx");
        }
        tx.writeDown();
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
        data.setClose();
        pipeC.setClose();
        pipeS.setClose();
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
