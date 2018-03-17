package serv;

import addr.addrIP;
import java.util.Comparator;
import java.util.List;
import pack.packRtp;
import pack.packSip;
import pipe.pipeLine;
import pipe.pipeModem;
import pipe.pipeSide;
import prt.prtGenConn;
import prt.prtServS;
import snd.sndCodec;
import snd.sndCodecG711aLaw;
import snd.sndCodecG711uLaw;
import tab.tabGen;
import user.userFilter;
import user.userHelping;
import user.userLine;
import util.bits;
import util.cmds;
import util.debugger;
import util.logger;
import util.uniResLoc;

/**
 * modulator demodulator server
 *
 * @author matecsaba
 */
public class servModem extends servGeneric implements prtServS {

    /**
     * codec, true=alaw, false=ulaw
     */
    protected boolean aLaw = true;

    /**
     * mode, true=answer, false=originate
     */
    protected boolean answer = true;

    /**
     * line configuration
     */
    protected userLine lin = new userLine();

    /**
     * defaults text
     */
    public final static String defaultL[] = {
        "server modem .*! port " + packSip.port,
        "server modem .*! protocol " + proto2string(protoAllDgrm),
        "server modem .*! codec alaw",
        "server modem .*! mode answer",};

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public tabGen<userFilter> srvDefFlt() {
        return defaultF;
    }

    public String srvName() {
        return "modem";
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
        if (answer) {
            a = "answer";
        } else {
            a = "originate";
        }
        lst.add(beg + "mode " + a);
        lin.getShRun(beg, lst);
    }

    public boolean srvCfgStr(cmds cmd) {
        cmds c = cmd.copyBytes(false);
        String a = c.word();
        if (a.equals("mode")) {
            a = c.word();
            if (a.equals("answer")) {
                answer = true;
            }
            if (a.equals("originate")) {
                answer = false;
            }
            return false;
        }
        if (a.equals("codec")) {
            a = c.word();
            if (a.equals("alaw")) {
                aLaw = true;
            }
            if (a.equals("ulaw")) {
                aLaw = false;
            }
            return false;
        }
        return lin.doCfgStr(cmd);
    }

    public void srvHelp(userHelping l) {
        l.add("1 2  codec                          set codec to use");
        l.add("2 .    alaw                         g711 a law");
        l.add("2 .    ulaw                         g711 u law");
        l.add("1 2  mode                           set mode to use");
        l.add("2 .    answer                       answer");
        l.add("2 .    originate                    originate");
        lin.getHelp(l);
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.timeout = 180000;
        pipe.lineRx = pipeSide.modTyp.modeCRtryLF;
        pipe.lineTx = pipeSide.modTyp.modeCRLF;
        new servModemDoer(this, pipe, id);
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

class servModemDoer implements Runnable {

    public servModem lower;

    public pipeSide ctrl;

    public prtGenConn conn;

    public tabGen<servModemConn> conns = new tabGen<servModemConn>();

    public servModemDoer(servModem parent, pipeSide stream, prtGenConn id) {
        lower = parent;
        ctrl = stream;
        conn = id;
        new Thread(this).start();
    }

    public String getContact() {
        return "<sip:modem@" + uniResLoc.addr2str(conn.iface.addr, conn.portLoc) + ">";
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
            if (debugger.servModemTraf) {
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
                if (debugger.servModemTraf) {
                    tx.dump("tx");
                }
                tx.writeDown();
                continue;
            }
            if (a.equals("options")) {
                tx.makeOk(rx, getContact(), 0);
                if (debugger.servModemTraf) {
                    tx.dump("tx");
                }
                tx.writeDown();
                continue;
            }
            if (a.equals("message")) {
                tx.makeOk(rx, getContact(), 0);
                if (debugger.servModemTraf) {
                    tx.dump("tx");
                }
                tx.writeDown();
                continue;
            }
            if (a.equals("notify")) {
                tx.makeOk(rx, getContact(), 0);
                if (debugger.servModemTraf) {
                    tx.dump("tx");
                }
                tx.writeDown();
                continue;
            }
            servModemConn cn = new servModemConn(this, rx.headerGet("Call-Id", 1));
            servModemConn old = conns.find(cn);
            if (old != null) {
                old.callRep = rx.byteCopy(null);
                continue;
            }
            if (a.equals("ack")) {
                continue;
            }
            if (a.equals("bye") || a.equals("cancel")) {
                tx.makeOk(rx, getContact(), 0);
                if (debugger.servModemTraf) {
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
        if (debugger.servModemTraf) {
            logger.debug("started");
        }
        try {
            doer();
        } catch (Exception e) {
            logger.traceback(e);
        }
        ctrl.setClose();
        if (debugger.servModemTraf) {
            logger.debug("stopped");
        }
    }

}

class servModemConn implements Runnable, Comparator<servModemConn> {

    public final String callId;

    public servModemDoer lower;

    public packSip callRep;

    public packSip callInv;

    public packRtp data;

    public pipeSide pipeC;

    public pipeSide pipeS;

    public servModemConn(servModemDoer parent, String cid) {
        lower = parent;
        callId = cid;
    }

    public void startWork() {
        data = new packRtp();
        pipeLine pip = new pipeLine(32768, false);
        pipeS = pip.getSide();
        pipeC = pip.getSide();
        pipeC.setReady();
        new Thread(this).start();
    }

    public int compare(servModemConn o1, servModemConn o2) {
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
        if (debugger.servModemTraf) {
            tx.dump("tx");
        }
        tx.writeDown();
        String trg = callInv.headerGet("To", 1);
        trg = packSip.updateTag(trg);
        callInv.headerSet("To", 1, trg);
        tx.makeNumeric("180 ringing", callInv, lower.getContact());
        if (debugger.servModemTraf) {
            tx.dump("tx");
        }
        tx.writeDown();
        tx.makeOk(callInv, lower.getContact(), 0);
        tx.makeSdp(lower.conn.iface.addr, lower.lower.getDataPort(), lower.getCodec());
        if (debugger.servModemTraf) {
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
        if (debugger.servModemTraf) {
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
        if (lower.lower.answer) {
            pipeModem.answer(pipeC, lower.getCodec(), data);
        } else {
            pipeModem.originate(pipeC, lower.getCodec(), data);
        }
        bits.sleep(2000);
        lower.lower.lin.createHandler(pipeS, "" + lower.conn, false);
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
                if (debugger.servModemTraf) {
                    tx.dump("tx");
                }
                return;
            }
        }
        bye();
    }

}
