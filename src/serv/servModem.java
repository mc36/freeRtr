package serv;

import addr.addrIP;
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
     * line configuration
     */
    protected userLine lin = new userLine();

    /**
     * defaults text
     */
    public final static String defaultL[] = {
        "server modem .*! port " + packSip.port,
        "server modem .*! protocol " + proto2string(protoAllDgrm),
        "server modem .*! codec alaw"
    };

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
        lin.getShRun(beg, lst);
    }

    public boolean srvCfgStr(cmds cmd) {
        cmds c = cmd.copyBytes(false);
        String a = c.word();
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

    private servModem lower;

    private pipeSide ctrl;

    private packRtp data;

    private prtGenConn conn;

    private pipeSide pipeC;

    private pipeSide pipeS;

    public servModemDoer(servModem parent, pipeSide stream, prtGenConn id) {
        lower = parent;
        ctrl = stream;
        conn = id;
        data = new packRtp();
        pipeLine pip = new pipeLine(32768, false);
        pipeS = pip.getSide();
        pipeC = pip.getSide();
        pipeC.setReady();
        new Thread(this).start();
    }

    private String getContact() {
        return "<sip:modem@" + uniResLoc.addr2str(conn.iface.addr, conn.portLoc) + ">";
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
            if (debugger.servModemTraf) {
                tx.dump("tx");
            }
            tx.writeDown();
            return 0;
        }
        if (s.equals("options")) {
            tx.makeOk(rx, getContact(), 0);
            if (debugger.servModemTraf) {
                tx.dump("tx");
            }
            tx.writeDown();
            return 0;
        }
        if (s.equals("bye")) {
            tx.makeOk(rx, getContact(), 0);
            if (debugger.servModemTraf) {
                tx.dump("tx");
            }
            tx.writeDown();
            return 1;
        }
        if (s.equals("cancel")) {
            tx.makeOk(rx, getContact(), 0);
            if (debugger.servModemTraf) {
                tx.dump("tx");
            }
            tx.writeDown();
            return 1;
        }
        if (s.equals("message")) {
            tx.makeOk(rx, getContact(), 0);
            if (debugger.servModemTraf) {
                tx.dump("tx");
            }
            tx.writeDown();
            return 0;
        }
        if (s.equals("notify")) {
            tx.makeOk(rx, getContact(), 0);
            if (debugger.servModemTraf) {
                tx.dump("tx");
            }
            tx.writeDown();
            return 0;
        }
        if (s.equals("invite")) {
            tx.makeNumeric("100 trying", rx, getContact());
            if (debugger.servModemTraf) {
                tx.dump("tx");
            }
            tx.writeDown();
            tx.makeNumeric("180 ringing", rx, getContact());
            if (debugger.servModemTraf) {
                tx.dump("tx");
            }
            tx.writeDown();
            tx.makeOk(rx, getContact(), 0);
            tx.makeSdp(conn.iface.addr, lower.getDataPort(), getCodec());
            if (debugger.servModemTraf) {
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
        if (debugger.servModemTraf) {
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
        pipeModem.answer(pipeC, getCodec(), data);
        bits.sleep(2000);
        lower.lin.createHandler(pipeS, "" + conn, false);
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
            if (debugger.servModemTraf) {
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
        trg += ";tag=" + bits.randomD();
        tx.makeReq("BYE", getContact(), trg, src, null, via, cid, csq + 1, 0);
        if (debugger.servModemTraf) {
            tx.dump("tx");
        }
        tx.writeDown();
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
        data.setClose();
        pipeC.setClose();
        pipeS.setClose();
        if (debugger.servModemTraf) {
            logger.debug("stopped");
        }
    }

}
