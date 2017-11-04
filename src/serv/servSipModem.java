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

/**
 * session initiation protocol (rfc3261) server
 *
 * @author matecsaba
 */
public class servSipModem extends servGeneric implements prtServS {

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
        "server sipmodem .*! port " + packSip.port,
        "server sipmodem .*! protocol " + proto2string(protoAllDgrm),
        "server sipmodem .*! codec alaw"
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public tabGen<userFilter> srvDefFlt() {
        return defaultF;
    }

    public String srvName() {
        return "sipmodem";
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
        new servSipModemDoer(this, pipe, id);
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

class servSipModemDoer implements Runnable {

    private servSipModem lower;

    private pipeSide ctrl;

    private packRtp data;

    private prtGenConn conn;

    private pipeSide pipeC;

    private pipeSide pipeS;

    public servSipModemDoer(servSipModem parent, pipeSide stream, prtGenConn id) {
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
        return "<sip:mdm@" + conn.iface.addr + ">";
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
        if (s.equals("register")) {
            tx.makeOk(rx, getContact(), 120);
            tx.writeDown();
            if (debugger.servSipModemTraf) {
                tx.dump("tx");
            }
            return 0;
        }
        if (s.equals("options")) {
            tx.makeOk(rx, getContact(), 0);
            tx.writeDown();
            if (debugger.servSipModemTraf) {
                tx.dump("tx");
            }
            return 0;
        }
        if (s.equals("bye")) {
            tx.makeOk(rx, getContact(), 0);
            tx.writeDown();
            if (debugger.servSipModemTraf) {
                tx.dump("tx");
            }
            return 1;
        }
        if (s.equals("cancel")) {
            tx.makeOk(rx, getContact(), 0);
            tx.writeDown();
            if (debugger.servSipModemTraf) {
                tx.dump("tx");
            }
            return 1;
        }
        if (s.equals("message")) {
            tx.makeOk(rx, getContact(), 0);
            tx.writeDown();
            if (debugger.servSipModemTraf) {
                tx.dump("tx");
            }
            return 0;
        }
        if (s.equals("invite")) {
            tx.makeNumeric("100 trying", rx, getContact());
            tx.writeDown();
            if (debugger.servSipModemTraf) {
                tx.dump("tx");
            }
            tx.makeNumeric("180 ringing", rx, getContact());
            tx.writeDown();
            if (debugger.servSipModemTraf) {
                tx.dump("tx");
            }
            tx.makeOk(rx, getContact(), 0);
            tx.makeSdp(conn.iface.addr, lower.getDataPort(), lower.aLaw);
            tx.writeDown();
            if (debugger.servSipModemTraf) {
                tx.dump("tx");
            }
            return 2;
        }
        return 0;
    }

    private void doer() {
        packSip rx = new packSip(ctrl);
        if (rx.readUp()) {
            return;
        }
        if (debugger.servSipModemTraf) {
            rx.dump("rx");
        }
        if (replyMessage(rx) != 2) {
            return;
        }
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
                return;
            }
            if (rx.readUp()) {
                return;
            }
            if (debugger.servSipModemTraf) {
                rx.dump("rx");
            }
            if (replyMessage(rx) == 1) {
                return;
            }
        }
    }

    public void run() {
        if (debugger.servSipModemTraf) {
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
        if (debugger.servSipModemTraf) {
            logger.debug("stopped");
        }
    }

}
