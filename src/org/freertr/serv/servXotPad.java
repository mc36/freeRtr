package org.freertr.serv;

import java.util.List;
import org.freertr.pack.packXotPad;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGenConn;
import org.freertr.prt.prtServS;
import org.freertr.tab.tabGen;
import org.freertr.user.userFilter;
import org.freertr.user.userHelping;
import org.freertr.user.userLine;
import org.freertr.util.cmds;
import org.freertr.util.logger;

/**
 * x25 over tcp (rfc1613) server
 *
 * @author matecsaba
 */
public class servXotPad extends servGeneric implements prtServS {

    /**
     * create instance
     */
    public servXotPad() {
    }

    /**
     * line handler
     */
    protected userLine lin = new userLine();

    /**
     * defaults text
     */
    public final static String[] defaultL = {
        "server xotpad .*! port " + packXotPad.port,
        "server xotpad .*! no second-port",
        "server xotpad .*! protocol " + proto2string(protoAllStrm)
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public tabGen<userFilter> srvDefFlt() {
        return defaultF;
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        new servXotPadRx(this, id, pipe).startWork();
        return false;
    }

    public void srvShRun(String beg, List<String> lst, int filter) {
        lin.getShRun(beg, lst);
    }

    public boolean srvCfgStr(cmds cmd) {
        return lin.doCfgStr(cmd);
    }

    public void srvHelp(userHelping l) {
        lin.getHelp(l);
    }

    public String srvName() {
        return "xotpad";
    }

    public int srvPort() {
        return packXotPad.port;
    }

    public int srvProto() {
        return protoAllStrm;
    }

    public boolean srvInit() {
        return genStrmStart(this, new pipeLine(32768, false), 0);
    }

    public boolean srvDeinit() {
        return genericStop(0);
    }

}

class servXotPadRx implements Runnable {

    public final servXotPad lower;

    public final packXotPad conn;

    public final pipeLine pl;

    public final pipeSide psx;

    public final pipeSide psc;

    public servXotPadRx(servXotPad parent, prtGenConn id, pipeSide pipe) {
        lower = parent;
        conn = new packXotPad(pipe);
        pl = new pipeLine(32768, false);
        psx = pl.getSide();
        psc = pl.getSide();
        psx.setReady();
        psc.setReady();
        lower.lin.createHandler(psc, "" + id, 0);
    }

    public void startWork() {
        new Thread(this).start();
        new servXotPadTx(this).startWork();
    }

    public void stopWork() {
        conn.setClose();
        psc.setClose();
        psx.setClose();
        pl.setClose();
    }

    public void run() {
        try {
            if (conn.parseCallReq(conn.recvPack())) {
                stopWork();
                return;
            }
            conn.sendPack(conn.createCallAcc());
            conn.doerRx(psx);
        } catch (Exception e) {
            logger.traceback(e);
        }
        stopWork();
    }

}

class servXotPadTx implements Runnable {

    private final servXotPadRx lower;

    public servXotPadTx(servXotPadRx parent) {
        lower = parent;
    }

    public void run() {
        try {
            lower.conn.doerTx(lower.psx);
        } catch (Exception e) {
            logger.traceback(e);
        }
        lower.stopWork();
    }

    public void startWork() {
        new Thread(this).start();
    }

}
