package net.freertr.serv;

import java.util.List;
import net.freertr.pack.packHolder;
import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeSide;
import net.freertr.prt.prtGenConn;
import net.freertr.prt.prtServS;
import net.freertr.tab.tabGen;
import net.freertr.user.userFilter;
import net.freertr.user.userHelping;
import net.freertr.user.userLine;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.debugger;
import net.freertr.util.logger;

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
     * port number
     */
    public static final int port = 1998;

    /**
     * line handler
     */
    protected userLine lin = new userLine();

    /**
     * defaults text
     */
    public final static String[] defaultL = {
        "server xotpad .*! port " + port,
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
        return port;
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

    private final servXotPad lower;

    private final pipeSide conn;

    private final pipeLine pl;

    private final pipeSide psx;

    private final pipeSide psl;

    private int lci;

    private int seqTx;

    private int seqRx;

    private boolean canTx;

    public servXotPadRx(servXotPad parent, prtGenConn id, pipeSide pipe) {
        lower = parent;
        conn = pipe;
        pl = new pipeLine(32768, false);
        psx = pl.getSide();
        psl = pl.getSide();
        psx.setReady();
        psl.setReady();
        lower.lin.createHandler(psl, "" + id, 0);
    }

    public void startWork() {
        new Thread(this).start();
        new servXotPadTx(this).startWork();
    }

    public void stopWork() {
        conn.setClose();
        psl.setClose();
        psx.setClose();
        pl.setClose();
        canTx = true;
    }

    public packHolder recvPack() {
        byte[] buf = new byte[4];
        if (conn.moreGet(buf, 0, buf.length) != buf.length) {
            return null;
        }
        if (bits.msbGetW(buf, 0) != 0) { // version
            return null;
        }
        int len = bits.msbGetW(buf, 2);
        packHolder pck = new packHolder(true, true);
        if (pck.pipeRecv(conn, 0, len, 144) != len) {
            return null;
        }
        if (debugger.servXotpadTraf) {
            logger.debug("rx " + pck.dump());
        }
        return pck;
    }

    public void sendPack(packHolder pck) {
        if (debugger.servXotpadTraf) {
            logger.debug("tx " + pck.dump());
        }
        pck.merge2end();
        pck.msbPutW(0, 0); // version
        pck.msbPutW(2, pck.dataSize());
        pck.putSkip(4);
        pck.merge2beg();
        pck.pipeSend(conn, 0, pck.dataSize(), 2);
    }

    public void doerRx() {
        packHolder pck = recvPack();
        if (pck == null) {
            return;
        }
        if (pck.getByte(0) != 0x10) { // info
            return;
        }
        lci = pck.getByte(1);
        if (pck.getByte(2) != 0x0b) { // call request
            return;
        }
        pck.clear();
        pck.putByte(0, 0x10); // info
        pck.putByte(1, lci);
        pck.putByte(2, 0x0f); // call connected
        pck.putByte(3, 0x00); // address length
        pck.putByte(4, 0x00); // utility length
        pck.putSkip(5);
        sendPack(pck);
        canTx = true;
        for (;;) {
            pck = recvPack();
            if (pck == null) {
                return;
            }
            if (pck.getByte(0) != 0x10) { // info
                return;
            }
            if (lci != pck.getByte(1)) {
                return;
            }
            int typ = pck.getByte(2);
            if ((typ & 0x1f) == 1) { // receiver ready
                canTx = true;
                continue;
            }
            if ((typ & 1) != 0) { // data
                continue;
            }
            pck.getSkip(3);
            pck.pipeSend(psx, 0, pck.dataSize(), 144);
            pck.clear();
            pck.putByte(0, 0x10); // info
            pck.putByte(1, lci);
            pck.putByte(2, (typ << 4) | 1); // command
            pck.putSkip(3);
            sendPack(pck);
            seqRx = (typ >>> 1) & 7;
        }
    }

    public void doerTx() {
        for (;;) {
            if (!canTx) {
                bits.sleep(100);
                continue;
            }
            int len = psx.ready2rx();
            if (len > 128) {
                len = 128;
            }
            if (len < 1) {
                len = 1;
            }
            byte[] buf = new byte[len];
            if (psx.moreGet(buf, 0, buf.length) != buf.length) {
                return;
            }
            packHolder pck = new packHolder(true, true);
            pck.putByte(0, 0x10); // info
            pck.putByte(1, lci);
            pck.putByte(2, (seqTx << 1) | (seqRx << 7)); // data
            pck.putSkip(3);
            pck.putCopy(buf, 0, 0, buf.length);
            pck.putSkip(buf.length);
            sendPack(pck);
            seqTx = (seqTx + 1) & 7;
            canTx = false;
        }
    }

    public void run() {
        try {
            doerRx();
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
            lower.doerTx();
        } catch (Exception e) {
            logger.traceback(e);
        }
        lower.stopWork();
    }

    public void startWork() {
        new Thread(this).start();
    }

}
