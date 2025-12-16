package org.freertr.sec;

import java.util.ArrayList;
import java.util.List;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.pipe.pipeTerm;
import org.freertr.util.bits;
import org.freertr.util.debugger;
import org.freertr.util.logger;

/**
 * telnet (rfc854) protocol
 *
 * @author matecsaba
 */
public class secTelnet {

    /**
     * lower layer session to use for encrypted communication
     */
    public final pipeSide lower;

    /**
     * user side of cleartext pipeline
     */
    public final pipeSide userC;

    /**
     * mode of operation, true=client, false=server
     */
    protected boolean client;

    /**
     * my side of cleartext pipeline
     */
    protected final pipeSide userS;

    /**
     * cleartext pipeline
     */
    protected final pipeLine userP;

    /**
     * beginning of commmand
     */
    public final static int cmdIAC = 255;

    /**
     * don't command
     */
    public final static int cmdDONT = 254;

    /**
     * do command
     */
    public final static int cmdDO = 253;

    /**
     * wont command
     */
    public final static int cmdWONT = 252;

    /**
     * will command
     */
    public final static int cmdWILL = 251;

    /**
     * subnegotiation begin
     */
    public final static int cmdSB = 250;

    /**
     * go ahead
     */
    public final static int cmdGA = 249;

    /**
     * erase line
     */
    public final static int cmdEL = 248;

    /**
     * erase line
     */
    public final static int cmdEC = 247;

    /**
     * are you there
     */
    public final static int cmdAYT = 246;

    /**
     * abort
     */
    public final static int cmdAbrt = 245;

    /**
     * interrupt process
     */
    public final static int cmdInt = 244;

    /**
     * break
     */
    public final static int cmdBrk = 243;

    /**
     * data mark
     */
    public final static int cmdMrk = 242;

    /**
     * no operation
     */
    public final static int cmdNop = 241;

    /**
     * subnegotiation end
     */
    public final static int cmdSE = 240;

    /**
     * binary transmission
     */
    public final static int optBin = 0;

    /**
     * echo mode
     */
    public final static int optEcho = 1;

    /**
     * suppress gaoptEcho
     */
    public final static int optSuppGA = 3;

    /**
     * terminal location
     */
    public final static int optTerLoc = 23;

    /**
     * terminal type
     */
    public final static int optTerTyp = 24;

    /**
     * window size
     */
    public final static int optWinSiz = 31;

    /**
     * send location
     *
     * @param pip pipe to use
     * @param loc location to send
     */
    public static void sendLocation(pipeSide pip, String loc) {
        byte[] buf = loc.getBytes();
        buf = bits.byteConcat(sndLocBeg, buf);
        buf = bits.byteConcat(buf, sndLocEnd);
        pip.blockingPut(buf, 0, buf.length);
    }

    private final static byte[] sndLocBeg = new byte[]{(byte) cmdIAC, (byte) cmdSB, (byte) optTerLoc};

    private final static byte[] sndLocEnd = new byte[]{(byte) cmdIAC, (byte) cmdSE};

    /**
     * receive location
     *
     * @param pip pipe to use
     * @return location got, null if nothing
     */
    public static String recvLocation(pipeSide pip) {
        byte[] buf = new byte[512];
        for (int r = 0; r < 10; r++) {
            pip.notif.misleep(500);
            int o = pip.nonDestructiveGet(buf, 0, buf.length);
            if (o == pipeLine.tryLater) {
                continue;
            }
            if (o < 0) {
                return null;
            }
            if (o < 5) {
                continue;
            }
            if (bits.byteComp(buf, 0, sndLocBeg, 0, sndLocBeg.length) != 0) {
                return null;
            }
            int p = -1;
            for (int i = 0; i < o; i++) {
                if (buf[i] == (byte) cmdSE) {
                    p = i + 1;
                    break;
                }
            }
            if (p < 0) {
                continue;
            }
            p -= sndLocEnd.length;
            if (bits.byteComp(buf, p, sndLocEnd, 0, sndLocEnd.length) != 0) {
                return null;
            }
            String a = "";
            for (int i = sndLocBeg.length; i < p; i++) {
                a += (char) buf[i];
            }
            pip.blockingGet(buf, 0, p + sndLocEnd.length);
            return a;
        }
        return null;
    }

    /**
     * convert command to string
     *
     * @param i command
     * @return string
     */
    public static String command2string(int i) {
        switch (i) {
            case cmdIAC:
                return "iac";
            case cmdDONT:
                return "dont";
            case cmdDO:
                return "do";
            case cmdWONT:
                return "wont";
            case cmdWILL:
                return "will";
            case cmdSB:
                return "sb";
            case cmdGA:
                return "ga";
            case cmdEL:
                return "el";
            case cmdEC:
                return "ec";
            case cmdAYT:
                return "ayt";
            case cmdAbrt:
                return "abort";
            case cmdInt:
                return "int";
            case cmdBrk:
                return "break";
            case cmdMrk:
                return "mark";
            case cmdNop:
                return "nop";
            case cmdSE:
                return "se";
            default:
                return "unknown=" + i;
        }
    }

    /**
     * convert option to string
     *
     * @param i option
     * @return string
     */
    public static String option2string(int i) {
        switch (i) {
            case optBin:
                return "binary";
            case optEcho:
                return "echo";
            case optSuppGA:
                return "suppGa";
            case optTerLoc:
                return "termLoc";
            case optTerTyp:
                return "termTyp";
            case optWinSiz:
                return "winSiz";
            default:
                return "unknown=" + i;
        }
    }

    /**
     * start telnet on a session
     *
     * @param session pipeside to use as lower layer
     * @param pipe pipeline to use on user side
     */
    public secTelnet(pipeSide session, pipeLine pipe) {
        lower = session;
        lower.setTime(120 * 1000);
        userP = pipe;
        userS = pipe.getSide();
        userC = pipe.getSide();
        userC.setTime(120 * 1000);
        userS.setTime(120 * 1000);
    }

    /**
     * get user side pipeline
     *
     * @return cleartext pipeline
     */
    public pipeSide getPipe() {
        return userC;
    }

    /**
     * start client connection
     */
    public void startClient() {
        client = true;
        workerStart();
    }

    /**
     * start server connection
     */
    public void startServer() {
        client = false;
        workerStart();
    }

    private void workerStart() {
        if (debugger.secTelnetTraf) {
            logger.debug("starting");
        }
        userS.setReady();
        new secTelnetRx(this);
        new secTelnetTx(this);
    }

    /**
     * stop threads
     */
    protected void workerStop() {
        userP.setClose();
        lower.setClose();
    }

    /**
     * sender worker
     */
    protected void workerTx() {
        if (debugger.secTelnetTraf) {
            logger.debug("tx started");
        }
        for (;;) {
            lower.setTime(userS.getTime());
            byte[] bufi = new byte[1024];
            int i = userS.blockingGet(bufi, 0, 1);
            if (i < 0) {
                return;
            }
            int o = userS.nonBlockGet(bufi, i, bufi.length - i);
            if (o < 0) {
                o = 0;
            }
            o += i;
            byte[] bufo = new byte[2048];
            int p = 0;
            for (i = 0; i < o; i++) {
                int q = bufi[i] & 0xff;
                if (q != cmdIAC) {
                    bufo[p] = (byte) q;
                    p++;
                    continue;
                }
                bufo[p] = (byte) cmdIAC;
                p++;
                bufo[p] = (byte) cmdIAC;
                p++;
            }
            lower.blockingPut(bufo, 0, p);
        }
    }

    private int netRx() {
        byte[] buf = new byte[1];
        if (lower.blockingGet(buf, 0, buf.length) != buf.length) {
            return -1;
        }
        return buf[0] & 0xff;
    }

    private void netTx(int opt, byte[] val) {
        if (debugger.secTelnetTraf) {
            logger.debug("tx opt=" + option2string(opt) + " buf=" + bits.byteDump(val, 0, -1));
        }
        byte[] buf = new byte[3];
        buf[0] = (byte) cmdIAC;
        buf[1] = (byte) cmdSB;
        buf[2] = (byte) opt;
        lower.blockingPut(buf, 0, buf.length);
        for (int o = 0; o < val.length; o++) {
            int i = val[o] & 0xff;
            if (i != cmdIAC) {
                lower.blockingPut(val, o, 1);
                continue;
            }
            buf = new byte[2];
            buf[0] = (byte) cmdIAC;
            buf[1] = (byte) cmdIAC;
            lower.blockingPut(buf, 0, buf.length);
        }
        buf = new byte[2];
        buf[0] = (byte) cmdIAC;
        buf[1] = (byte) cmdSE;
        lower.blockingPut(buf, 0, buf.length);
    }

    private void netTx(int cmd, int opt) {
        if (debugger.secTelnetTraf) {
            logger.debug("tx cmd=" + command2string(cmd) + " opt=" + option2string(opt));
        }
        byte[] buf = new byte[3];
        buf[0] = (byte) cmdIAC;
        buf[1] = (byte) cmd;
        buf[2] = (byte) opt;
        lower.blockingPut(buf, 0, buf.length);
    }

    /**
     * receiver worker
     */
    protected void workerRx() {
        if (debugger.secTelnetTraf) {
            logger.debug("rx started");
        }
        netTx(cmdWILL, optBin);
        netTx(cmdDO, optBin);
        int i;
        int o;
        if (client) {
            i = cmdWONT;
            o = cmdDO;
            netTx(cmdWILL, optTerTyp);
        } else {
            i = cmdWILL;
            o = cmdDONT;
            netTx(cmdDO, optWinSiz);
        }
        netTx(i, optEcho);
        netTx(o, optEcho);
        netTx(i, optSuppGA);
        netTx(o, optSuppGA);
        for (;;) {
            i = netRx();
            if (i < 0) {
                return;
            }
            if (i != cmdIAC) {
                byte[] buf = new byte[1];
                buf[0] = (byte) i;
                userS.blockingPut(buf, 0, buf.length);
                continue;
            }
            i = netRx();
            if (debugger.secTelnetTraf) {
                logger.debug("rx cmd=" + command2string(i));
            }
            o = -1;
            switch (i) {
                case cmdIAC:
                    byte[] buf = new byte[1];
                    buf[0] = (byte) cmdIAC;
                    userS.blockingPut(buf, 0, buf.length);
                    continue;
                case cmdSB:
                    o = netRx();
                    if (debugger.secTelnetTraf) {
                        logger.debug("rx opt=" + option2string(o));
                    }
                    List<Integer> lst = new ArrayList<Integer>();
                    for (;;) {
                        i = netRx();
                        if (i < 0) {
                            return;
                        }
                        if (i != cmdIAC) {
                            lst.add(i);
                            continue;
                        }
                        i = netRx();
                        if (i == cmdSE) {
                            break;
                        }
                        lst.add(i);
                    }
                    switch (o) {
                        case optTerTyp:
                            if (lst.size() < 1) {
                                break;
                            }
                            if (lst.get(0) != 1) {
                                break;
                            }
                            buf = "\000ansi".getBytes();
                            netTx(optTerTyp, buf);
                            break;
                        case optWinSiz:
                            if (lst.size() < 4) {
                                break;
                            }
                            i = (lst.get(0) << 8) | lst.get(1);
                            pipeTerm.setTermWdt(userS, i);
                            i = (lst.get(2) << 8) | lst.get(3);
                            pipeTerm.setTermLen(userS, i);
                            break;
                    }
                    continue;
                case cmdDO:
                    o = cmdWONT;
                    break;
                case cmdDONT:
                    o = cmdWONT;
                    break;
                case cmdWILL:
                    o = cmdDONT;
                    break;
                case cmdWONT:
                    break;
                default:
                    continue;
            }
            i = netRx();
            if (debugger.secTelnetTraf) {
                logger.debug("rx opt=" + option2string(i));
            }
            switch (i) {
                case optBin:
                case optEcho:
                case optSuppGA:
                    continue;
                case optTerTyp:
                    if (!client) {
                        break;
                    }
                    continue;
                case optWinSiz:
                    if (client) {
                        break;
                    }
                    continue;
            }
            if (o < 0) {
                continue;
            }
            netTx(o, i);
        }
    }

}

class secTelnetRx implements Runnable {

    private final secTelnet lower;

    public secTelnetRx(secTelnet parent) {
        lower = parent;
        new Thread(this).start();
    }

    public void run() {
        try {
            lower.workerRx();
        } catch (Exception e) {
            logger.traceback(e);
        }
        lower.workerStop();
        if (debugger.secTelnetTraf) {
            logger.debug("rx stopped");
        }
    }

}

class secTelnetTx implements Runnable {

    private final secTelnet lower;

    public secTelnetTx(secTelnet parent) {
        lower = parent;
        new Thread(this).start();
    }

    public void run() {
        try {
            lower.workerTx();
        } catch (Exception e) {
            logger.traceback(e);
        }
        lower.workerStop();
        if (debugger.secTelnetTraf) {
            logger.debug("tx stopped");
        }
    }

}
