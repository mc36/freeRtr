package net.freertr.sec;

import java.util.ArrayList;
import java.util.List;
import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeSide;
import net.freertr.user.userReader;
import net.freertr.util.bits;
import net.freertr.util.debugger;
import net.freertr.util.logger;

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
    public static final int cmdIAC = 255;

    /**
     * don't command
     */
    public static final int cmdDONT = 254;

    /**
     * do command
     */
    public static final int cmdDO = 253;

    /**
     * wont command
     */
    public static final int cmdWONT = 252;

    /**
     * will command
     */
    public static final int cmdWILL = 251;

    /**
     * subnegotiation begin
     */
    public static final int cmdSB = 250;

    /**
     * go ahead
     */
    public static final int cmdGA = 249;

    /**
     * erase line
     */
    public static final int cmdEL = 248;

    /**
     * erase line
     */
    public static final int cmdEC = 247;

    /**
     * are you there
     */
    public static final int cmdAYT = 246;

    /**
     * abort
     */
    public static final int cmdAbrt = 245;

    /**
     * interrupt process
     */
    public static final int cmdInt = 244;

    /**
     * break
     */
    public static final int cmdBrk = 243;

    /**
     * data mark
     */
    public static final int cmdMrk = 242;

    /**
     * no operation
     */
    public static final int cmdNop = 241;

    /**
     * subnegotiation end
     */
    public static final int cmdSE = 240;

    /**
     * binary transmission
     */
    public static final int optBin = 0;

    /**
     * echo mode
     */
    public static final int optEcho = 1;

    /**
     * suppress gaoptEcho
     */
    public static final int optSuppGA = 3;

    /**
     * terminal type
     */
    public static final int optTerTyp = 24;

    /**
     * window size
     */
    public static final int optWinSiz = 31;

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
            byte[] buf = new byte[1];
            if (userS.blockingGet(buf, 0, buf.length) != buf.length) {
                return;
            }
            int i = buf[0] & 0xff;
            if (i == secTelnet.cmdIAC) {
                buf = new byte[2];
                buf[0] = (byte) secTelnet.cmdIAC;
                buf[1] = (byte) secTelnet.cmdIAC;
            }
            lower.blockingPut(buf, 0, buf.length);
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
            int i = val[o];
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
            if (i != secTelnet.cmdIAC) {
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
                case secTelnet.cmdIAC:
                    byte[] buf = new byte[1];
                    buf[0] = (byte) secTelnet.cmdIAC;
                    userS.blockingPut(buf, 0, buf.length);
                    continue;
                case secTelnet.cmdSB:
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
                        if (i != secTelnet.cmdIAC) {
                            lst.add(i);
                            continue;
                        }
                        i = netRx();
                        if (i == secTelnet.cmdSE) {
                            break;
                        }
                        lst.add(i);
                    }
                    switch (o) {
                        case secTelnet.optTerTyp:
                            if (lst.size() < 1) {
                                break;
                            }
                            if (lst.get(0) != 1) {
                                break;
                            }
                            buf = "\000ansi".getBytes();
                            netTx(secTelnet.optTerTyp, buf);
                            break;
                        case secTelnet.optWinSiz:
                            if (lst.size() < 4) {
                                break;
                            }
                            i = (lst.get(0) << 8) | lst.get(1);
                            userReader.setTermWdt(userS, i);
                            i = (lst.get(2) << 8) | lst.get(3);
                            userReader.setTermLen(userS, i);
                            break;
                    }
                    continue;
                case secTelnet.cmdDO:
                    o = cmdWONT;
                    break;
                case secTelnet.cmdDONT:
                    o = secTelnet.cmdWONT;
                    break;
                case secTelnet.cmdWILL:
                    o = cmdDONT;
                    break;
                case secTelnet.cmdWONT:
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
