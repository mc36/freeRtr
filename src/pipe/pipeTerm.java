package pipe;

import sec.secClient;
import serv.servGeneric;
import util.bits;
import util.logger;

/**
 * terminal emulator
 *
 * @author matecsaba
 */
public class pipeTerm {

    /**
     * stream handler
     */
    protected pipeSide stream;

    /**
     * console handler
     */
    protected pipeSide console;

    /**
     * rx thread running
     */
    protected boolean runningRx;

    /**
     * tx thread running
     */
    protected boolean runningTx;

    /**
     * threads needed
     */
    protected boolean runningNeed;

    /**
     * thread time in millisecs
     */
    protected final static int threadTime = 200;

    /**
     * thread buffer size
     */
    protected final static int theradBuf = 1024;

    /**
     * create one terminal
     *
     * @param con console to use
     * @param strm stream to use
     */
    public pipeTerm(pipeSide con, pipeSide strm) {
        console = con;
        stream = strm;
    }

    /**
     * terminal session
     */
    public void doTerm() {
        if (stream == null) {
            return;
        }
        if (stream.isClosed() != 0) {
            return;
        }
        runningRx = true;
        runningTx = true;
        runningNeed = true;
        console.linePut("");
        new Thread(new pipeTermRx(this)).start();
        new Thread(new pipeTermTx(this)).start();
        for (;;) {
            bits.sleep(threadTime * 2);
            if (stream.isClosed() != 0) {
                break;
            }
            if (console.isClosed() != 0) {
                break;
            }
        }
        console.linePut("");
        stream.setClose();
        runningNeed = false;
        for (; runningTx || runningRx;) {
            bits.sleep(500);
        }
        console.linePut("connection closed");
    }

}

class pipeTermRx implements Runnable {

    private pipeTerm parent;

    public pipeTermRx(pipeTerm prnt) {
        parent = prnt;
    }

    public void run() {
        try {
            for (;;) {
                if (!parent.runningNeed) {
                    break;
                }
                byte buf[] = new byte[pipeTerm.theradBuf];
                int siz = parent.stream.nonBlockGet(buf, 0, buf.length);
                if (siz < 1) {
                    bits.sleep(pipeTerm.threadTime);
                    continue;
                }
                parent.console.blockingPut(buf, 0, siz);
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
        parent.runningRx = false;
    }

}

class pipeTermTx implements Runnable {

    private pipeTerm parent;

    public pipeTermTx(pipeTerm prnt) {
        parent = prnt;
    }

    public void run() {
        try {
            for (;;) {
                if (!parent.runningNeed) {
                    break;
                }
                byte buf[] = new byte[pipeTerm.theradBuf];
                int siz = parent.console.nonBlockGet(buf, 0, buf.length);
                if (siz < 1) {
                    bits.sleep(pipeTerm.threadTime);
                    continue;
                }
                if ((siz > 1) || (buf[0] != 0x3)) {
                    parent.stream.blockingPut(buf, 0, siz);
                    continue;
                }
                siz = parent.console.blockingGet(buf, 0, 1);
                if (siz != 1) {
                    continue;
                }
                switch (buf[0]) {
                    case 104: // h
                    case 72: // H
                    case 63: // ?
                    case 8: // ctrl+h
                        parent.console.linePut("");
                        parent.console.linePut("^H - show this help message");
                        parent.console.linePut("^T - start tls client");
                        parent.console.linePut("^C - send one ctrl+c character");
                        parent.console.linePut("^V - send two ctrl+c characters");
                        parent.console.linePut("^X - close terminal session");
                        break;
                    case 116: // t
                    case 84: // T
                    case 20: // ctrl+t
                        parent.console.strPut("tls negotiation");
                        pipeSide res = secClient.openSec(parent.stream, servGeneric.protoTls, null, null);
                        if (res == null) {
                            parent.console.linePut(" failed!");
                        } else {
                            parent.console.linePut(" success!");
                            parent.stream = res;
                        }
                        break;
                    case 99: // c
                    case 67: // C
                    case 3: // ctrl+c
                        buf[0] = 3;
                        parent.stream.blockingPut(buf, 0, 1);
                        break;
                    case 118: // v
                    case 86: // V
                    case 22: // ctrl+v
                        buf[0] = 3;
                        buf[1] = 3;
                        parent.stream.blockingPut(buf, 0, 2);
                        break;
                    case 120: // x
                    case 88: // X
                    case 24: // ctrl+x
                        parent.stream.setClose();
                        break;
                    default:
                        parent.stream.blockingPut(buf, 0, 1);
                        break;
                }
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
        parent.runningTx = false;
    }

}
