package sec;

import addr.addrIP;
import clnt.clntProxy;
import cry.cryBase64;
import cry.cryHashGeneric;
import cry.cryHashSha1;
import pipe.pipeLine;
import pipe.pipeSide;
import serv.servGeneric;
import serv.servHttp;
import user.userTerminal;
import util.bits;
import util.debugger;
import util.logger;
import util.uniResLoc;

/**
 * websocket (rfc6455) protocol
 *
 * @author matecsaba
 */
public class secWebsock {

    /**
     * lower layer session to use for encrypted communication
     */
    public final pipeSide lower;

    /**
     * user side of cleartext pipeline
     */
    public final pipeSide userC;

    /**
     * mode of transmission, true=binary, false=text
     */
    public boolean binary = true;

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
     * hash guid
     */
    public final static String hashGuid = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11";

    /**
     * continuation
     */
    public final static int opcCont = 0;

    /**
     * text data
     */
    public final static int opcText = 1;

    /**
     * binary data
     */
    public final static int opcBinary = 2;

    /**
     * close
     */
    public final static int opcClose = 8;

    /**
     * ping
     */
    public final static int opcPing = 9;

    /**
     * pong
     */
    public final static int opcPong = 10;

    /**
     * calculate hash
     *
     * @param s input
     * @return output
     */
    public final static String calcHash(String s) {
        cryHashGeneric h = new cryHashSha1();
        h.init();
        h.update(s.trim().getBytes());
        h.update(hashGuid.getBytes());
        return cryBase64.encodeBytes(h.finish());
    }

    /**
     * convert opcode to string
     *
     * @param i opcode
     * @return string
     */
    public final static String opcode2string(int i) {
        switch (i) {
            case opcCont:
                return "cont";
            case opcText:
                return "text";
            case opcBinary:
                return "bin";
            case opcClose:
                return "close";
            case opcPing:
                return "ping";
            case opcPong:
                return "pong";
            default:
                return "unknown=" + i;
        }
    }

    /**
     * start websocket on a session
     *
     * @param session pipeside to use as lower layer
     * @param pipe pipeline to use on user side
     */
    public secWebsock(pipeSide session, pipeLine pipe) {
        lower = session;
        lower.timeout = 120 * 1000;
        userP = pipe;
        userS = pipe.getSide();
        userC = pipe.getSide();
        userC.timeout = 120 * 1000;
        userS.timeout = userC.timeout;
    }

    /**
     * start connection
     *
     * @param prx proxy profile to use
     * @param trg target url
     * @param prt protocol
     * @return pipe if success, null on error
     */
    public final static pipeSide doConnect(clntProxy prx, uniResLoc trg, String prt) {
        addrIP adr = userTerminal.justResolv(trg.server, 0);
        if (adr == null) {
            return null;
        }
        pipeSide pipe;
        if (!trg.proto.equals("wss")) {
            pipe = prx.doConnect(servGeneric.protoTcp, adr, trg.getPort(new servHttp().srvPort()), "websock");
        } else {
            pipe = prx.doConnect(servGeneric.protoTcp, adr, trg.getPort(servHttp.securePort), "websocks");
            if (pipe == null) {
                return null;
            }
            pipe = secClient.openSec(pipe, servGeneric.protoTls, null, null);
        }
        if (pipe == null) {
            return null;
        }
        pipe.lineRx = pipeSide.modTyp.modeCRtryLF;
        pipe.lineTx = pipeSide.modTyp.modeCRLF;
        pipe.linePut("GET /" + trg.toPathName() + " HTTP/1.1");
        pipe.linePut("Host: " + trg.server);
        pipe.linePut("Upgrade: websocket");
        pipe.linePut("Connection: Upgrade");
        byte[] buf = new byte[16];
        for (int i = 0; i < buf.length; i++) {
            buf[i] = (byte) bits.randomB();
        }
        pipe.linePut("Sec-WebSocket-Key: " + cryBase64.encodeBytes(buf));
        pipe.linePut("Origin: http://" + trg.server + "/");
        pipe.linePut("Sec-WebSocket-Protocol: " + prt);
        pipe.linePut("Sec-WebSocket-Version: 13");
        pipe.linePut("");
        String s = null;
        for (;;) {
            if (pipe.isClosed() != 0) {
                break;
            }
            String a = pipe.lineGet(1);
            if (s == null) {
                s = a;
            }
            if (a == null) {
                break;
            }
            if (a.length() < 1) {
                break;
            }
        }
        if (s == null) {
            s = "";
        }
        if (!s.startsWith("HTTP/1.1 101 ")) {
            pipe.setClose();
            return null;
        }
        return pipe;
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
        if (debugger.secWebsockTraf) {
            logger.debug("starting");
        }
        userS.setReady();
        new secWebsockRx(this);
        new secWebsockTx(this);
        new secWebsockKep(this);
    }

    /**
     * stop threads
     */
    protected synchronized void workerStop() {
        sendClose(1000, "00end of stream");
        userP.setClose();
        lower.setClose();
    }

    /**
     * sender worker
     */
    protected void workerTx() {
        if (debugger.secWebsockTraf) {
            logger.debug("tx started");
        }
        for (;;) {
            byte[] buf = new byte[1024];
            int i = userS.blockingGet(buf, 0, buf.length);
            if (i < 1) {
                return;
            }
            boolean b;
            if (binary) {
                b = netTx(opcBinary, getMasker(), buf, i);
            } else {
                b = netTx(opcText, getMasker(), buf, i);
            }
            if (b) {
                return;
            }
        }
    }

    /**
     * receiver worker
     */
    protected void workerRx() {
        if (debugger.secWebsockTraf) {
            logger.debug("rx started");
        }
        for (;;) {
            byte[] buf = new byte[2];
            if (lower.moreGet(buf, 0, buf.length) != buf.length) {
                return;
            }
            byte[] msk = null;
            if ((buf[1] & 0x80) != 0) {
                msk = new byte[4];
            }
            int len = buf[1] & 0x7f;
            int opc = buf[0] & 0xf;
            boolean fin = (buf[0] & 0x80) != 0;
            if (len == 126) {
                buf = new byte[2];
                if (lower.moreGet(buf, 0, buf.length) != buf.length) {
                    return;
                }
                len = bits.msbGetW(buf, 0);
            } else if (len == 127) {
                buf = new byte[8];
                if (lower.moreGet(buf, 0, buf.length) != buf.length) {
                    return;
                }
                len = bits.msbGetD(buf, 4);
            }
            if (msk != null) {
                if (lower.moreGet(msk, 0, msk.length) != msk.length) {
                    return;
                }
            }
            if (debugger.secWebsockTraf) {
                logger.debug("rx opc=" + opcode2string(opc) + " fin=" + fin + " msk=" + bits.byteDump(msk, 0, -1) + " len=" + len);
            }
            buf = new byte[len];
            if (lower.moreGet(buf, 0, buf.length) != buf.length) {
                return;
            }
            doMasking(buf, msk);
            switch (opc) {
                case opcCont:
                case opcBinary:
                case opcText:
                    if (userS.morePut(buf, 0, buf.length) != buf.length) {
                        return;
                    }
                    break;
                case opcClose:
                    return;
                case opcPing:
                    if (netTx(opcPong, getMasker(), buf, buf.length)) {
                        return;
                    }
                    break;
                case opcPong:
                    break;
            }
        }
    }

    /**
     * keepalive worker
     */
    protected void workerKeep() {
        if (debugger.secWebsockTraf) {
            logger.debug("keepalive started");
        }
        for (;;) {
            bits.sleep(30000);
            byte[] buf = ("send " + bits.randomW() + " back").getBytes();
            if (netTx(opcPing, getMasker(), buf, buf.length)) {
                return;
            }
        }
    }

    private void doMasking(byte[] buf, byte[] msk) {
        if (msk == null) {
            return;
        }
        for (int i = 0; i < buf.length; i++) {
            buf[i] ^= msk[i & 3];
        }
    }

    private byte[] getMasker() {
        if (!client) {
            return null;
        }
        byte[] msk = new byte[4];
        bits.msbPutD(msk, 0, bits.randomD());
        return msk;
    }

    private boolean sendClose(int stat, String text) {
        byte[] buf = text.getBytes();
        bits.msbPutW(buf, 0, stat);
        return netTx(opcClose, getMasker(), buf, buf.length);
    }

    private synchronized boolean netTx(int opc, byte[] msk, byte[] dat, int len) {
        if (debugger.secWebsockTraf) {
            logger.debug("tx opc=" + opcode2string(opc) + " msk=" + bits.byteDump(msk, 0, -1) + " len=" + len);
        }
        byte[] buf;
        if (len > 125) {
            buf = new byte[4];
            buf[1] = 126;
            bits.msbPutW(buf, 2, len);
        } else {
            buf = new byte[2];
            buf[1] = (byte) len;
        }
        buf[0] = (byte) (0x80 | opc);
        if (msk != null) {
            buf[1] |= (byte) 0x80;
        }
        if (lower.morePut(buf, 0, buf.length) != buf.length) {
            return true;
        }
        if (msk != null) {
            if (lower.morePut(msk, 0, msk.length) != msk.length) {
                return true;
            }
        }
        doMasking(dat, msk);
        if (lower.morePut(dat, 0, len) != len) {
            return true;
        }
        return false;
    }

}

class secWebsockRx implements Runnable {

    private final secWebsock lower;

    public secWebsockRx(secWebsock parent) {
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
        if (debugger.secWebsockTraf) {
            logger.debug("rx stopped");
        }
    }

}

class secWebsockTx implements Runnable {

    private final secWebsock lower;

    public secWebsockTx(secWebsock parent) {
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
        if (debugger.secWebsockTraf) {
            logger.debug("tx stopped");
        }
    }

}

class secWebsockKep implements Runnable {

    private final secWebsock lower;

    public secWebsockKep(secWebsock parent) {
        lower = parent;
        new Thread(this).start();
    }

    public void run() {
        try {
            lower.workerKeep();
        } catch (Exception e) {
            logger.traceback(e);
        }
        lower.workerStop();
        if (debugger.secWebsockTraf) {
            logger.debug("keepalive stopped");
        }
    }

}
