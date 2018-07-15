package prt;

import addr.addrIP;
import addr.addrIPv4;
import addr.addrIPv6;
import cfg.cfgVrf;
import ip.ipFwdIface;
import ip.ipIfcLoop;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;
import pipe.pipeLine;
import pipe.pipeSide;
import util.logger;

/**
 * local tcp server
 *
 * @author matecsaba
 */
public class prtLocTcp implements Runnable {

    private ServerSocket socket;

    private prtGen proto;

    private int port;

    /**
     * create new listener
     *
     * @param local os port
     * @param prt protocol to search
     * @param remote vrf port
     * @param bind binding address, empty string if not restricted
     * @throws Exception if something went wrong
     */
    public prtLocTcp(int local, prtGen prt, int remote, String bind) throws Exception {
        InetAddress addr = null;
        if (bind.length() > 0) {
            try {
                addr = InetAddress.getByName(bind);
            } catch (Exception e) {
                addr = null;
            }
        }
        socket = new ServerSocket(local, 32, addr);
        proto = prt;
        port = remote;
        new Thread(this).start();
    }

    /**
     * start server
     *
     * @param loc local port
     * @param vrf vrf to use
     * @param rem remote port
     * @param adr address to bind to, null if nothing
     * @return false on success, true on error
     */
    public static boolean startServer(int loc, cfgVrf vrf, int rem, String adr) {
        try {
            new prtLocTcp(loc, vrf.tcp4, rem, adr);
            return false;
        } catch (Exception e) {
            logger.traceback(e);
            return true;
        }
    }

    public void run() {
        try {
            for (;;) {
                Socket clnt = socket.accept();
                if (doAccept(clnt)) {
                    clnt.close();
                }
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

    /**
     * convert java address
     *
     * @param adr java address
     * @return converted address
     */
    public static addrIP java2addr(InetAddress adr) {
        byte[] buf = adr.getAddress();
        addrIP res = new addrIP();
        switch (buf.length) {
            case addrIPv4.size:
                addrIPv4 a4 = new addrIPv4();
                a4.fromBuf(buf, 0);
                res.fromIPv4addr(a4);
                break;
            case addrIPv6.size:
                addrIPv6 a6 = new addrIPv6();
                a6.fromBuf(buf, 0);
                res.fromIPv6addr(a6);
                break;
        }
        return res;
    }

    private boolean doAccept(Socket clnt) throws Exception {
        addrIP srcA = java2addr(clnt.getInetAddress());
        int srcP = clnt.getPort();
        prtGenServ srv = proto.srvrs.get(0, srcA, 0, port, srcP, true);
        if (srv == null) {
            return true;
        }
        if (!srv.stream) {
            return true;
        }
        prtGenConn conn = new prtGenConn(new prtTcp(), srv.serverP, srv.serverS, srv.sample, true, new ipFwdIface(-1, new ipIfcLoop()), port, srcA, srcP, "local:" + srv.name, null, -1);
        pipeLine pipeHandler = pipeLine.doClone(srv.sample, false);
        pipeSide pipeNetwork = pipeHandler.getSide();
        pipeSide pipeClient = pipeHandler.getSide();
        pipeClient.setReady();
        pipeNetwork.setReady();
        if (srv.serverS.streamAccept(pipeClient, conn)) {
            return true;
        }
        new prtLocTcpRx(clnt, pipeNetwork);
        new prtLocTcpTx(clnt, pipeNetwork);
        return false;
    }

    /**
     * do one session
     *
     * @param pipe pipe to use
     * @param sock socket to use
     */
    public static void doSession(pipeSide pipe, Socket sock) {
        pipe.setReady();
        new prtLocTcpRx(sock, pipe);
        new prtLocTcpTx(sock, pipe);
    }

}

class prtLocTcpRx implements Runnable {

    public Socket sock;

    public pipeSide pipe;

    public prtLocTcpRx(Socket clnt, pipeSide conn) {
        sock = clnt;
        pipe = conn;
        new Thread(this).start();
    }

    public void run() {
        try {
            InputStream strm = sock.getInputStream();
            for (;;) {
                int i = strm.available();
                if (i < 1) {
                    i = 1;
                }
                byte[] buf = new byte[i];
                if (strm.read(buf) != buf.length) {
                    break;
                }
                if (pipe.morePut(buf, 0, buf.length) != buf.length) {
                    break;
                }
            }
        } catch (Exception e) {
        }
        try {
            pipe.setClose();
            sock.close();
        } catch (Exception e) {
        }
    }

}

class prtLocTcpTx implements Runnable {

    public Socket sock;

    public pipeSide pipe;

    public prtLocTcpTx(Socket clnt, pipeSide conn) {
        sock = clnt;
        pipe = conn;
        new Thread(this).start();
    }

    public void run() {
        try {
            OutputStream strm = sock.getOutputStream();
            for (;;) {
                int i = pipe.ready2rx();
                if (i < 1) {
                    i = 1;
                }
                byte[] buf = new byte[i];
                if (pipe.blockingGet(buf, 0, buf.length) != buf.length) {
                    break;
                }
                strm.write(buf);
            }
        } catch (Exception e) {
        }
        try {
            pipe.setClose();
            sock.close();
        } catch (Exception e) {
        }
    }

}
