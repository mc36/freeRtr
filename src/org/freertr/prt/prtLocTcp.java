package org.freertr.prt;

import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.net.Socket;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv4;
import org.freertr.addr.addrIPv6;
import org.freertr.cfg.cfgVrf;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipFwdIface;
import org.freertr.ip.ipFwdTab;
import org.freertr.ip.ipIfcLoop;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.util.logger;

/**
 * local tcp server
 *
 * @author matecsaba
 */
public class prtLocTcp implements Runnable {

    private ServerSocket socket;

    private cfgVrf vrf;

    private int port;

    private addrIP source;

    /**
     * create new listener
     *
     * @param local os port
     * @param prt protocol to search
     * @param remote vrf port
     * @param bind binding address, empty string if not restricted
     * @param fake address to fake on connection
     * @throws Exception if something went wrong
     */
    public prtLocTcp(int local, cfgVrf prt, int remote, String bind, String fake) throws Exception {
        if (fake.length() < 2) {
            source = null;
        } else {
            source = new addrIP();
            source.fromString(fake);
        }
        InetAddress addr = null;
        InetSocketAddress sadr = null;
        if (bind.length() < 2) {
            sadr = new InetSocketAddress(local);
        } else {
            addr = InetAddress.getByName(bind);
            sadr = new InetSocketAddress(addr, local);
        }
        socket = new ServerSocket();
        socket.setReuseAddress(true);
        socket.bind(sadr);
        vrf = prt;
        port = remote;
        logger.startThread(this);
    }

    /**
     * start server
     *
     * @param loc local port
     * @param vrf vrf to use
     * @param rem remote port
     * @param bind address to bind to, null if nothing
     * @param fake address to fake on connection
     * @return false on success, true on error
     */
    public static boolean startServer(int loc, cfgVrf vrf, int rem, String bind, String fake) {
        try {
            new prtLocTcp(loc, vrf, rem, bind, fake);
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
        addrIP srcA;
        if (source != null) {
            srcA = source.copyBytes();
        } else {
            srcA = java2addr(clnt.getInetAddress());
        }
        int srcP = clnt.getPort();
        prtTcp tcp = vrf.getTcp(srcA);
        ipFwd fwd = vrf.getFwd(srcA);
        ipFwdIface ifc = ipFwdTab.findSendingIface(fwd, srcA);
        prtGenServ srv = tcp.findAccepter(ifc, srcA, port, srcP);
        if (srv == null) {
            logger.warn("server not found on port " + port + " from " + srcA + " " + srcP);
            return true;
        }
        if (!srv.stream) {
            logger.warn("not stream server on port " + port);
            return true;
        }
        ipIfcLoop ipi = new ipIfcLoop();
        ipi.setIPv4addr(new addrIPv4(), addrIPv4.size * 8);
        prtGenConn conn = new prtGenConn(new prtTcp(), srv.serverP, srv.serverS, srv.sample, true, ipi.getFwdIface(), port, srcA, srcP, "local:" + srv.name, -1, null, -1, -1);
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
        logger.startThread(this);
    }

    public void run() {
        try {
            InputStream strm = sock.getInputStream();
            for (;;) {
                int i = strm.available();
                if (i < 1) {
                    i = 1;
                }
                if (i > 1024) {
                    i = 1024;
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
        logger.startThread(this);
    }

    public void run() {
        try {
            OutputStream strm = sock.getOutputStream();
            for (;;) {
                int i = pipe.ready2rx();
                if (i < 1) {
                    i = 1;
                }
                if (i > 1024) {
                    i = 1024;
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
