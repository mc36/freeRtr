package net.freertr.clnt;

import net.freertr.addr.addrEmpty;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrType;
import net.freertr.cfg.cfgIfc;
import net.freertr.ifc.ifcDn;
import net.freertr.ifc.ifcNull;
import net.freertr.ifc.ifcUp;
import net.freertr.pack.packForti;
import net.freertr.pack.packHolder;
import net.freertr.pipe.pipeSide;
import net.freertr.serv.servGeneric;
import net.freertr.user.userTerminal;
import net.freertr.util.bits;
import net.freertr.util.counter;
import net.freertr.util.debugger;
import net.freertr.util.logger;
import net.freertr.util.state;
import net.freertr.util.uniResLoc;
import net.freertr.util.version;

/**
 * forti client
 *
 * @author matecsaba
 */
public class clntForti implements Runnable, ifcDn {

    /**
     * create instance
     */
    public clntForti() {
    }

    /**
     * upper layer
     */
    public ifcUp upper = new ifcNull();

    /**
     * target of tunnel
     */
    public String target = null;

    /**
     * proxy profile
     */
    public clntProxy proxy;

    /**
     * username to use
     */
    public String username = null;

    /**
     * password to use
     */
    public String password = null;

    /**
     * config class
     */
    public cfgIfc cfger;

    /**
     * counter
     */
    public counter cntr = new counter();

    private boolean working = true;

    private pipeSide pipe;

    private boolean good;

    private addrIP trg;

    private String cookie;

    private uniResLoc url;

    /**
     * get hw address
     *
     * @return address
     */
    public addrType getHwAddr() {
        return new addrEmpty();
    }

    /**
     * set filter more
     *
     * @param promisc promiscous mode
     */
    public void setFilter(boolean promisc) {
    }

    /**
     * get state
     *
     * @return state
     */
    public state.states getState() {
        return state.states.up;
    }

    /**
     * close interface
     */
    public void closeDn() {
        clearState();
    }

    /**
     * flap interface
     */
    public void flapped() {
        clearState();
    }

    /**
     * set upper level
     *
     * @param server upper
     */
    public void setUpper(ifcUp server) {
        upper = server;
        upper.setParent(this);
    }

    /**
     * get counter
     *
     * @return counter
     */
    public counter getCounter() {
        return cntr;
    }

    /**
     * get mtu size
     *
     * @return mtu
     */
    public int getMTUsize() {
        return 1504;
    }

    /**
     * get bandwidth
     *
     * @return bandwidth
     */
    public long getBandwidth() {
        return 8000000;
    }

    /**
     * send packet
     *
     * @param pck packet
     */
    public void sendPack(packHolder pck) {
        if (!good) {
            return;
        }
        if (pipe == null) {
            return;
        }
        cntr.tx(pck);
        pck.getSkip(2);
        pck.putDefaults();
        if (debugger.clntFortiTraf) {
            logger.debug("tx " + pck.dump());
        }
        packForti ps = new packForti(pipe);
        ps.sendPack(pck);
    }

    private void clearState() {
        good = false;
        if (pipe != null) {
            pipe.setClose();
        }
        cookie = null;
    }

    public void run() {
        for (;;) {
            if (!working) {
                break;
            }
            try {
                clearState();
                workDoer();
            } catch (Exception e) {
                logger.traceback(e);
            }
            clearState();
            bits.sleep(1000);
        }
    }

    /**
     * start connection
     */
    public void workStart() {
        new Thread(this).start();
    }

    /**
     * stop connection
     */
    public void workStop() {
        working = false;
        clearState();
    }

    private boolean mkConn() {
        if (debugger.clntFortiTraf) {
            logger.debug("connecting " + trg);
        }
        pipe = proxy.doConnect(servGeneric.protoTcp, trg, url.getPort(0), "forti");
        if (pipe == null) {
            return true;
        }
        pipe.setTime(120000);
        pipe.lineRx = pipeSide.modTyp.modeCRtryLF;
        pipe.lineTx = pipeSide.modTyp.modeCRLF;
        return false;
    }

    private void sendLine(String s) {
        if (debugger.clntFortiTraf) {
            logger.debug("tx: " + s);
        }
        pipe.linePut(s);
    }

    private String recvLn() {
        String s = pipe.lineGet(1);
        if (debugger.clntFortiTraf) {
            logger.debug("rx: " + s);
        }
        return s;
    }

    private byte[] recvHdr() {
        int cntLen = 0;
        for (;;) {
            String s = recvLn();
            if (s.length() < 1) {
                break;
            }
            String a;
            int i = s.indexOf(":");
            if (i < 0) {
                a = s.trim().toLowerCase();
                s = "";
            } else {
                a = s.substring(0, i).trim().toLowerCase();
                s = s.substring(i + 1, s.length()).trim();
            }
            if (a.equals("content-length")) {
                cntLen = bits.str2num(s);
                continue;
            }
            if (a.equals("set-cookie")) {
                i = s.indexOf(";");
                if (i < 0) {
                    continue;
                }
                s = s.substring(0, i).trim();
                i = s.indexOf("=");
                if (i < 0) {
                    continue;
                }
                a = s.substring(0, i).trim().toLowerCase();
                s = s.substring(i + 1, s.length()).trim();
                if (a.equals("svpncookie")) {
                    cookie = a + "=" + s;
                    continue;
                }
            }
        }
        if (cntLen < 1) {
            return new byte[0];
        }
        byte[] buf = new byte[cntLen];
        pipe.moreGet(buf, 0, buf.length);
        return buf;
    }

    private void workDoer() {
        url = uniResLoc.parseOne(target);
        if (debugger.clntFortiTraf) {
            logger.debug("resolving " + url.dump());
        }
        trg = userTerminal.justResolv(url.server, proxy.prefer);
        if (trg == null) {
            return;
        }
        if (mkConn()) {
            return;
        }
        url.addParam("username", username);
        url.addParam("credential", password);
        url.filPath = "remote/";
        url.filName = "logincheck";
        sendLine("POST " + url.toURL(false, true) + " HTTP/1.1");
        sendLine("user-agent: " + version.usrAgnt);
        sendLine("host: " + url.server);
        sendLine("connection: keep-alive");
        sendLine("content-length: 0");
        sendLine("content-type: application/x-www-form-urlencoded");
        sendLine("");
        recvHdr();
        pipe.setClose();
        if (mkConn()) {
            return;
        }
        sendLine("GET /remote/fortisslvpn_xml HTTP/1.1");
        sendLine("user-agent: " + version.usrAgnt);
        sendLine("host: " + url.server);
        sendLine("cookie: " + cookie);
        sendLine("");
        recvHdr();
        sendLine("GET /remote/sslvpn-tunnel HTTP/1.1");
        sendLine("host: sslvpn");
        sendLine("cookie: " + cookie);
        sendLine("");
        packHolder pckB = new packHolder(true, true);
        packForti pckS = new packForti(pipe);
        good = true;
        for (;;) {
            if (pckS.recvPack(pckB)) {
                break;
            }
            if (debugger.clntFortiTraf) {
                logger.debug("rx " + pckB.dump());
            }
            pckB.msbPutW(0, 0xff03);
            pckB.putSkip(2);
            pckB.merge2beg();
            upper.recvPack(pckB);
        }
    }

}
