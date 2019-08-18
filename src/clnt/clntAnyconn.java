package clnt;

import addr.addrEmpty;
import addr.addrIP;
import addr.addrType;
import cfg.cfgAll;
import cfg.cfgIfc;
import ifc.ifcDn;
import ifc.ifcEther;
import ifc.ifcNull;
import ifc.ifcUp;
import pack.packAnyconn;
import pack.packHolder;
import pipe.pipeSide;
import serv.servGeneric;
import user.userTerminal;
import util.bits;
import util.counter;
import util.debugger;
import util.logger;
import util.state;
import util.uniResLoc;
import util.version;

/**
 * anyconnect client
 *
 * @author matecsaba
 */
public class clntAnyconn implements Runnable, ifcDn {

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

    private uniResLoc url;

    private String cookie1; // webvpncontext

    private String cookie2; // webvpn

    private String cookie3; // webvpnc

    private addrIP addr4;

    private addrIP addr6;

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
        if (ifcEther.stripEtherType(pck)) {
            cntr.drop(pck, counter.reasons.badProto);
            return;
        }
        if (debugger.clntAnyconnTraf) {
            logger.debug("tx " + pck.dump());
        }
        packAnyconn ps = new packAnyconn(pipe);
        ps.msgTyp = packAnyconn.typData;
        pck.putDefaults();
        ps.sendPack(pck);
    }

    private void clearState() {
        good = false;
        if (pipe != null) {
            pipe.setClose();
        }
        cookie1 = null;
        cookie2 = null;
        addr4 = null;
        addr6 = null;
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
        if (debugger.clntAnyconnTraf) {
            logger.debug("connecting " + trg);
        }
        pipe = proxy.doConnect(servGeneric.protoTcp, trg, url.getPort(0), "anyconn");
        if (pipe == null) {
            return true;
        }
        pipe.lineRx = pipeSide.modTyp.modeCRtryLF;
        pipe.lineTx = pipeSide.modTyp.modeCRLF;
        return false;
    }

    private void sendLine(String s) {
        if (debugger.clntAnyconnTraf) {
            logger.debug("tx: " + s);
        }
        pipe.linePut(s);
    }

    private String recvLn() {
        String s = pipe.lineGet(1);
        if (debugger.clntAnyconnTraf) {
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
            if (a.equals("location")) {
                url = uniResLoc.parseOne(s);
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
                if (a.equals("webvpncontext")) {
                    cookie1 = a + "=" + s;
                    continue;
                }
                if (a.equals("webvpn")) {
                    cookie2 = a + "=" + s;
                    continue;
                }
                if (a.equals("webvpnc")) {
                    cookie3 = a + "=" + s;
                    continue;
                }
            }
            if (a.equals("x-cstp-address")) {
                addr4 = new addrIP();
                addr4.fromString(s);
                continue;
            }
            if (a.equals("x-cstp-address-ip6")) {
                i = s.indexOf("/");
                if (i < 0) {
                    continue;
                }
                s = s.substring(0, i).trim();
                addr6 = new addrIP();
                addr6.fromString(s);
                continue;
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
        if (debugger.clntAnyconnTraf) {
            logger.debug("resolving " + url.dump());
        }
        trg = userTerminal.justResolv(url.server, proxy.prefer);
        if (trg == null) {
            return;
        }
        if (mkConn()) {
            return;
        }
        String cntx = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><config-auth client=\"vpn\" type=\"init\"><version who=\"vpn\">" + version.VerNam + "</version><device-id>" + version.getKernelName() + "</device-id><group-access>" + url.toURL(false, true) + "</group-access></config-auth>";
        sendLine("POST " + url.toURL(false, false) + " HTTP/1.1");
        sendLine("user-agent: " + version.usrAgnt);
        sendLine("host: " + url.server);
        sendLine("connection: keep-alive");
        sendLine("content-length: " + (cntx.length() + 2));
        sendLine("content-type: application/x-www-form-urlencoded");
        sendLine("");
        sendLine(cntx);
        recvHdr();
        pipe.setClose();
        if (mkConn()) {
            return;
        }
        cntx = "username=" + uniResLoc.percentEncode(username) + "&password=" + uniResLoc.percentEncode(password);
        sendLine("POST " + url.toURL(false, false) + " HTTP/1.1");
        sendLine("user-agent: " + version.usrAgnt);
        sendLine("host: " + url.server);
        sendLine("cookie: " + cookie1);
        sendLine("content-length: " + cntx.length());
        sendLine("content-Type: application/x-www-form-urlencoded");
        sendLine("accept: */*");
        sendLine("accept-encoding: identity");
        sendLine("connection: keep-alive");
        sendLine("x-transcend-version: 1");
        sendLine("x-support-http-auth: true");
        sendLine("x-pad: 0000000000000000000000000000000000");
        sendLine("");
        pipe.strPut(cntx);
        recvHdr();
        sendLine("CONNECT /CSCOSSLC/tunnel HTTP/1.1");
        sendLine("user-agent: " + version.usrAgnt);
        sendLine("host: " + url.server);
        sendLine("cookie: " + cookie2);
        sendLine("x-cstp-version: 1");
        sendLine("x-cstp-hostname: " + cfgAll.hostName);
        sendLine("x-cstp-base-mtu: 1500");
        sendLine("x-cstp-mtu: 1500");
        sendLine("x-cstp-address-type: ipv6,ipv4");
        sendLine("");
        recvHdr();
        if (debugger.clntAnyconnTraf) {
            logger.debug("addresses: ipv4=" + addr4 + " ipv6=" + addr6);
        }
        if (addr4 != null) {
            cfger.addr4changed(addr4.toIPv4(), cfger.mask4, null);
        }
        if (addr6 != null) {
            cfger.addr6changed(addr6.toIPv6(), cfger.mask6, null);
        }
        packHolder pckB = new packHolder(true, true);
        packAnyconn pckS = new packAnyconn(pipe);
        good = true;
        for (;;) {
            if (pckS.recvPack(pckB)) {
                break;
            }
            if (debugger.clntAnyconnTraf) {
                logger.debug("rx " + pckB.dump());
            }
            switch (pckS.msgTyp) {
                case packAnyconn.typData:
                    int i = ifcEther.guessEtherType(pckB);
                    if (i < 0) {
                        logger.info("got bad protocol from " + target);
                        break;
                    }
                    pckB.msbPutW(0, i); // ethertype
                    pckB.putSkip(2);
                    pckB.merge2beg();
                    upper.recvPack(pckB);
                    break;
                case packAnyconn.typDpdReq:
                    pckS.msgTyp = packAnyconn.typDpdRep;
                    pckS.sendPack(pckB);
                    break;
                case packAnyconn.typDpdRep:
                    break;
                case packAnyconn.typKeep:
                    break;
                case packAnyconn.typTerm:
                    return;
                case packAnyconn.typDisc:
                    return;
                default:
                    logger.info("unknown type: " + pckS.msgTyp);
                    break;
            }
        }
    }

}
