package net.freertr.clnt;

import net.freertr.addr.addrEmpty;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrType;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgIfc;
import net.freertr.ifc.ifcDn;
import net.freertr.ifc.ifcEther;
import net.freertr.ifc.ifcNull;
import net.freertr.ifc.ifcUp;
import net.freertr.pack.packAnyconn;
import net.freertr.pack.packHolder;
import net.freertr.pipe.pipeSide;
import net.freertr.user.userFormat;
import net.freertr.util.bits;
import net.freertr.util.counter;
import net.freertr.util.debugger;
import net.freertr.util.extMrkLng;
import net.freertr.util.logger;
import net.freertr.util.state;
import net.freertr.util.uniResLoc;
import net.freertr.util.version;

/**
 * anyconnect client
 *
 * @author matecsaba
 */
public class clntAnyconn implements Runnable, ifcDn {

    /**
     * create instance
     */
    public clntAnyconn() {
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
     * public key
     */
    public byte[] pubkey;

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

    private uniResLoc url;

    private String cookie1; // webvpncontext

    private String cookie2; // webvpn

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

    private void workDoer() {
        url = uniResLoc.parseOne(target);
        clntHttp cln = new clntHttp(null, proxy, pubkey, debugger.clntAnyconnTraf);
        if (cln.doConnect(url)) {
            return;
        }
        String cntx = extMrkLng.header + "<config-auth client=\"vpn\" type=\"init\"><version who=\"vpn\">" + version.VerNam + "</version><device-id>" + version.getKernelName() + "</device-id><group-access>" + url.toURL(false, true) + "</group-access></config-auth>";
        cln.sendLine("POST " + url.toURL(false, false) + " HTTP/1.1");
        cln.sendLine("user-agent: " + version.usrAgnt);
        cln.sendLine("host: " + url.server);
        cln.sendLine("connection: keep-alive");
        cln.sendLine("content-length: " + (cntx.length() + 2));
        cln.sendLine("content-type: application/x-www-form-urlencoded");
        cln.sendLine("");
        cln.sendLine(cntx);
        cln.doHeaders(url);
        cln.doBody();
        cln.cleanUp();
        int i = extMrkLng.findParam(cln.cookies, "|webvpncontext|");
        if (i < 0) {
            return;
        }
        cookie1 = cln.cookies.get(i).getNamVal();
        cln = new clntHttp(null, proxy, pubkey, debugger.clntAnyconnTraf);
        if (cln.doConnect(url)) {
            return;
        }
        cntx = "username=" + uniResLoc.percentEncode(username) + "&password=" + uniResLoc.percentEncode(password);
        cln.sendLine("POST " + url.toURL(false, false) + " HTTP/1.1");
        cln.sendLine("user-agent: " + version.usrAgnt);
        cln.sendLine("host: " + url.server);
        cln.sendLine("cookie: " + cookie1);
        cln.sendLine("content-length: " + cntx.length());
        cln.sendLine("content-Type: application/x-www-form-urlencoded");
        cln.sendLine("accept: */*");
        cln.sendLine("accept-encoding: identity");
        cln.sendLine("connection: keep-alive");
        cln.sendLine("x-transcend-version: 1");
        cln.sendLine("x-support-http-auth: true");
        cln.sendLine("x-pad: 0000000000000000000000000000000000");
        cln.sendLine("");
        cln.pipe.strPut(cntx);
        cln.doHeaders(url);
        cln.doBody();
        pipe = cln.pipe;
        i = extMrkLng.findParam(cln.cookies, "|webvpn|");
        if (i < 0) {
            return;
        }
        cookie2 = cln.cookies.get(i).getNamVal();
        cln.sendLine("CONNECT /CSCOSSLC/tunnel HTTP/1.1");
        cln.sendLine("user-agent: " + version.usrAgnt);
        cln.sendLine("host: " + url.server);
        cln.sendLine("cookie: " + cookie2);
        cln.sendLine("x-cstp-version: 1");
        cln.sendLine("x-cstp-hostname: " + cfgAll.hostName);
        cln.sendLine("x-cstp-base-mtu: 1500");
        cln.sendLine("x-cstp-mtu: 1500");
        cln.sendLine("x-cstp-address-type: ipv6,ipv4");
        cln.sendLine("");
        cln.doHeaders(url);
        i = extMrkLng.findParam(cln.headers, "|x-cstp-address|");
        if (i >= 0) {
            String s = cln.headers.get(i).value;
            addr4 = new addrIP();
            addr4.fromString(s);
        }
        i = extMrkLng.findParam(cln.headers, "|x-cstp-address-ip6|");
        if (i >= 0) {
            String s = cln.headers.get(i).value;
            i = s.indexOf("/");
            if (i >= 0) {
                s = s.substring(0, i).trim();
                addr6 = new addrIP();
                addr6.fromString(s);
            }
        }
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
                    i = ifcEther.guessEtherType(pckB);
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

    /**
     * get show
     *
     * @return state
     */
    public userFormat getShow() {
        userFormat res = new userFormat("|", "category|value");
        res.add("upper|" + upper);
        res.add("cntr|" + cntr);
        res.add("cookie1|" + cookie1);
        res.add("cookie2|" + cookie2);
        return res;
    }

}
