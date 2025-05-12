package org.freertr.clnt;

import org.freertr.addr.addrEmpty;
import org.freertr.addr.addrType;
import org.freertr.cfg.cfgIfc;
import org.freertr.cfg.cfgInit;
import org.freertr.ifc.ifcDn;
import org.freertr.ifc.ifcNull;
import org.freertr.ifc.ifcPpp;
import org.freertr.ifc.ifcUp;
import org.freertr.pack.packForti;
import org.freertr.pack.packHolder;
import org.freertr.pipe.pipeSide;
import org.freertr.user.userFormat;
import org.freertr.util.bits;
import org.freertr.util.counter;
import org.freertr.util.debugger;
import org.freertr.enc.encXml;
import org.freertr.enc.encUrl;
import org.freertr.util.logger;
import org.freertr.util.state;

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

    private String cookie;

    private encUrl url;

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

    private void workDoer() {
        url = encUrl.parseOne(target);
        clntHttp cln = new clntHttp(null, proxy, pubkey, debugger.clntFortiTraf);
        if (cln.doConnect(url)) {
            return;
        }
        url.addParam("username", username);
        url.addParam("credential", password);
        url.filPath = "remote/";
        url.filName = "logincheck";
        cln.sendLine("POST " + url.toURL(false, false, true, true) + " HTTP/1.1");
        cln.sendLine("User-Agent: " + cfgInit.versionAgent);
        cln.sendLine("Host: " + url.server);
        cln.sendLine("Connection: keep-alive");
        cln.sendLine("Content-Length: 0");
        cln.sendLine("Content-Type: application/x-www-form-urlencoded");
        cln.sendLine("");
        cln.doHeaders(url);
        cln.doBody();
        cln.cleanUp();
        int i = encXml.findParam(cln.cookies, "|svpncookie|");
        if (i < 0) {
            return;
        }
        cookie = cln.cookies.get(i).getNamVal();
        cln = new clntHttp(null, proxy, pubkey, debugger.clntFortiTraf);
        if (cln.doConnect(url)) {
            return;
        }
        cln.sendLine("GET /remote/fortisslvpn_xml HTTP/1.1");
        cln.sendLine("User-Agent: " + cfgInit.versionAgent);
        cln.sendLine("Host: " + url.server);
        cln.sendLine("Cookie: " + cookie);
        cln.sendLine("");
        cln.doHeaders(url);
        cln.doBody();
        cln.sendLine("GET /remote/sslvpn-tunnel HTTP/1.1");
        cln.sendLine("Host: sslvpn");
        cln.sendLine("Cookie: " + cookie);
        cln.sendLine("");
        pipe = cln.pipe;
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
            pckB.msbPutW(0, ifcPpp.preamble);
            pckB.putSkip(2);
            pckB.merge2beg();
            upper.recvPack(pckB);
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
        res.add("cookie|" + cookie);
        return res;
    }

}
