package org.freertr.clnt;

import java.util.List;
import org.freertr.addr.addrEmpty;
import org.freertr.addr.addrIPv4;
import org.freertr.addr.addrIPv6;
import org.freertr.addr.addrType;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgIfc;
import org.freertr.cfg.cfgInit;
import org.freertr.ifc.ifcDn;
import org.freertr.ifc.ifcEther;
import org.freertr.ifc.ifcNull;
import org.freertr.ifc.ifcUp;
import org.freertr.pack.packHolder;
import org.freertr.pack.packPulse;
import org.freertr.pipe.pipeSide;
import org.freertr.user.userFormat;
import org.freertr.util.bits;
import org.freertr.util.counter;
import org.freertr.util.debugger;
import org.freertr.enc.encXml;
import org.freertr.enc.encXmlEntry;
import org.freertr.enc.encUrl;
import org.freertr.util.logger;
import org.freertr.util.state;
import org.freertr.enc.encTlv;

/**
 * pulse client
 *
 * @author matecsaba
 */
public class clntPulse implements Runnable, ifcDn {

    /**
     * create instance
     */
    public clntPulse() {
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

    private addrIPv4 addr4;

    private addrIPv6 addr6;

    private packPulse pckTx;

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
        if (pckTx == null) {
            return;
        }
        cntr.tx(pck);
        pck.getSkip(2);
        pck.putDefaults();
        if (debugger.clntPulseTraf) {
            logger.debug("tx " + pck.dump());
        }
        pckTx.sendPack(pck);
    }

    private void clearState() {
        good = false;
        if (pipe != null) {
            pipe.setClose();
        }
        pckTx = null;
        cookie = null;
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

    private void parseAvps(packHolder pckBin) {
        for (;;) {
            encTlv tlv = packPulse.getAvp(pckBin);
            if (tlv == null) {
                break;
            }
            if (debugger.clntPulseTraf) {
                logger.debug("tlv: " + tlv.dump());
            }
        }
    }

    private void parseAttrs(packHolder pck, int ofs, int len) {
        if (pck.msbGetD(ofs + 4) != 0x03000000) {
            return;
        }
        ofs += 8;
        len -= 8;
        for (;;) {
            if (len < 1) {
                break;
            }
            int typ = pck.msbGetW(ofs + 0);
            int siz = pck.msbGetW(ofs + 2);
            ofs += 4;
            len -= 4;
            if (debugger.clntPulseTraf) {
                logger.debug("attr typ=" + typ + " len=" + siz);
            }
            switch (typ) {
                case 1:
                    addr4 = new addrIPv4();
                    pck.getAddr(addr4, ofs);
                    break;
                case 8:
                    addr6 = new addrIPv6();
                    pck.getAddr(addr6, ofs);
                    break;
            }
            ofs += siz;
            len -= siz;
        }
    }

    private void workDoer() {
        url = encUrl.parseOne(target);
        clntHttp cln = new clntHttp(null, proxy, pubkey, debugger.clntPulseTraf);
        if (cln.doConnect(url)) {
            return;
        }
        cln.sendLine("GET /" + url.toPathName() + " HTTP/1.1");
        cln.sendLine("Connection: keep-alive");
        cln.sendLine("User-Agent: " + cfgInit.versionAgent);
        cln.sendLine("Host: " + url.server);
        cln.sendLine("");
        cln.doHeaders(url);
        cln.doBody();
        cln.cleanUp();
        cln = new clntHttp(null, proxy, pubkey, debugger.clntPulseTraf);
        if (cln.doConnect(url)) {
            return;
        }
        cln.sendLine("GET /" + url.toPathName() + " HTTP/1.1");
        cln.sendLine("User-Agent: " + cfgInit.versionAgent);
        cln.sendLine("Host: " + url.server);
        cln.sendLine("");
        cln.doHeaders(url);
        List<Byte> buf = cln.doBody();
        cln.cleanUp();
        if (buf == null) {
            return;
        }
        encXml xml = new encXml();
        xml.setup2html();
        if (xml.fromString(buf)) {
            return;
        }
        String realm = null;
        for (int o = 0; o < xml.data.size(); o++) {
            encXmlEntry ntry = xml.data.get(o);
            if (!ntry.name.endsWith("/form/input")) {
                continue;
            }
            List<encXmlEntry> lst = encXml.decodeParams(ntry.param);
            int i = encXml.findParam(lst, "|name|");
            if (i < 0) {
                continue;
            }
            if (!lst.get(i).data.trim().toLowerCase().equals("realm")) {
                continue;
            }
            i = encXml.findParam(lst, "|value|");
            if (i < 0) {
                continue;
            }
            realm = lst.get(i).data.trim();
        }
        if (realm == null) {
            return;
        }
        cln = new clntHttp(null, proxy, pubkey, debugger.clntPulseTraf);
        if (cln.doConnect(url)) {
            return;
        }
        url.filName = "login";
        url.filExt = ".cgi";
        String s = "tz_offset=60&username=" + encUrl.percentEncode(username) + "&password=" + encUrl.percentEncode(password) + "&realm=" + encUrl.percentEncode(realm) + "&btnSubmit=Sign+In";
        cln.sendLine("POST /" + url.toPathName() + " HTTP/1.1");
        cln.sendLine("User-Agent: " + cfgInit.versionAgent);
        cln.sendLine("Host: " + url.server);
        cln.sendLine("Content-Length: " + s.length());
        cln.sendLine("Content-Type: application/x-www-form-urlencoded");
        cln.sendLine("");
        cln.pipe.strPut(s);
        cln.doHeaders(url);
        cln.doBody();
        cln.cleanUp();
        int i = encXml.findParam(cln.cookies, "|dsid|");
        if (i < 0) {
            return;
        }
        cookie = cln.cookies.get(i).data;
        cln = new clntHttp(null, proxy, pubkey, debugger.clntPulseTraf);
        if (cln.doConnect(url)) {
            return;
        }
        url = encUrl.parseOne(target);
        cln.sendLine("GET /" + url.toPathName() + " HTTP/1.1");
        cln.sendLine("User-Agent: " + cfgInit.versionAgent);
        cln.sendLine("Host: " + url.server);
        cln.sendLine("Content-Length: 0");
        cln.sendLine("content-Type: EAP");
        cln.sendLine("Upgrade: IF-T/TLS 1.0");
        cln.sendLine("");
        cln.doHeaders(url);
        pipe = cln.pipe;
        pckTx = new packPulse(pipe);
        packPulse pckRx = new packPulse(pipe);
        packHolder pckBin = new packHolder(true, true);
        pckBin.msbPutD(0, 0x010202);
        pckBin.putSkip(4);
        pckBin.merge2beg();
        pckTx.type = 0x01; // version negotiation
        pckTx.vendor = packPulse.vendTcg;
        pckTx.sendPack(pckBin);
        if (debugger.clntPulseTraf) {
            logger.debug("tx: " + pckBin.dump());
        }
        if (pckRx.recvPack(pckBin)) {
            return;
        }
        if (debugger.clntPulseTraf) {
            logger.debug("rx: " + pckBin.dump());
        }
        pckBin.clear();
        s = "clientHostName=" + cfgAll.hostName;
        i = s.length() + 1;
        pckBin.putAsciiZ(0, i, s, 0);
        pckBin.putSkip(i);
        pckBin.merge2end();
        pckTx.type = 0x88; // client info
        pckTx.vendor = packPulse.vendJunos;
        pckTx.sendPack(pckBin);
        if (debugger.clntPulseTraf) {
            logger.debug("tx: " + pckBin.dump());
        }
        if (pckRx.recvAuth(pckBin)) {
            return;
        }
        if (debugger.clntPulseTraf) {
            logger.debug("rx: " + pckBin.dump());
        }
        pckBin.clear();
        s = "anonymous";
        i = s.length();
        pckBin.putAsciiZ(0, i, s, 0);
        pckBin.putSkip(i);
        pckBin.merge2end();
        packPulse.putEap(pckBin, 2, 1, 1, 0);
        pckTx.type = 0x06; // auth req
        pckTx.vendor = packPulse.vendTcg;
        pckTx.sendAuth(pckBin);
        if (debugger.clntPulseTraf) {
            logger.debug("tx: " + pckBin.dump());
        }
        if (pckRx.recvAuth(pckBin)) {
            return;
        }
        if (debugger.clntPulseTraf) {
            logger.debug("rx: " + pckBin.dump());
        }
        packPulse.getEap(pckBin);
        parseAvps(pckBin);
        pckBin.clear();
        packPulse.putAvp(pckBin, 0xd70, cfgInit.versionName.getBytes());
        packPulse.putAvp(pckBin, 0xd53, cookie.getBytes());
        packPulse.putEap(pckBin, 2, 2, 0xfe, 1);
        pckTx.type = 0x06; // auth req
        pckTx.vendor = packPulse.vendTcg;
        pckTx.sendAuth(pckBin);
        if (debugger.clntPulseTraf) {
            logger.debug("tx: " + pckBin.dump());
        }
        if (pckRx.recvAuth(pckBin)) {
            return;
        }
        if (debugger.clntPulseTraf) {
            logger.debug("rx: " + pckBin.dump());
        }
        packPulse.getEap(pckBin);
        parseAvps(pckBin);
        pckBin.clear();
        packPulse.putEap(pckBin, 2, 3, 0xfe, 1);
        pckTx.type = 0x06; // auth req
        pckTx.vendor = packPulse.vendTcg;
        pckTx.sendAuth(pckBin);
        if (debugger.clntPulseTraf) {
            logger.debug("tx: " + pckBin.dump());
        }
        if (pckRx.recvAuth(pckBin)) {
            return;
        }
        if (debugger.clntPulseTraf) {
            logger.debug("rx: " + pckBin.dump());
        }
        packPulse.getEap(pckBin);
        parseAvps(pckBin);
        for (;;) {
            if (pckRx.recvPack(pckBin)) {
                return;
            }
            if (debugger.clntPulseTraf) {
                logger.debug("rx: " + pckBin.dump());
            }
            if (pckRx.type == 0x8f) {
                break;
            }
            if (pckRx.type != 0x01) {
                continue;
            }
            if (pckBin.msbGetD(0x10) != 0x2c20f000) {
                continue;
            }
            int len;
            int ofs = 0x1c;
            if (pckBin.getByte(0x10) == 0x2e) {
                len = pckBin.msbGetW(0x1e);
                parseAttrs(pckBin, 0x1c, len);
                ofs += len;
            }
            len = pckBin.msbGetW(ofs + 2);
            ofs += len;
            parseAttrs(pckBin, ofs, pckBin.dataSize() - ofs);
        }
        if (debugger.clntPulseTraf) {
            logger.debug("addrs=" + addr4 + " " + addr6);
        }
        if (addr4 != null) {
            cfger.addr4changed(addr4, cfger.mask4, null);
        }
        if (addr6 != null) {
            cfger.addr6changed(addr6, cfger.mask6, null);
        }
        pckTx.type = 4; // data
        pckTx.vendor = packPulse.vendJunos;
        good = true;
        for (;;) {
            if (pckRx.recvPack(pckBin)) {
                break;
            }
            if (debugger.clntPulseTraf) {
                logger.debug("rx: " + pckBin.dump());
            }
            if (pckRx.type != 4) {
                continue;
            }
            i = ifcEther.guessEtherType(pckBin);
            if (i < 0) {
                logger.info("got bad protocol from " + target);
                break;
            }
            pckBin.msbPutW(0, i); // ethertype
            pckBin.putSkip(2);
            pckBin.merge2beg();
            upper.recvPack(pckBin);
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
