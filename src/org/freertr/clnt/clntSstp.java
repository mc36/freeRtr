package org.freertr.clnt;

import org.freertr.addr.addrEmpty;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrType;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgInit;
import org.freertr.ifc.ifcDn;
import org.freertr.ifc.ifcNull;
import org.freertr.ifc.ifcUp;
import org.freertr.pack.packHolder;
import org.freertr.pack.packSstp;
import org.freertr.pipe.pipeSide;
import org.freertr.sec.secClient;
import org.freertr.serv.servGeneric;
import org.freertr.enc.encUrl;
import org.freertr.util.bits;
import org.freertr.util.counter;
import org.freertr.util.debugger;
import org.freertr.util.logger;
import org.freertr.util.state;
import org.freertr.util.version;

/**
 * secure socket tunneling protocol client
 *
 * @author matecsaba
 */
public class clntSstp implements Runnable, ifcDn {

    /**
     * create instance
     */
    public clntSstp() {
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
     * unique id
     */
    public String unique;

    /**
     * client pubkey
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
     * counter
     */
    public counter cntr = new counter();

    private boolean working = true;

    private pipeSide pipe;

    private boolean good;

    /**
     * get hw address
     *
     * @return hw address
     */
    public addrType getHwAddr() {
        return new addrEmpty();
    }

    /**
     * set filter
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
     * set upper layer
     *
     * @param server upper layer
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
     * @return mtu size
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
        if (debugger.clntSstpTraf) {
            logger.debug("tx " + pck.dump());
        }
        packSstp ps = new packSstp(pipe);
        pck.putDefaults();
        ps.sendData(pck);
    }

    private void clearState() {
        good = false;
        if (pipe != null) {
            pipe.setClose();
        }
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

    private void sendLine(String s) {
        if (debugger.clntSstpTraf) {
            logger.debug("tx: " + s);
        }
        pipe.linePut(s);
    }

    private void sendAuth(encUrl url) {
        String s = clntHttp.getAuthor(url.username, url.password);
        if (s == null) {
            return;
        }
        sendLine(s);
    }

    private String recvLn() {
        String s = pipe.lineGet(1);
        if (debugger.clntSstpTraf) {
            logger.debug("rx: " + s);
        }
        return s;
    }

    private void workDoer() {
        encUrl url = encUrl.parseOne(target + "sra_{BA195980-CD49-458b-9E23-C84EE0ADCD75}/");
        if (debugger.clntSstpTraf) {
            logger.debug("resolving " + url.toURL(true, false, false, true));
        }
        addrIP trg = clntDns.justResolv(url.server, proxy.prefer);
        if (trg == null) {
            return;
        }
        if (debugger.clntSstpTraf) {
            logger.debug("connecting " + trg);
        }
        pipe = proxy.doConnect(servGeneric.protoTcp, trg, url.getPort(0), "sstp");
        if (pipe == null) {
            return;
        }
        pipe = secClient.openSec(pipe, url.getSecurity(), pubkey, url.username, url.password);
        if (pipe == null) {
            return;
        }
        pipe.setTime(120000);
        pipe.lineRx = pipeSide.modTyp.modeCRtryLF;
        pipe.lineTx = pipeSide.modTyp.modeCRLF;
        sendLine("SSTP_DUPLEX_POST " + url.toURL(false, false, true, true) + " HTTP/1.1");
        sendLine("User-Agent: " + cfgInit.versionAgent);
        sendLine("Content-Length: 18446744073709551615");
        sendLine("Host: " + url.server);
        sendLine("sstpCorrelationID: /os/" + version.name + "/{" + cfgAll.hostName + "_" + unique + "}");
        sendAuth(url);
        sendLine("");
        for (;;) {
            String s = recvLn();
            if (s.length() < 1) {
                break;
            }
        }
        if (pipe.isClosed() != 0) {
            return;
        }
        packSstp pckS = new packSstp(pipe);
        pckS.fillConnReq();
        packHolder pckB = new packHolder(true, true);
        pckS.createConnReq(pckB);
        pckS.sendCtrl(pckB);
        if (debugger.clntSstpTraf) {
            logger.debug("tx " + pckS.dump());
        }
        pckB = pckS.recvPack();
        if (pckB == null) {
            return;
        }
        if (pckS.parseCtrl(pckB)) {
            return;
        }
        if (debugger.clntSstpTraf) {
            logger.debug("rx " + pckS.dump());
        }
        if (pckS.parseConnAck()) {
            return;
        }
        good = true;
        for (;;) {
            pckB = pckS.recvPack();
            if (pckB == null) {
                break;
            }
            if (debugger.clntSstpTraf) {
                logger.debug("rx " + pckB.dump());
            }
            upper.recvPack(pckB);
        }
    }

}
