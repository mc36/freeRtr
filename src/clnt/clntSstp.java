package clnt;

import addr.addrEmpty;
import addr.addrIP;
import addr.addrType;
import cfg.cfgAll;
import ifc.ifcDn;
import ifc.ifcNull;
import ifc.ifcUp;
import pack.packHolder;
import pack.packSstp;
import pipe.pipeSide;
import serv.servGeneric;
import user.userTerminal;
import util.bits;
import util.counter;
import util.debugger;
import util.logger;
import util.state;
import util.uniResLoc;
import util.verCore;
import util.version;

/**
 * secure socket tunneling protocol client
 *
 * @author matecsaba
 */
public class clntSstp implements Runnable, ifcDn {

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

    private void sendAuth(uniResLoc url) {
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
        uniResLoc url = uniResLoc.parseOne(target + "sra_{BA195980-CD49-458b-9E23-C84EE0ADCD75}/");
        if (debugger.clntSstpTraf) {
            logger.debug("resolving " + url.dump());
        }
        addrIP trg = userTerminal.justResolv(url.server, proxy.prefer);
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
        pipe.lineRx = pipeSide.modTyp.modeCRtryLF;
        pipe.lineTx = pipeSide.modTyp.modeCRLF;
        sendLine("SSTP_DUPLEX_POST " + url.toURL(false, true) + " HTTP/1.1");
        sendLine("user-agent: " + version.usrAgnt);
        sendLine("content-Length: 18446744073709551615");
        sendLine("host: " + url.server);
        sendLine("sstpCorrelationID: /os/" + verCore.name + "/{" + cfgAll.hostName + "_" + unique + "}");
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
