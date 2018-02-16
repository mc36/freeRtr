package rtr;

import addr.addrIP;
import addr.addrPrefix;
import clnt.clntProxy;
import java.util.Comparator;
import java.util.List;
import pack.packRpki;
import pipe.pipeSide;
import serv.servGeneric;
import tab.tabRoute;
import tab.tabRouteEntry;
import user.userTerminal;
import util.bits;
import util.debugger;
import util.logger;

/**
 * resource public key infrastructure
 *
 * @author matecsaba
 */
public class rtrBgpRpki implements Comparator<rtrBgpRpki>, Runnable {

    private pipeSide pipe;

    private boolean need2run;

    private int serial;

    private int session;

    private rtrBgp lower;

    /**
     * name of monitor
     */
    public String rpkiName;

    /**
     * proxy to use
     */
    public clntProxy proxy;

    /**
     * server to use
     */
    public String server;

    /**
     * port to use
     */
    public int port;

    /**
     * accepted prefixes
     */
    public tabRoute<addrIP> table = new tabRoute<addrIP>("rx");

    /**
     * create new instance
     *
     * @param parent process
     */
    public rtrBgpRpki(rtrBgp parent) {
        lower = parent;
    }

    public String toString() {
        return rpkiName;
    }

    public int compare(rtrBgpRpki o1, rtrBgpRpki o2) {
        return o1.rpkiName.compareTo(o2.rpkiName);
    }

    /**
     * stop this peer
     */
    protected void stopNow() {
        need2run = false;
        if (pipe != null) {
            pipe.setClose();
        }
    }

    /**
     * start this peer
     */
    protected void startNow() {
        need2run = true;
        new Thread(this).start();
    }

    /**
     * get configuration
     *
     * @param l list to append
     * @param beg beginning
     */
    public void getConfig(List<String> l, String beg) {
        l.add(beg + "rpki " + rpkiName + " " + proxy.name + " " + server + " " + port);
    }

    public void run() {
        try {
            for (;;) {
                doWork();
                if (!need2run) {
                    break;
                }
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

    private void doWork() {
        bits.sleep(1000);
        addrIP adr = userTerminal.justResolv(server, 0);
        if (adr == null) {
            return;
        }
        pipe = proxy.doConnect(servGeneric.protoTcp, adr, port, "rpki");
        if (pipe == null) {
            return;
        }
        packRpki pck = new packRpki();
        pck.typ = packRpki.msgResetQuery;
        pck.sendPack(pipe);
        if (debugger.rtrBgpTraf) {
            logger.debug("tx " + pck.dump());
        }
        logger.warn("rpki " + rpkiName + " up");
        int cnt = 0;
        int dat = 0;
        for (;;) {
            if (pipe.isClosed() != 0) {
                break;
            }
            if (!need2run) {
                break;
            }
            bits.sleep(1000);
            cnt++;
            pck = new packRpki();
            if (cnt >= 30) {
                cnt = 0;
                pck.serial = serial;
                pck.sess = session;
                pck.typ = packRpki.msgSerialQuery;
                pck.sendPack(pipe);
                if (debugger.rtrBgpTraf) {
                    logger.debug("tx " + pck.dump());
                }
            }
            for (;;) {
                if (pipe.ready2rx() < 1) {
                    break;
                }
                if (pck.recvPack(pipe)) {
                    pipe.setClose();
                    break;
                }
                if (debugger.rtrBgpTraf) {
                    logger.debug("rx " + pck.dump());
                }
                tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
                switch (pck.typ) {
                    case packRpki.msgIpv4addr:
                        if ((lower.afiUni & rtrBgpUtil.afiMask) != rtrBgpUtil.afiIpv4) {
                            break;
                        }
                        ntry.prefix = addrPrefix.ip4toIP(pck.pref4);
                        ntry.metric = pck.max;
                        ntry.rouSrc = pck.as;
                        if (pck.withdraw) {
                            table.del(ntry);
                        } else {
                            table.add(tabRoute.addType.always, ntry, true, true);
                        }
                        dat++;
                        break;
                    case packRpki.msgIpv6addr:
                        if ((lower.afiUni & rtrBgpUtil.afiMask) != rtrBgpUtil.afiIpv6) {
                            break;
                        }
                        ntry.prefix = addrPrefix.ip6toIP(pck.pref6);
                        ntry.metric = pck.max;
                        ntry.rouSrc = pck.as;
                        if (pck.withdraw) {
                            table.del(ntry);
                        } else {
                            table.add(tabRoute.addType.always, ntry, true, true);
                        }
                        dat++;
                        break;
                    case packRpki.msgCacheReply:
                        break;
                    case packRpki.msgCacheReset:
                        table.clear();
                        dat++;
                        pck.typ = packRpki.msgResetQuery;
                        pck.sendPack(pipe);
                        if (debugger.rtrBgpTraf) {
                            logger.debug("tx " + pck.dump());
                        }
                        break;
                    case packRpki.msgEndData:
                        session = pck.sess;
                        serial = pck.serial;
                        if (dat > 0) {
                            lower.needFull.add(1);
                            lower.compute.wakeup();
                        }
                        dat = 0;
                        break;
                }
            }
        }
        table.clear();
        logger.error("rpki " + rpkiName + " down");
        pipe.setClose();
        pipe = null;
    }

}
