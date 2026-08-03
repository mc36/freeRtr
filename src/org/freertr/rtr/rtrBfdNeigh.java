package org.freertr.rtr;

import org.freertr.addr.addrIP;
import org.freertr.pack.packBfd;
import org.freertr.pack.packHolder;
import org.freertr.prt.prtGenConn;
import org.freertr.tab.tabGen;
import org.freertr.util.bits;
import org.freertr.util.debugger;
import org.freertr.util.logger;

/**
 * bfd neighbor
 *
 * @author matecsaba
 */
public class rtrBfdNeigh implements Runnable, Comparable<rtrBfdNeigh> {

    /**
     * address of peer
     */
    public final addrIP peer;

    private final rtrBfdIface iface;

    /**
     * transmit connection
     */
    protected prtGenConn connTx;

    /**
     * receive connection
     */
    protected prtGenConn connRx;

    private tabGen<rtrBfdNeighClnt> clients;

    private boolean need2run;

    private packBfd lastRxPack;

    private long lastRxTime;

    private int currState;

    private int myDisc;

    private long upTime;

    /**
     * create one neighbor
     *
     * @param ifc local interface
     * @param adr address of peer
     */
    public rtrBfdNeigh(rtrBfdIface ifc, addrIP adr) {
        peer = adr.copyBytes();
        iface = ifc;
        clients = new tabGen<rtrBfdNeighClnt>();
    }

    public String toString() {
        return "bfd with " + peer;
    }

    public int compareTo(rtrBfdNeigh o) {
        return peer.compareTo(o.peer);
    }

    /**
     * check state
     *
     * @return true on up, false on error
     */
    public boolean getState() {
        return currState == packBfd.stUp;
    }

    /**
     * get neighbor info
     *
     * @return string
     */
    public String getShNeigh() {
        String s = peer + "|" + packBfd.state2string(currState) + "|" + packBfd.getRxInt(lastRxPack, iface.intervalRx) + "|" + bits.timePast(upTime) + "|";
        for (int i = 0; i < clients.size(); i++) {
            rtrBfdNeighClnt clnt = clients.get(i);
            if (clnt == null) {
                continue;
            }
            s += " " + clnt.name;
        }
        return s;
    }

    /**
     * check if have clients
     *
     * @return true if have, false if not
     */
    public boolean hasClients() {
        return clients.size() > 0;
    }

    /**
     * add one client
     *
     * @param clnt client to add
     * @param nam name of client
     * @return false if now added, true if already
     */
    public boolean clientAdd(rtrBfdClnt clnt, String nam) {
        return clients.add(new rtrBfdNeighClnt(clnt, nam)) != null;
    }

    /**
     * delete one client
     *
     * @param clnt client to delete
     * @return true if deleted, false if not
     */
    public boolean clientDel(rtrBfdClnt clnt) {
        return clients.del(new rtrBfdNeighClnt(clnt, "")) != null;
    }

    /**
     * start this peer
     *
     * @return true on error, false on success
     */
    protected boolean startNow() {
        if (debugger.rtrBfdEvnt) {
            logger.debug("starting with " + peer);
        }
        upTime = bits.getTime();
        connTx = iface.udp.packetConnect(iface, iface.ifc, packBfd.portLoc, peer, packBfd.portLoc, "bfd", -1, null, -1, -1);
        if (connTx == null) {
            return true;
        }
        connTx.timeout = 0;
        connTx.workInterval = 10000;
        connTx.sendTTL = 255;
        connTx.sendTOS = 0;
        connTx.sendDFN = 0;
        connTx.sendFLW = 0;
        need2run = true;
        myDisc = bits.randomD();
        logger.startThread(this);
        return false;
    }

    /**
     * stop this peer
     */
    public void stopNow() {
        logger.error("neighbor " + peer + " down");
        need2run = false;
        iface.neighs.del(this);
        for (int i = 0; i < clients.size(); i++) {
            rtrBfdNeighClnt clnt = clients.get(i);
            if (clnt == null) {
                continue;
            }
            clnt.clnt.bfdPeerDown();
        }
        if (connTx != null) {
            connTx.setClosing();
        }
        if (connRx != null) {
            connRx.setClosing();
        }
    }

    /**
     * received one packet
     *
     * @param pck packet received
     */
    protected void recvPack(packHolder pck) {
        packBfd bfd = new packBfd();
        if (bfd.parseHeader(pck)) {
            return;
        }
        if (debugger.rtrBfdTraf) {
            logger.debug("rx from " + peer + " " + bfd);
        }
        lastRxTime = bits.getTime();
        lastRxPack = bfd;
    }

    public void run() {
        try {
            doWork();
        } catch (Exception e) {
            logger.traceback(e);
            stopNow();
        }
        if (debugger.rtrBfdEvnt) {
            logger.debug("stopped with " + peer);
        }
    }

    private void doWork() {
        packHolder pckR = new packHolder(true, true);
        packBfd pckB = new packBfd();
        pckB.discrLoc = myDisc;
        int lastState = packBfd.stShut;
        for (;;) {
            if (!need2run) {
                return;
            }
            int txint = pckB.getTxInt(lastRxPack, iface.intervalTx);
            pckB.timeRx = iface.intervalRx * 1000;
            pckB.timeTx = txint * 1000;
            pckB.multiplier = iface.multiplier;
            pckB.keyId = iface.keyId;
            pckB.password = iface.password;
            currState = packBfd.stDown;
            if (lastRxPack != null) {
                if ((bits.getTime() - lastRxTime) > packBfd.getRxInt(lastRxPack, iface.intervalRx)) {
                    stopNow();
                    return;
                }
                if ((lastRxPack.flags & packBfd.flgPoll) != 0) {
                    pckB.flags |= packBfd.flgFinal;
                } else {
                    pckB.flags = 0;
                }
                switch (lastRxPack.status) {
                    case packBfd.stUp:
                    case packBfd.stInit:
                        currState = packBfd.stUp;
                        break;
                    default:
                        currState = packBfd.stInit;
                        break;
                }
                if (iface.password != null) {
                    if (lastRxPack.password == null) {
                        currState = packBfd.stDown;
                    } else {
                        if (iface.keyId != lastRxPack.keyId) {
                            currState = packBfd.stDown;
                        }
                        if (iface.password.compareTo(lastRxPack.password) != 0) {
                            currState = packBfd.stDown;
                        }
                    }
                }
                pckB.discrRem = lastRxPack.discrLoc;
            }
            pckB.status = currState;
            pckR.clear();
            pckB.createHeader(pckR);
            connTx.send2net(pckR);
            if (debugger.rtrBfdTraf) {
                logger.debug("sending to " + peer + " " + pckB);
            }
            if (lastState != currState) {
                if (debugger.rtrBfdEvnt) {
                    logger.debug("peer " + peer + " from " + packBfd.state2string(lastState) + " to "
                            + packBfd.state2string(currState));
                }
                if (lastState == packBfd.stUp) {
                    stopNow();
                    return;
                }
                if (currState == packBfd.stUp) {
                    logger.warn("neighbor " + peer + " up");
                }
                lastState = currState;
            }
            bits.sleep(txint);
        }
    }

}

class rtrBfdNeighClnt implements Comparable<rtrBfdNeighClnt> {

    public final rtrBfdClnt clnt;

    public final String name;

    public final int hsh;

    public rtrBfdNeighClnt(rtrBfdClnt client, String nam) {
        clnt = client;
        name = nam;
        hsh = clnt.hashCode();
    }

    public int compareTo(rtrBfdNeighClnt o) {
        if (hsh < o.hsh) {
            return -1;
        }
        if (hsh > o.hsh) {
            return +1;
        }
        return 0;
    }

}
