package prt;

import addr.addrIP;
import ip.ipFwdIface;
import java.util.Comparator;
import pack.packHolder;
import pipe.pipeLine;
import pipe.pipeSide;
import tab.tabConnectLower;
import util.bits;
import util.counter;
import util.debugger;
import util.logger;
import util.notifier;

/**
 * one packet connection handler
 *
 * @author matecsaba
 */
public class prtGenConn implements Runnable, Comparator<prtGenConn>, tabConnectLower {

    /**
     * notifier of runner
     */
    public final notifier notif;

    /**
     * protocol data may stored in this field
     */
    public Object proto;

    /**
     * sending protocol
     */
    public int sendPRT = -1;

    /**
     * sending tos
     */
    public int sendTOS = -1;

    /**
     * sending ttl
     */
    public int sendTTL = -1;

    /**
     * timeout
     */
    public int timeout;

    /**
     * reporting router
     */
    public addrIP errRtr;

    /**
     * reported error
     */
    public counter.reasons errCod;

    /**
     * direction of connection: true=incoming, false=outgoing
     */
    public final boolean direction;

    /**
     * local port
     */
    public final int portLoc;

    /**
     * remote port
     */
    public final int portRem;

    /**
     * peer address
     */
    public final addrIP peerAddr;

    /**
     * my name
     */
    public final String name;

    /**
     * sending interface to use
     */
    public final ipFwdIface iface;

    /**
     * time to wait between works
     */
    public int workInterval;

    /**
     * time of last activity
     */
    protected long lastActivity;

    /**
     * client side of pipeline
     */
    protected final pipeSide pipeClient;

    /**
     * password if applicable
     */
    public final String passwd;

    private final pipeSide pipeNetwork;

    private final boolean stream;

    private final prtGen lower;

    private final prtServP upperP;

    private final prtServS upperS;

    private boolean ready = false;

    private boolean closing = false;

    private boolean deleted = false;

    private boolean registered = false;

    public int compare(prtGenConn o1, prtGenConn o2) {
        if (o1.portLoc < o2.portLoc) {
            return -1;
        }
        if (o1.portLoc > o2.portLoc) {
            return +1;
        }
        if (o1.portRem < o2.portRem) {
            return -1;
        }
        if (o1.portRem > o2.portRem) {
            return +1;
        }
        return o1.peerAddr.compare(o1.peerAddr, o2.peerAddr);
    }

    /**
     * the constructor
     *
     * @param low lower layer
     * @param upP upper layer for packet mode
     * @param upS upper layer for stream mode
     * @param dir direction of connection, true=accepted, false=initiated
     * @param pip sample pipeline for stream mode
     * @param ifc interface
     * @param prtL local port
     * @param adrR remote address
     * @param prtR remote port
     * @param nam name of client
     * @param pwd password if applicable
     * @param ttl time to live
     */
    protected prtGenConn(prtGen low, prtServP upP, prtServS upS, pipeLine pip, boolean dir, ipFwdIface ifc, int prtL, addrIP adrR, int prtR, String nam, String pwd, int ttl) {
        iface = ifc;
        passwd = pwd;
        sendTTL = ttl;
        portLoc = prtL;
        portRem = prtR;
        peerAddr = adrR.copyBytes();
        name = nam;
        lower = low;
        upperP = upP;
        upperS = upS;
        direction = dir;
        lastActivity = bits.getTime();
        timeout = 120000;
        workInterval = 10000;
        if (pip == null) {
            stream = false;
            pipeNetwork = null;
            pipeClient = null;
            notif = new notifier();
            return;
        }
        stream = true;
        boolean b = true;
        if (upperS != null) {
            b = upperS.streamForceBlock();
        }
        if (b) {
            b = pip.isBlockMode();
        } else {
            b = low.getProtoNum() != prtTcp.protoNum;
        }
        pipeLine pipeHandler = pipeLine.doClone(pip, b);
        pipeNetwork = pipeHandler.getSide();
        pipeClient = pipeHandler.getSide();
        pipeClient.setReady();
        pipeClient.timeout = timeout;
        notif = pipeNetwork.notif;
    }

    public String toString() {
        return name + " #" + ipFwdIface.getNum(iface) + " " + portLoc + " -> " + peerAddr + " " + portRem;
    }

    /**
     * signal ready on connection
     *
     * @return status, false means success, true means error
     */
    protected boolean setReady() {
        if (ready) {
            return true;
        }
        if (debugger.prtGenTraf) {
            logger.debug("ready " + this);
        }
        ready = true;
        if (stream) {
            pipeNetwork.setReady();
        } else {
            upperP.datagramReady(this);
        }
        return false;
    }

    /**
     * immediately delete this connection
     *
     * @return status, false means success, true means error
     */
    protected synchronized boolean deleteImmediately() {
        if (deleted) {
            return true;
        }
        if (debugger.prtGenTraf) {
            logger.debug("delete " + this);
        }
        deleted = true;
        if (!registered) {
            return false;
        }
        if (lower.clnts.del(iface.ifwNum, peerAddr, 0, portLoc, portRem, false) != null) {
            registered = false;
            return false;
        }
        logger.info("failed to del " + this);
        return true;
    }

    /**
     * register connection to lower layer
     *
     * @return status, false means success, true means error
     */
    protected synchronized boolean register2lower() {
        if (closing || deleted) {
            return true;
        }
        if (registered) {
            return true;
        }
        if (debugger.prtGenTraf) {
            logger.debug("register " + this);
        }
        if (!lower.clnts.add(iface.ifwNum, peerAddr, 0, portLoc, portRem, this, name, true)) {
            registered = true;
            new Thread(this).start();
            return false;
        }
        logger.info("failed to add " + this);
        return true;
    }

    /**
     * send datagram to server
     *
     * @param pck packet to send
     * @return return false if successful, true if error happened
     */
    protected boolean send2server(packHolder pck) {
        if (closing) {
            return false;
        }
        if (stream) {
            return pck.pipeSend(pipeNetwork, 0, pck.dataSize(), 1) < 1;
        } else {
            return upperP.datagramRecv(this, pck);
        }
    }

    /**
     * check rx queue at server
     *
     * @return return bytes free at server rx queue
     */
    protected int freeAtServer() {
        if (stream) {
            return pipeNetwork.ready2tx();
        } else {
            return 0xffffff;
        }
    }

    /**
     * try to accept new connection
     *
     * @return status, false means success, true means error
     */
    protected boolean newConnectAccept() {
        boolean b;
        if (stream) {
            b = upperS.streamAccept(pipeClient, this);
        } else {
            b = upperP.datagramAccept(this);
        }
        if (debugger.prtGenTraf) {
            if (b) {
                logger.debug("refused " + this);
            } else {
                logger.debug("accepted " + this);
            }
        }
        return b;
    }

    /**
     * upper wants close connection
     *
     * @return true if error happened, false if not
     */
    public boolean setClosing() {
        if (closing) {
            return true;
        }
        if (debugger.prtGenTraf) {
            logger.debug("closing " + this);
        }
        timeout = 10000;
        closing = true;
        ready = false;
        lastActivity = bits.getTime();
        lower.connectionClose(this);
        if (stream) {
            pipeNetwork.setClose();
        } else {
            upperP.datagramClosed(this);
        }
        return false;
    }

    /**
     * check how much bytes free in tx buffer of protocol
     *
     * @return number of bytes ready, -2=no connection, -1=not opened yet
     */
    public int txBytesFree() {
        if (closing || deleted) {
            return -2;
        }
        if (!ready) {
            return -1;
        }
        return lower.connectionBytes(this);
    }

    /**
     * send datagram to network
     *
     * @param pck packet to send
     * @return return true if connection not exists
     */
    public boolean send2net(packHolder pck) {
        if (closing || deleted) {
            return true;
        }
        pck.merge2beg();
        pck.UDPsrc = portLoc;
        pck.UDPtrg = portRem;
        pck.IPtrg.setAddr(peerAddr);
        pck.IPsrc.setAddr(iface.addr);
        pck.IPdf = false;
        pck.IPttl = sendTTL;
        pck.IPtos = sendTOS;
        if (lower.connectionSend(this, pck)) {
            return true;
        }
        return false;
    }

    /**
     * do timeout work
     */
    protected void internalTimeout() {
        if (timeout < 1) {
            return;
        }
        if ((bits.getTime() - lastActivity) < timeout) {
            return;
        }
        if (setClosing()) {
            deleteImmediately();
            return;
        }
    }

    /**
     * do internal work
     *
     * @return false on success, true on error
     */
    protected boolean internalWorker() {
        if (!ready) {
            return true;
        }
        if (!stream) {
            upperP.datagramWork(this);
            return true;
        }
        int siz = lower.connectionBytes(this);
        if (siz < 0) {
            setClosing();
            return true;
        }
        siz -= 512;
        if (siz < 1) {
            return true;
        }
        packHolder pck = new packHolder(true, true);
        pck.clear();
        siz = pck.pipeRecv(pipeNetwork, 0, siz, 142);
        if (siz < 1) {
            if (pipeNetwork.isClosed() == 0) {
                return true;
            }
            setClosing();
            return true;
        }
        if (send2net(pck)) {
            logger.info("failed to send to net " + this);
            return true;
        }
        return false;
    }

    public void run() {
        if (debugger.prtGenTraf) {
            logger.debug("started " + this);
        }
        try {
            notif.misleep(workInterval);
            for (;;) {
                if (deleted) {
                    break;
                }
                internalTimeout();
                for (;;) {
                    if (internalWorker()) {
                        break;
                    }
                }
                lower.connectionWork(this);
                notif.misleep(workInterval);
            }
        } catch (Exception e) {
            logger.exception(e);
        }
        if (debugger.prtGenTraf) {
            logger.debug("stopped " + this);
        }
    }

    /**
     * dump
     *
     * @return string
     */
    public String dumper() {
        return "" + proto;
    }

}
