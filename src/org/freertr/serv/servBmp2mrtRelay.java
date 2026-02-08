package org.freertr.serv;

import org.freertr.addr.addrIP;
import org.freertr.cfg.cfgAceslst;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgProxy;
import org.freertr.clnt.clntDns;
import org.freertr.clnt.clntProxy;
import org.freertr.pack.packHolder;
import org.freertr.pipe.pipeSide;
import org.freertr.rtr.rtrBgpMon;
import org.freertr.tab.tabAceslstN;
import org.freertr.tab.tabListing;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.logger;

/**
 * bmp server relay
 *
 * @author matecsaba
 */
public class servBmp2mrtRelay implements Comparable<servBmp2mrtRelay>, Runnable {

    /**
     * proxy to use
     */
    protected clntProxy proxy;

    /**
     * server to connect
     */
    protected String server;

    /**
     * port to use
     */
    protected int port;

    private tabListing<tabAceslstN<addrIP>, addrIP> acl;

    private boolean need2run;

    private pipeSide pipe;

    /**
     * create instance
     */
    public servBmp2mrtRelay() {
    }

    /**
     * convert from string
     *
     * @param cmd commands to read
     * @return true on error, false on success
     */
    public boolean fromString(cmds cmd) {
        cfgProxy prx = cfgAll.proxyFind(cmd.word(), false);
        if (prx == null) {
            cmd.error("no such proxy");
            return true;
        }
        proxy = prx.proxy;
        server = cmd.word();
        port = bits.str2num(cmd.word());
        String a = cmd.word();
        if (a.length() < 1) {
            return false;
        }
        cfgAceslst ac = cfgAll.aclsFind(a, false);
        if (ac == null) {
            cmd.error("no such access list");
            return true;
        }
        acl = ac.aceslst;
        return false;
    }

    public String toString() {
        String a = "";
        if (acl != null) {
            a = " " + acl.listName;
        }
        return proxy.name + " " + server + " " + port + a;
    }

    public int compareTo(servBmp2mrtRelay o) {
        if (port < o.port) {
            return -1;
        }
        if (port > o.port) {
            return +1;
        }
        return server.compareTo(o.server);
    }

    /**
     * start work
     */
    public void startWork() {
        need2run = true;
        logger.startThread(this);
    }

    /**
     * stop work
     */
    public void stopWork() {
        need2run = false;
        if (pipe == null) {
            return;
        }
        pipe.setClose();
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
        addrIP adr = clntDns.justResolv(server, proxy.prefer);
        if (adr == null) {
            return;
        }
        pipe = proxy.doConnect(servGeneric.protoTcp, adr, port, "bmp");
        if (pipe == null) {
            return;
        }
        packHolder pck = new packHolder(true, true);
        pck.putByte(0, 3); // version
        pck.msbPutD(1, servBmp2mrt.size); // length
        pck.putByte(5, rtrBgpMon.typInit); // type
        pck.putSkip(servBmp2mrt.size);
        pck.merge2beg();
        pck.pipeSend(pipe, 0, pck.dataSize(), 1);
        logger.warn("relay " + server + " up");
        for (;;) {
            if (pipe.isClosed() != 0) {
                break;
            }
            if (!need2run) {
                break;
            }
            bits.sleep(1000);
        }
        logger.warn("relay " + server + " down");
        pipe.setClose();
        pipe = null;
    }

    /**
     * got a message
     *
     * @param as as number
     * @param from connection source
     * @param peer remote peer address
     * @param typ message type
     * @param pck message body
     */
    public void gotMessage(int as, addrIP from, addrIP peer, int typ, packHolder pck) {
        if (pipe == null) {
            return;
        }
        pck = pck.copyBytes(true, true);
        if (acl != null) {
            pck.IPsrc.setAddr(from);
            pck.IPtrg.setAddr(peer);
            pck.UDPtrg = as;
            if (!acl.matches(false, false, pck)) {
                return;
            }
        }
        pck.putByte(0, 3); // version
        pck.msbPutD(1, servBmp2mrt.size + pck.dataSize()); // length
        pck.putByte(5, typ); // type
        pck.putSkip(servBmp2mrt.size);
        pck.merge2beg();
        pck.pipeSend(pipe, 0, pck.dataSize(), 1);
    }

}
