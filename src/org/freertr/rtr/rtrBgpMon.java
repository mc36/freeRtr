package org.freertr.rtr;

import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv4;
import org.freertr.cfg.cfgAll;
import org.freertr.clnt.clntDns;
import org.freertr.clnt.clntProxy;
import org.freertr.pack.packHolder;
import org.freertr.pipe.pipeSide;
import org.freertr.serv.servGeneric;
import org.freertr.util.bits;
import org.freertr.util.logger;
import org.freertr.enc.encTlv;

/**
 * bgp monitor protocol
 *
 * @author matecsaba
 */
public class rtrBgpMon implements Comparable<rtrBgpMon>, Runnable {

    private final rtrBgp parent;

    private pipeSide pipe;

    private boolean need2run;

    /**
     * name of monitor
     */
    public final String monName;

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
     * header size
     */
    public final static int size = 48;

    /**
     * monitor
     */
    public final static int typMon = 0;

    /**
     * statistics
     */
    public final static int typStat = 1;

    /**
     * peer down
     */
    public final static int typPerDn = 2;

    /**
     * peer up
     */
    public final static int typPerUp = 3;

    /**
     * initialization
     */
    public final static int typInit = 4;

    /**
     * termination
     */
    public final static int typTerm = 5;

    /**
     * create instance
     *
     * @param lower parent
     * @param nam name of monitor
     */
    public rtrBgpMon(rtrBgp lower, String nam) {
        parent = lower;
        monName = nam;
    }

    public String toString() {
        return monName;
    }

    public int compareTo(rtrBgpMon o) {
        return monName.compareTo(o.monName);
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
        logger.startThread(this);
    }

    /**
     * get tlv handler
     *
     * @return tlv
     */
    public static encTlv getTlv() {
        return new encTlv(0, 16, 16, 16, 1, 0, 4, 1, 0, 1024, true);
    }

    /**
     * get configuration
     *
     * @param l list to append
     * @param beg beginning
     */
    public void getConfig(List<String> l, String beg) {
        l.add(beg + "monitor " + monName + " " + proxy.name + " " + server + " " + port);
    }

    public void run() {
        try {
            for (;;) {
                doWork();
                if (!need2run) {
                    break;
                }
                bits.sleep(1000);
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

    private void sendCounter(packHolder pck, rtrBgpNeigh nei) {
        if (nei == null) {
            return;
        }
        if (nei.monitor != this) {
            return;
        }
        pck.clear();
        pck.msbPutD(0, 5); // number of counters
        pck.putSkip(4);
        pck.merge2end();
        encTlv tlv = getTlv();
        tlv.valSiz = 4;
        bits.msbPutD(tlv.valDat, 0, nei.conn.repPolRej);
        tlv.putBytes(pck, 0); // policy reject
        bits.msbPutD(tlv.valDat, 0, nei.conn.repClstrL);
        tlv.putBytes(pck, 3); // cluster list loop
        bits.msbPutD(tlv.valDat, 0, nei.conn.repAsPath);
        tlv.putBytes(pck, 4); // aspath loop
        bits.msbPutD(tlv.valDat, 0, nei.conn.repOrgnId);
        tlv.putBytes(pck, 5); // originator id loop
        bits.msbPutD(tlv.valDat, 0, nei.conn.repAsConf);
        tlv.putBytes(pck, 6); // as confed loop
        pck.merge2end();
        doSend(pipe, pck, false, typStat, nei.conn, nei);
    }

    private void doWork() {
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
        pck.msbPutD(1, 6); // length
        pck.putByte(5, typInit); // type
        pck.putSkip(6);
        pck.merge2beg();
        pck.pipeSend(pipe, 0, pck.dataSize(), 1);
        logger.warn("monitor " + monName + " up");
        int cnt = 0;
        for (;;) {
            if (pipe.isClosed() != 0) {
                break;
            }
            if (!need2run) {
                break;
            }
            bits.sleep(1000);
            cnt++;
            if (cnt < 30) {
                continue;
            }
            cnt = 0;
            if (pipe.ready2tx() < 1024) {
                continue;
            }
            for (int i = 0; i < parent.neighs.size(); i++) {
                sendCounter(pck, parent.neighs.get(i));
            }
            for (int i = 0; i < parent.lstnNei.size(); i++) {
                sendCounter(pck, parent.lstnNei.get(i));
            }
        }
        logger.error("monitor " + monName + " down");
        pipe.setClose();
        pipe = null;
    }

    /**
     * create header
     *
     * @param pck packet to update
     * @param tim timestamp
     * @param dir direction: false=rx, true=tx
     * @param typ message type
     * @param per peer address
     * @param asn as number
     * @param rid router id
     */
    public static void createHeader(packHolder pck, long tim, boolean dir, int typ, addrIP per, int asn, addrIPv4 rid) {
        pck.putByte(0, 3); // version
        pck.msbPutD(1, pck.dataSize() + size); // length
        pck.putByte(5, typ); // type
        pck.putByte(6, 0); // peer type
        int i = 0;
        if (!per.isIPv4()) {
            i |= 0x80;
        }
        if (dir) {
            i |= 0x10;
        }
        pck.putByte(7, i); // peer flags
        pck.msbPutQ(8, 0); // peer distinguisher
        pck.putAddr(16, per); // address
        pck.msbPutD(32, asn); // as
        pck.putAddr(36, rid); // routerid
        pck.msbPutD(40, (int) (tim / 1000)); // seconds
        pck.msbPutD(44, 1000 * (int) (tim % 1000)); // microsecs
        pck.putSkip(size);
        pck.merge2beg();
    }

    private static void doSend(pipeSide pip, packHolder pck, boolean dir, int typ, rtrBgpSpeak spk, rtrBgpNeigh nei) {
        if (pip == null) {
            return;
        }
        createHeader(pck, bits.getTime() + cfgAll.timeServerOffset, dir, typ, nei.peerAddr, nei.remoteAs, spk.peerRouterID);
        pck.pipeSend(pip, 0, pck.dataSize(), 1);
    }

    /**
     * got event
     *
     * @param state state: false=down, true=up
     * @param spk speaker
     * @param nei neighbor
     */
    public void gotEvent(boolean state, rtrBgpSpeak spk, rtrBgpNeigh nei) {
        packHolder pck = new packHolder(true, true);
        int i;
        if (state) {
            i = typPerUp;
            pck.putSkip(20);
        } else {
            i = typPerDn;
            pck.putSkip(1);
        }
        pck.merge2beg();
        doSend(pipe, pck, false, i, spk, nei);
    }

    /**
     * got update
     *
     * @param dir direction: false=rx, true=tx
     * @param typ type
     * @param spk speaker
     * @param nei neighbor
     * @param buf data bytes
     */
    public void gotMessage(boolean dir, int typ, rtrBgpSpeak spk, rtrBgpNeigh nei, byte[] buf) {
        packHolder pck = new packHolder(true, true);
        pck.putCopy(buf, 0, 0, buf.length);
        pck.putSkip(buf.length);
        pck.merge2beg();
        rtrBgpUtil.createHeader(pck, typ);
        doSend(pipe, pck, dir, typMon, spk, nei);
    }

}
