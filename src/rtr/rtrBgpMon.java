package rtr;

import addr.addrIP;
import cfg.cfgAll;
import clnt.clntProxy;
import java.util.Comparator;
import java.util.List;
import pack.packHolder;
import pipe.pipeSide;
import serv.servGeneric;
import user.userTerminal;
import util.bits;
import util.logger;

/**
 * bgp monitor protocol
 *
 * @author matecsaba
 */
public class rtrBgpMon implements Comparator<rtrBgpMon>, Runnable {

    private pipeSide pipe;

    private boolean need2run;

    /**
     * name of monitor
     */
    public String monName;

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
    public static final int size = 48;

    /**
     * monitor
     */
    public static final int typMon = 0;

    /**
     * statistics
     */
    public static final int typStat = 1;

    /**
     * peer down
     */
    public static final int typPerDn = 2;

    /**
     * peer up
     */
    public static final int typPerUp = 3;

    /**
     * initialization
     */
    public static final int typInit = 4;

    /**
     * termination
     */
    public static final int typTerm = 5;

    public String toString() {
        return monName;
    }

    public int compare(rtrBgpMon o1, rtrBgpMon o2) {
        return o1.monName.compareTo(o2.monName);
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
        l.add(beg + "monitor " + monName + " " + proxy.name + " " + server + " " + port);
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
            pck.clear();
            pck.putByte(0, 3); // version
            pck.msbPutD(1, 6); // length
            pck.putByte(5, typStat); // type
            pck.putSkip(6);
            pck.merge2beg();
            pck.pipeSend(pipe, 0, pck.dataSize(), 1);
        }
        logger.error("monitor " + monName + " down");
        pipe.setClose();
        pipe = null;
    }

    private void doSend(packHolder pck, boolean dir, int typ, rtrBgpSpeak spk, rtrBgpNeigh nei) {
        if (pipe == null) {
            return;
        }
        pck.putByte(0, 3); // version
        pck.msbPutD(1, pck.dataSize() + size); // length
        pck.putByte(5, typ); // type
        pck.putByte(6, 0); // peer type
        int i = 0;
        if (!nei.peerAddr.isIPv4()) {
            i |= 0x80;
        }
        if (dir) {
            i |= 0x40;
        }
        pck.putByte(7, i); // peer flags
        pck.msbPutQ(8, 0); // peer distinguisher
        pck.putAddr(16, nei.peerAddr); // address
        pck.msbPutD(32, nei.remoteAs); // as
        pck.putAddr(36, spk.peerRouterID); // routerid
        long l = bits.getTime() + cfgAll.timeServerOffset;
        pck.msbPutD(40, (int) (l / 1000)); // seconds
        pck.msbPutD(44, (int) (l % 1000)); // microsecs
        pck.putSkip(size);
        pck.merge2beg();
        pck.pipeSend(pipe, 0, pck.dataSize(), 1);
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
        doSend(pck, false, i, spk, nei);
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
        for (int i = 0; i < 16; i++) {
            pck.putByte(i, 0xff);
        }
        pck.msbPutW(16, pck.dataSize() + rtrBgpSpeak.sizeU);
        pck.putByte(18, typ);
        pck.putSkip(rtrBgpSpeak.sizeU);
        pck.merge2beg();
        doSend(pck, dir, typMon, spk, nei);
    }

}
