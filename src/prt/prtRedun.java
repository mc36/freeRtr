package prt;

import addr.addrMac;
import cfg.cfgInit;
import ifc.ifcDn;
import ifc.ifcThread;
import ifc.ifcUp;
import java.io.RandomAccessFile;
import java.util.ArrayList;
import java.util.List;
import pack.packHolder;
import pack.packRedun;
import user.userFormat;
import user.userFlash;
import util.bits;
import util.counter;
import util.debugger;
import util.logger;
import util.notifier;
import util.state;
import util.version;

/**
 * redundancy protocol
 *
 * @author matecsaba
 */
public class prtRedun implements Runnable {

    /**
     * my magic number
     */
    protected static int magic = 0;

    /**
     * current state
     */
    protected static int state = 0;

    private final static List<prtRedunIfc> ifaces = new ArrayList<prtRedunIfc>();

    public void run() {
        try {
            for (;;) {
                bits.sleep(packRedun.timeKeep);
                if (!cfgInit.booting) {
                    state = packRedun.statActive;
                }
                for (int i = 0; i < ifaces.size(); i++) {
                    ifaces.get(i).doPack(packRedun.typHello, new packHolder(true, true));
                }
            }
        } catch (Exception e) {
            logger.exception(e);
        }
    }

    /**
     * generate show output
     *
     * @return output
     */
    public static userFormat doShow() {
        userFormat l = new userFormat("|", "iface|state|magic");
        l.add("self|" + state + "|" + magic);
        for (int i = 0; i < ifaces.size(); i++) {
            prtRedunIfc ifc = ifaces.get(i);
            l.add(ifc.doShow());
        }
        return l;
    }

    /**
     * add one physical interface
     *
     * @param name name of interface
     * @param thrd interface thread handler
     */
    public static void ifcAdd(String name, ifcThread thrd) {
        prtRedunIfc ifc = new prtRedunIfc();
        ifc.doInit(name, thrd);
        ifaces.add(ifc);
    }

    /**
     * find best peer
     *
     * @return best peer index, -1 if none
     */
    public static int findActive() {
        long tim = bits.getTime();
        int bi = -1;
        int bm = magic;
        for (int i = 0; i < ifaces.size(); i++) {
            prtRedunIfc ifc = ifaces.get(i);
            if (!ifc.bidir) {
                continue;
            }
            if ((tim - ifc.time) > packRedun.timeHold) {
                continue;
            }
            if (ifc.state == packRedun.statActive) {
                return i;
            }
            if (ifc.magic < bm) {
                continue;
            }
            bi = i;
            bm = ifc.magic;
        }
        return bi;
    }

    /**
     * terminate the redundancy
     */
    public static void doShut() {
    }

    /**
     * initialize the redundancy
     */
    public static void doInit() {
        magic = (bits.randomD() & 0x3fffffff) + 1;
        if (ifaces.size() < 1) {
            state = packRedun.statActive;
            return;
        }
        state = packRedun.statSpeak;
        new Thread(new prtRedun()).start();
        bits.sleep(packRedun.timeHold);
        if (findActive() < 0) {
            return;
        }
        state = packRedun.statStandby;
        logger.info("initialized as standby");
        for (;;) {
            bits.sleep(packRedun.timeKeep);
            if (findActive() < 0) {
                break;
            }
        }
        state = packRedun.statActive;
    }

    /**
     * notify peers to reload
     */
    public static void doNotify() {
        for (int i = 0; i < ifaces.size(); i++) {
            prtRedunIfc ifc = ifaces.get(i);
            ifc.doFile(cfgInit.cfgFileSw);
            ifc.doPack(packRedun.typReload, new packHolder(true, true));
        }
    }

}

class prtRedunIfc implements ifcUp {

    private ifcThread lower;

    private counter cntr = new counter();

    private addrMac hwaddr;

    private RandomAccessFile filRx;

    public String name;

    public boolean bidir;

    public int magic;

    public int state;

    public long time;

    public notifier notif = new notifier();

    public int ackRx;

    public void doInit(String nam, ifcThread thrd) {
        name = nam;
        lower = thrd;
        state = 0;
        magic = 0;
        time = 0;
        lower.setFilter(false);
        lower.setUpper(this);
        lower.startLoop(1);
        hwaddr = (addrMac) lower.getHwAddr();
    }

    public String doShow() {
        return name + "|" + state + "|" + magic;
    }

    public void setParent(ifcDn parent) {
    }

    public void setState(state.states stat) {
    }

    public void closeUp() {
    }

    public counter getCounter() {
        return cntr;
    }

    public void recvPack(packHolder pck) {
        packRedun pckP = new packRedun();
        if (pckP.parseHeader(pck)) {
            return;
        }
        if (debugger.prtRedun) {
            logger.debug("rx " + pckP);
        }
        magic = pckP.magic;
        state = pckP.state;
        bidir = pckP.peer == prtRedun.magic;
        time = bits.getTime();
        switch (pckP.type) {
            case packRedun.typHello:
                if ((magic == prtRedun.magic) && (state >= prtRedun.state)) {
                    cfgInit.stopRouter(true, 6, "magic collision");
                }
                if ((prtRedun.state == packRedun.statActive) && (state == packRedun.statActive) && (magic <= prtRedun.magic)) {
                    cfgInit.stopRouter(true, 6, "dual active");
                }
                break;
            case packRedun.typReload:
                if (prtRedun.state == packRedun.statActive) {
                    break;
                }
                cfgInit.stopRouter(true, 6, "peer request");
                break;
            case packRedun.typAck:
                ackRx = pck.msbGetD(0);
                notif.wakeup();
                break;
            case packRedun.typFilBeg:
                try {
                    filRx.close();
                } catch (Exception e) {
                }
                try {
                    filRx = new RandomAccessFile(version.getFileName() + ".tmp", "rw");
                    filRx.seek(0);
                    filRx.setLength(0);
                } catch (Exception e) {
                }
                doAck(-2);
                break;
            case packRedun.typFilDat:
                int i = pck.msbGetD(0);
                int o = pck.msbGetW(4);
                pck.getSkip(6);
                byte[] buf = new byte[o];
                pck.getCopy(buf, 0, 0, o);
                try {
                    filRx.seek(i);
                    filRx.write(buf);
                } catch (Exception e) {
                }
                doAck(i);
                break;
            case packRedun.typFilEnd:
                try {
                    filRx.close();
                } catch (Exception e) {
                }
                filRx = null;
                String a = pck.getAsciiZ(0, packRedun.dataMax, 0);
                String b = null;
                if (a.equals(packRedun.fnCore)) {
                    b = version.getFileName();
                }
                if (a.equals(packRedun.fnStart)) {
                    b = cfgInit.cfgFileSw;
                }
                a = version.getFileName() + ".tmp";
                userFlash.rename(a, b, true, true);
                doAck(-3);
                break;
        }
    }

    public void doPack(int typ, packHolder pckB) {
        pckB.merge2beg();
        packRedun pckP = new packRedun();
        pckP.type = typ;
        pckP.state = prtRedun.state;
        pckP.magic = prtRedun.magic;
        pckP.peer = magic;
        pckP.createHeader(pckB);
        if (debugger.prtRedun) {
            logger.debug("tx " + pckP);
        }
        pckB.ETHsrc.setAddr(hwaddr);
        pckB.ETHtrg.setAddr(addrMac.getBroadcast());
        lower.sendPack(pckB);
    }

    public void doAck(int ofs) {
        packHolder pck = new packHolder(true, true);
        pck.msbPutD(0, ofs);
        pck.putSkip(4);
        doPack(packRedun.typAck, pck);
    }

    public boolean doRetry(int typ, packHolder pck) {
        ackRx = -1;
        for (int i = 0; i < packRedun.timeMult; i++) {
            doPack(typ, pck.copyBytes(true, true));
            notif.sleep(packRedun.timeKeep);
            if (ackRx != -1) {
                return false;
            }
        }
        return true;
    }

    public boolean doFile(String fn) {
        RandomAccessFile fr;
        long siz;
        try {
            fr = new RandomAccessFile(fn, "r");
            siz = fr.length();
        } catch (Exception e) {
            return true;
        }
        if (doRetry(packRedun.typFilBeg, new packHolder(true, true))) {
            try {
                fr.close();
            } catch (Exception e) {
            }
            return true;
        }
        long pos = 0;
        for (; pos < siz;) {
            long rndl = siz - pos;
            if (rndl > packRedun.dataMax) {
                rndl = packRedun.dataMax;
            }
            int rndi = (int) rndl;
            byte buf[] = new byte[rndi];
            try {
                fr.read(buf, 0, rndi);
            } catch (Exception e) {
                return true;
            }
            packHolder pck = new packHolder(true, true);
            pck.msbPutD(0, (int) pos);
            pck.msbPutW(4, buf.length);
            pck.putSkip(6);
            pos += rndl;
            pck.putCopy(buf, 0, 0, buf.length);
            pck.putSkip(buf.length);
            if (doRetry(packRedun.typFilDat, pck)) {
                try {
                    fr.close();
                } catch (Exception e) {
                }
                return true;
            }
        }
        try {
            fr.close();
        } catch (Exception e) {
            return true;
        }
        packHolder pck = new packHolder(true, true);
        pck.putAsciiZ(0, packRedun.dataMax, packRedun.fnStart, 0);
        pck.putSkip(packRedun.dataMax);
        if (doRetry(packRedun.typFilEnd, pck)) {
            return true;
        }
        return false;
    }

}
