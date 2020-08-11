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
import pipe.pipeSide;
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
    protected static int magic = (bits.randomD() & 0x3fffffff) + 1;

    /**
     * current state
     */
    protected static int state = packRedun.statInit;

    /**
     * current uptime
     */
    protected static int uptime = 0;
    
    private final static List<prtRedunIfc> ifaces = new ArrayList<prtRedunIfc>();
    
    public void run() {
        try {
            packHolder pck = new packHolder(true, true);
            for (;;) {
                uptime = (int) ((bits.getTime() - cfgInit.started) / 1000);
                for (int i = 0; i < ifaces.size(); i++) {
                    pck.clear();
                    ifaces.get(i).doPack(packRedun.typHello, pck);
                }
                bits.sleep(packRedun.timeKeep);
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
        userFormat l = new userFormat("|", "iface|state|bidir|magic|uptime|heard");
        l.add("self|" + state + "|-|" + magic + "|" + bits.timeDump(uptime) + "|-");
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
            if ((tim - ifc.heard) > packRedun.timeHold) {
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
     *
     * @param con console
     */
    public static void doInit(pipeSide con) {
        if (ifaces.size() < 1) {
            state = packRedun.statActive;
            return;
        }
        logger.info("initializing redundancy");
        state = packRedun.statSpeak;
        new Thread(new prtRedun()).start();
        bits.sleep(packRedun.timeInit);
        if (findActive() < 0) {
            state = packRedun.statActive;
            logger.info("became active");
            return;
        }
        state = packRedun.statStandby;
        logger.info("became standby");
        for (;;) {
            bits.sleep(packRedun.timeKeep);
            if (findActive() < 0) {
                break;
            }
            if (con == null) {
                continue;
            }
            if (con.ready2rx() < 1) {
                continue;
            }
            byte[] buf = new byte[256];
            con.nonBlockGet(buf, 0, buf.length);
            con.linePut("this node is standby and not ready for login");
        }
        state = packRedun.statActive;
        logger.info("lost active");
    }

    /**
     * sync config to peers
     */
    public static void doConfig() {
        for (int i = 0; i < ifaces.size(); i++) {
            ifaces.get(i).doFile(cfgInit.cfgFileSw, packRedun.fnStart);
        }
    }

    /**
     * reload peers
     */
    public static void doReload() {
        for (int i = 0; i < ifaces.size(); i++) {
            ifaces.get(i).doRetry(packRedun.typReload, new packHolder(true, true));
        }
    }
    
}

class prtRedunIfc implements ifcUp {
    
    private ifcThread lower;
    
    private counter cntr = new counter();
    
    private addrMac hwaddr;
    
    private RandomAccessFile filRx;
    
    private String filNm;
    
    public String name;
    
    public boolean bidir;
    
    public int magic;
    
    public int state;
    
    public int uptime;
    
    public long heard;
    
    public int dualAct;
    
    public notifier notif = new notifier();
    
    public int ackRx;
    
    public void doInit(String nam, ifcThread thrd) {
        bidir = false;
        name = nam;
        lower = thrd;
        state = packRedun.statInit;
        magic = 0;
        heard = 0;
        uptime = 0;
        dualAct = 0;
        lower.setFilter(false);
        lower.setUpper(this);
        lower.startLoop(1);
        hwaddr = (addrMac) lower.getHwAddr();
        filNm = version.myWorkDir() + "red" + bits.randomD() + ".tmp";
    }
    
    public String doShow() {
        return name + "|" + state + "|" + bidir + "|" + magic + "|" + bits.timeDump(uptime) + "|" + bits.timePast(heard);
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
        uptime = pckP.uptime;
        heard = bits.getTime();
        switch (pckP.type) {
            case packRedun.typHello:
                if ((magic == prtRedun.magic) && (state >= prtRedun.state)) {
                    cfgInit.stopRouter(true, 6, "magic collision");
                }
                if (prtRedun.state != packRedun.statActive) {
                    dualAct = 0;
                    break;
                }
                if (state != packRedun.statActive) {
                    dualAct = 0;
                    break;
                }
                if (uptime >= prtRedun.uptime) {
                    cfgInit.stopRouter(true, 6, "dual active, peer older");
                }
                logger.warn("dual active, peer younger");
                dualAct++;
                if (dualAct < 5) {
                    break;
                }
                doPack(packRedun.typReload, new packHolder(true, true));
                break;
            case packRedun.typReload:
                if (prtRedun.state == packRedun.statActive) {
                    break;
                }
                doAck(-4);
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
                filRx = new RandomAccessFile(filNm, "rw");
                filRx.seek(0);
                filRx.setLength(0);
            } catch (Exception e) {
                logger.error("unable to open file");
                break;
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
                    logger.error("unable to write file");
                    break;
                }
                doAck(i);
                break;
            case packRedun.typFilEnd:
                try {
                filRx.close();
            } catch (Exception e) {
                logger.error("unable to close file");
                break;
            }
            filRx = null;
            String a = pck.getAsciiZ(0, packRedun.dataMax, 0);
            String b = null;
            logger.info("received file " + a);
            if (a.equals(packRedun.fnCore)) {
                b = version.getFileName();
            }
            if (a.equals(packRedun.fnStart)) {
                b = cfgInit.cfgFileSw;
            }
            if (b == null) {
                logger.error("got invalid filename");
                break;
            }
            userFlash.rename(filNm, b, true, true);
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
        pckP.uptime = prtRedun.uptime;
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
        for (int i = 0; i < 8; i++) {
            doPack(typ, pck.copyBytes(true, true));
            notif.sleep(packRedun.timeKeep);
            if (ackRx != -1) {
                return false;
            }
        }
        logger.error("peer does not respond");
        return true;
    }
    
    public boolean doFile(String fn, String rfn) {
        RandomAccessFile fr;
        long siz;
        try {
            fr = new RandomAccessFile(fn, "r");
            siz = fr.length();
        } catch (Exception e) {
            logger.error("unable to open file");
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
                logger.error("unable to read file");
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
            logger.error("unable to close file");
            return true;
        }
        packHolder pck = new packHolder(true, true);
        pck.putAsciiZ(0, packRedun.dataMax, rfn, 0);
        pck.putSkip(packRedun.dataMax);
        if (doRetry(packRedun.typFilEnd, pck)) {
            return true;
        }
        return false;
    }
    
}
