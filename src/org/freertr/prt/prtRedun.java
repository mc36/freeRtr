package org.freertr.prt;

import java.io.File;
import java.io.RandomAccessFile;
import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrMac;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgInit;
import org.freertr.ifc.ifcDn;
import org.freertr.ifc.ifcThread;
import org.freertr.ifc.ifcUp;
import org.freertr.util.cmds;
import org.freertr.pack.packHolder;
import org.freertr.pack.packRedundancy;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSetting;
import org.freertr.pipe.pipeSide;
import org.freertr.user.userExec;
import org.freertr.user.userFlash;
import org.freertr.user.userFormat;
import org.freertr.user.userRead;
import org.freertr.user.userUpgrade;
import org.freertr.util.bits;
import org.freertr.util.counter;
import org.freertr.util.debugger;
import org.freertr.util.logger;
import org.freertr.util.notifier;
import org.freertr.util.state;
import org.freertr.util.syncInt;

/**
 * redundancy protocol
 *
 * @author matecsaba
 */
public class prtRedun implements Runnable {

    /**
     * create instance
     */
    public prtRedun() {
    }

    /**
     * my magic number
     */
    protected static int magic = (bits.randomD() & 0x3fffffff) + 1;

    /**
     * current state
     */
    protected static int state = packRedundancy.statInit;

    /**
     * current uptime
     */
    protected static int uptime = 0;

    /**
     * current startup
     */
    protected static long started = 0;

    private final static List<prtRedunIfc> ifaces = new ArrayList<prtRedunIfc>();

    public void run() {
        try {
            for (;;) {
                sendHellos();
                bits.sleep(cfgAll.redundancyKeep);
            }
        } catch (Exception e) {
            logger.exception(e);
        }
    }

    private void start() {
        logger.startThread(this);
    }

    private static void sendHellos() {
        packHolder pck = new packHolder(true, true);
        long tim = bits.getTime();
        uptime = (int) ((tim - started) / 1000);
        for (int i = 0; i < ifaces.size(); i++) {
            pck.clear();
            prtRedunIfc ifc = ifaces.get(i);
            ifc.doPack(packRedundancy.typHello, pck);
            if ((tim - ifc.heard) < cfgAll.redundancyHold) {
                continue;
            }
            if (ifc.reach.set(0) != 0) {
                logger.error("peer down on " + ifc);
                ifc.changes++;
            }
        }
    }

    /**
     * get list of interfaces
     *
     * @return list
     */
    public static List<String> getIfacesLst() {
        List<String> res = new ArrayList<String>();
        for (int i = 0; i < ifaces.size(); i++) {
            res.add(ifaces.get(i).name);
        }
        return res;
    }

    /**
     * generate show output
     *
     * @return output
     */
    public static userFormat doShowStatus() {
        userFormat l = new userFormat("|", "iface|reach|state|prio|uptime|changes|magic|heard");
        l.add("self|-|" + packRedundancy.stat2str(state) + "|" + cfgInit.redunPrio + "|" + bits.timeDump(uptime) + "|-|" + bits.padBeg(bits.toHexD(magic), 8, "0") + "|-");
        for (int i = 0; i < ifaces.size(); i++) {
            prtRedunIfc ifc = ifaces.get(i);
            l.add(ifc.name + "|" + ifc.reach + "|" + packRedundancy.stat2str(ifc.last.state) + "|" + ifc.last.priority + "|" + bits.timeDump(ifc.last.uptime) + "|" + ifc.changes + "|" + bits.padBeg(bits.toHexD(ifc.last.magic), 8, "0") + "|" + bits.timePast(ifc.heard));
        }
        return l;
    }

    /**
     * generate show output
     *
     * @return output
     */
    public static userFormat doShowDescr() {
        userFormat l = new userFormat("|", "iface|reach|state|descr");
        l.add("self|-|" + packRedundancy.stat2str(state) + "|" + cfgInit.prntNam);
        for (int i = 0; i < ifaces.size(); i++) {
            prtRedunIfc ifc = ifaces.get(i);
            l.add(ifc.name + "|" + ifc.reach + "|" + packRedundancy.stat2str(ifc.last.state) + "|" + ifc.descr);
        }
        return l;
    }

    /**
     * generate show output
     *
     * @param fn filename
     * @return output
     */
    public static userFormat doShowHash(String fn) {
        userFormat l = new userFormat("|", "iface|reach|state|match|hash");
        String mine = getFileHash(wireName2fileName(fn));
        l.add("self|-|" + packRedundancy.stat2str(state) + "|-|" + mine);
        for (int i = 0; i < ifaces.size(); i++) {
            prtRedunIfc ifc = ifaces.get(i);
            String got = ifc.doHash(fn);
            if (got == null) {
                got = "nothing";
            }
            l.add(ifc.name + "|" + ifc.reach + "|" + packRedundancy.stat2str(ifc.last.state) + "|" + mine.equals(got) + "|" + got);
        }
        return l;
    }

    /**
     * generate show output
     *
     * @param cmd command
     * @return output
     */
    public static List<String> doShowCmd(String cmd) {
        List<String> l = new ArrayList<String>();
        for (int i = 0; i < ifaces.size(); i++) {
            prtRedunIfc ifc = ifaces.get(i);
            l.add("");
            l.add(cmds.errbeg + " command " + cmd + " on " + ifc.name + ", " + ifc.descr + ":");
            List<String> got = ifc.doCmd(cmd);
            if (got == null) {
                l.add("nothing");
                continue;
            }
            l.addAll(got);
        }
        return l;
    }

    /**
     * get file hash
     *
     * @param fn filename
     * @return hash or comment started error
     */
    public static String getFileHash(String fn) {
        if (fn == null) {
            return cmds.comment + "invalid filename";
        }
        if (!new File(fn).exists()) {
            return cmds.comment + "file not found";
        }
        String a = userUpgrade.calcFileHash(fn);
        if (a != null) {
            return a;
        }
        return cmds.comment + "got no hash";
    }

    /**
     * add one physical interface
     *
     * @param name name of interface
     * @param thrd interface thread handler
     * @param desc description
     */
    public static void ifcAdd(String name, ifcThread thrd, String desc) {
        prtRedunIfc ifc = new prtRedunIfc();
        ifc.doInit(name, thrd, desc);
        ifaces.add(ifc);
    }

    /**
     * fill in generic parts
     *
     * @return get self packet
     */
    public static packRedundancy getSelf() {
        packRedundancy pckP = new packRedundancy();
        pckP.state = prtRedun.state;
        pckP.magic = prtRedun.magic;
        pckP.uptime = prtRedun.uptime;
        pckP.priority = cfgInit.redunPrio;
        return pckP;
    }

    /**
     * find best peer
     *
     * @return best peer index, -1 if none
     */
    public static int findActive() {
        packRedundancy bst = getSelf();
        int idx = -1;
        for (int i = 0; i < ifaces.size(); i++) {
            prtRedunIfc ifc = ifaces.get(i);
            if (ifc.reach.get() != 3) {
                continue;
            }
            if (ifc.last.state == packRedundancy.statActive) {
                return i;
            }
            String a = bst.otherBetter(ifc.last);
            if (a == null) {
                continue;
            }
            idx = i;
            bst = ifc.last;
        }
        return idx;
    }

    /**
     * handle console
     *
     * @param con console
     * @param act active
     */
    public static void handleConsole(pipeSide con, int act) {
        if (con == null) {
            return;
        }
        if (con.ready2rx() < 1) {
            return;
        }
        byte[] buf = new byte[256];
        con.nonBlockGet(buf, 0, buf.length);
        con.linePut("this node is standby, active on " + ifaces.get(act));
    }

    /**
     * terminate the redundancy
     */
    public static void doShut() {
    }

    /**
     * get hardware forwarder
     *
     * @return offload info
     */
    public static String getShGenOneLiner() {
        if (ifaces.size() < 1) {
            return "";
        }
        return "redun,";
    }

    /**
     * initialize the redundancy
     *
     * @param con console
     */
    public static void doInit(pipeSide con) {
        if (ifaces.size() < 1) {
            state = packRedundancy.statActive;
            return;
        }
        logger.info("initializing redundancy");
        started = bits.getTime();
        state = packRedundancy.statSpeak;
        new prtRedun().start();
        bits.sleep(cfgAll.redundancyInit);
        int act = findActive();
        if (act < 0) {
            state = packRedundancy.statActive;
            sendHellos();
            logger.info("became active");
            return;
        }
        ifaces.get(act).doXfer(packRedundancy.fnState);
        state = packRedundancy.statStandby;
        sendHellos();
        logger.info("became standby, active on " + ifaces.get(act));
        for (;;) {
            act = findActive();
            if (act < 0) {
                break;
            }
            handleConsole(con, act);
            bits.sleep(cfgAll.redundancyKeep);
            if ((bits.getTime() - started) < cfgAll.redundancyTake) {
                continue;
            }
            String a = ifaces.get(act).last.otherBetter(getSelf());
            if (a == null) {
                break;
            }
            state = packRedundancy.statActive;
            sendHellos();
            logger.info("preempting over " + ifaces.get(act) + " because won on " + a);
            return;
        }
        for (;;) {
            act = findActive();
            if (act < 0) {
                break;
            }
            handleConsole(con, act);
            bits.sleep(cfgAll.redundancyKeep);
        }
        state = packRedundancy.statActive;
        sendHellos();
        logger.info("lost active after " + bits.timeDump(uptime));
    }

    private static prtRedunIfc findIface(String ifc) {
        for (int i = 0; i < ifaces.size(); i++) {
            prtRedunIfc cur = ifaces.get(i);
            if (ifc.equals(cur.name)) {
                return cur;
            }
        }
        return null;
    }

    /**
     * set local priority
     *
     * @param pri priority to use
     */
    public static void setPrio(int pri) {
        cfgInit.redunPrio = pri;
    }

    /**
     * set peer priority
     *
     * @param ifc name of interface
     * @param pri priority to use
     * @return true on error, false on success
     */
    public static boolean setPrio(String ifc, int pri) {
        prtRedunIfc fnd = findIface(ifc);
        if (fnd == null) {
            return true;
        }
        return fnd.doPrio(pri);
    }

    /**
     * sync config from peer
     *
     * @param ifc name of interface
     * @return true on error, false on success
     */
    public static boolean doConfig(String ifc) {
        prtRedunIfc fnd = findIface(ifc);
        if (fnd == null) {
            return true;
        }
        return fnd.doXfer(packRedundancy.fnStart);
    }

    /**
     * sync config to peers
     */
    public static void doConfig() {
        for (int i = 0; i < ifaces.size(); i++) {
            ifaces.get(i).doFile(cfgInit.cfgFileSw, packRedundancy.fnStart);
        }
    }

    /**
     * sync config from peer
     *
     * @param ifc name of interface
     * @return true on error, false on success
     */
    public static boolean doCore(String ifc) {
        prtRedunIfc fnd = findIface(ifc);
        if (fnd == null) {
            return true;
        }
        return fnd.doXfer(packRedundancy.fnCore);
    }

    /**
     * sync core to peers
     */
    public static void doCore() {
        for (int i = 0; i < ifaces.size(); i++) {
            ifaces.get(i).doFile(cfgInit.getFileName(), packRedundancy.fnCore);
        }
    }

    /**
     * sync state to peers
     */
    public static void doState() {
        for (int i = 0; i < ifaces.size(); i++) {
            ifaces.get(i).doFile(cfgInit.myStateFile(), packRedundancy.fnState);
        }
    }

    /**
     * reload peers
     */
    public static void doReload() {
        for (int i = 0; i < ifaces.size(); i++) {
            ifaces.get(i).doRetry(packRedundancy.typReload, new packHolder(true, true));
        }
    }

    /**
     * convert wire name to file name
     *
     * @param s wire name
     * @return file name
     */
    public static String wireName2fileName(String s) {
        if (s.equals(packRedundancy.fnCore)) {
            return cfgInit.getFileName();
        }
        if (s.equals(packRedundancy.fnState)) {
            return cfgInit.myStateFile();
        }
        if (s.equals(packRedundancy.fnStart)) {
            return cfgInit.cfgFileSw;
        }
        return null;
    }

}

class prtRedunIfc implements ifcUp {

    private ifcThread lower;

    private counter cntr = new counter();

    private addrMac hwaddr;

    private RandomAccessFile filRx;

    private String filNm;

    public String name;

    public String descr;

    public String lastFileHash;

    public final syncInt reach = new syncInt(0);

    public packRedundancy last = new packRedundancy();

    public long heard;

    public int changes;

    public int dualAct;

    public notifier notif = new notifier();

    public int ackRx;

    public String toString() {
        return "" + name;
    }

    public void doInit(String nam, ifcThread thrd, String desc) {
        reach.set(0);
        name = nam;
        descr = desc;
        lower = thrd;
        last.state = packRedundancy.statInit;
        heard = 0;
        dualAct = 0;
        lower.setFilter(false);
        lower.setUpper(this);
        lower.startLoop(1);
        hwaddr = (addrMac) lower.getHwAddr();
        filNm = cfgInit.getRWpath() + "red" + bits.randomD() + userUpgrade.tmpExt;
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
        packRedundancy pckP = new packRedundancy();
        if (pckP.parseHeader(pck)) {
            return;
        }
        if (debugger.prtRedun) {
            logger.debug("rx " + pckP);
        }
        last = pckP;
        if (pckP.peer == prtRedun.magic) {
            if (reach.set(3) != 3) {
                logger.warn("peer up on " + name);
                changes++;
            }
        } else {
            if (reach.set(1) >= 1) {
                logger.error("echo mismatch on " + name);
            }
        }
        heard = bits.getTime();
        switch (pckP.type) {
            case packRedundancy.typHello:
                if ((last.magic == prtRedun.magic) && (last.state >= prtRedun.state)) {
                    cfgInit.stopRouter(true, 6, "magic collision");
                }
                if (prtRedun.state != packRedundancy.statActive) {
                    dualAct = 0;
                    break;
                }
                if (last.state != packRedundancy.statActive) {
                    dualAct = 0;
                    break;
                }
                String a = prtRedun.getSelf().otherBetter(pckP);
                if (a != null) {
                    cfgInit.stopRouter(true, 9, "dual active, reloading because lost on " + a);
                }
                dualAct++;
                if (dualAct < 5) {
                    break;
                }
                logger.warn("dual active, reloading peer");
                doPack(packRedundancy.typReload, new packHolder(true, true));
                break;
            case packRedundancy.typReload:
                if (prtRedun.state == packRedundancy.statActive) {
                    break;
                }
                doAck(-4);
                cfgInit.stopRouter(true, 10, "peer request");
                break;
            case packRedundancy.typAck:
                ackRx = pck.msbGetD(0);
                notif.wakeup();
                break;
            case packRedundancy.typFilBeg:
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
            case packRedundancy.typFilDat:
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
            case packRedundancy.typFilEnd:
                try {
                    filRx.close();
                } catch (Exception e) {
                    logger.error("unable to close file");
                    break;
                }
                filRx = null;
                a = pck.getAsciiZ(0, packRedundancy.dataMax, 0);
                if (a.equals(packRedundancy.fnShow)) {
                    lastFileHash = filNm;
                    doAck(-3);
                    break;
                }
                String b = prtRedun.wireName2fileName(a);
                if (b == null) {
                    logger.error("got invalid filename");
                    break;
                }
                logger.info("received file " + a + " as " + b);
                userFlash.copy(filNm, b, true);
                userFlash.delete(filNm);
                doAck(-3);
                break;
            case packRedundancy.typSumReq:
                a = pck.getAsciiZ(0, packRedundancy.dataMax, 0);
                b = prtRedun.wireName2fileName(a);
                if (b == null) {
                    logger.error("got invalid filename");
                    break;
                }
                logger.info("hash file " + a + " as " + b);
                b = prtRedun.getFileHash(b);
                if (b.startsWith(cmds.comment)) {
                    b = b.substring(1, b.length());
                    logger.info(b);
                }
                pck.clear();
                pck.putAsciiZ(0, packRedundancy.dataMax, b, 0);
                pck.putSkip(packRedundancy.dataMax);
                doPack(packRedundancy.typSumVal, pck);
                break;
            case packRedundancy.typSumVal:
                lastFileHash = pck.getAsciiZ(0, packRedundancy.dataMax, 0);
                break;
            case packRedundancy.typSetPri:
                cfgInit.redunPrio = pck.msbGetD(0);
                logger.info("priority changed to " + cfgInit.redunPrio);
                doAck(-5);
                break;
            case packRedundancy.typExecCmd:
                a = pck.getAsciiZ(0, packRedundancy.dataMax, 0);
                logger.info("exec command " + a);
                doAck(-6);
                new prtRedunExec(this, a);
                break;
            case packRedundancy.typXferReq:
                a = pck.getAsciiZ(0, packRedundancy.dataMax, 0);
                b = prtRedun.wireName2fileName(a);
                if (b == null) {
                    logger.error("got invalid filename");
                    break;
                }
                logger.info("transfer request " + a + " as " + b);
                doAck(-7);
                new prtRedunXfer(this, b, a);
                break;
        }
    }

    public void doPack(int typ, packHolder pckB) {
        pckB.merge2beg();
        packRedundancy pckP = prtRedun.getSelf();
        pckP.type = typ;
        pckP.peer = last.magic;
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
        doPack(packRedundancy.typAck, pck);
    }

    public boolean doRetry(int typ, packHolder pck) {
        ackRx = -1;
        for (int i = 0; i < 8; i++) {
            doPack(typ, pck.copyBytes(true, true));
            notif.sleep(cfgAll.redundancyKeep);
            if (ackRx != -1) {
                return false;
            }
        }
        logger.error("peer does not respond");
        return true;
    }

    public List<String> doCmd(String fn) {
        packHolder pck = new packHolder(true, true);
        pck.putAsciiZ(0, packRedundancy.dataMax, fn, 0);
        pck.putSkip(packRedundancy.dataMax);
        doPack(packRedundancy.typExecCmd, pck);
        for (int i = 0; i < 10; i++) {
            bits.sleep(cfgAll.redundancyHold / 10);
            if (lastFileHash != null) {
                break;
            }
        }
        if (lastFileHash == null) {
            return bits.str2lst("timeout getting show");
        }
        List<String> res = bits.txt2buf(lastFileHash);
        userFlash.delete(lastFileHash);
        lastFileHash = null;
        if (res == null) {
            return bits.str2lst("error reading show");
        }
        return res;
    }

    public String doHash(String fn) {
        lastFileHash = null;
        packHolder pck = new packHolder(true, true);
        pck.putAsciiZ(0, packRedundancy.dataMax, fn, 0);
        pck.putSkip(packRedundancy.dataMax);
        doPack(packRedundancy.typSumReq, pck);
        for (int i = 0; i < 10; i++) {
            bits.sleep(cfgAll.redundancyHold / 10);
            if (lastFileHash != null) {
                break;
            }
        }
        if (lastFileHash == null) {
            return "timeout getting hash";
        }
        String a = lastFileHash;
        lastFileHash = null;
        return a;
    }

    private long getFileTime(String s) {
        try {
            return new File(s).lastModified();
        } catch (Exception e) {
            return -1;
        }
    }

    public boolean doXfer(String fn) {
        String b = prtRedun.wireName2fileName(fn);
        if (b == null) {
            return true;
        }
        logger.info("requesting file " + fn + " as " + b);
        long lst = getFileTime(b);
        packHolder pck = new packHolder(true, true);
        pck.putAsciiZ(0, packRedundancy.dataMax, fn, 0);
        pck.putSkip(packRedundancy.dataMax);
        doPack(packRedundancy.typXferReq, pck);
        for (int i = 0; i < 10; i++) {
            bits.sleep(cfgAll.redundancyInit / 10);
            if (lst != getFileTime(b)) {
                return false;
            }
        }
        return true;
    }

    public boolean doPrio(int pri) {
        packHolder pck = new packHolder(true, true);
        pck.msbPutD(0, pri);
        pck.putSkip(4);
        if (doRetry(packRedundancy.typSetPri, pck)) {
            return true;
        }
        return false;
    }

    public boolean doFile(String fn, String rfn) {
        logger.info("syncing " + fn + " as " + rfn);
        RandomAccessFile fr;
        long siz = -1;
        try {
            fr = new RandomAccessFile(fn, "r");
        } catch (Exception e) {
            logger.error("unable to open file");
            return true;
        }
        try {
            siz = fr.length();
        } catch (Exception e) {
            logger.error("unable to get file size");
        }
        if (siz < 0) {
            try {
                fr.close();
            } catch (Exception e) {
            }
            return true;
        }
        if (doRetry(packRedundancy.typFilBeg, new packHolder(true, true))) {
            try {
                fr.close();
            } catch (Exception e) {
            }
            return true;
        }
        long pos = 0;
        for (; pos < siz;) {
            long rndl = siz - pos;
            if (rndl > packRedundancy.dataMax) {
                rndl = packRedundancy.dataMax;
            }
            int rndi = (int) rndl;
            byte[] buf = new byte[rndi];
            try {
                fr.read(buf, 0, rndi);
            } catch (Exception e) {
                logger.error("unable to read file");
                siz = -1;
                break;
            }
            packHolder pck = new packHolder(true, true);
            pck.msbPutD(0, (int) pos);
            pck.msbPutW(4, buf.length);
            pck.putSkip(6);
            pos += rndl;
            pck.putCopy(buf, 0, 0, buf.length);
            pck.putSkip(buf.length);
            if (doRetry(packRedundancy.typFilDat, pck)) {
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
        if (siz < 0) {
            return true;
        }
        packHolder pck = new packHolder(true, true);
        pck.putAsciiZ(0, packRedundancy.dataMax, rfn, 0);
        pck.putSkip(packRedundancy.dataMax);
        if (doRetry(packRedundancy.typFilEnd, pck)) {
            return true;
        }
        return false;
    }

}

class prtRedunExec implements Runnable {

    public final prtRedunIfc ifc;

    public final String cmd;

    public prtRedunExec(prtRedunIfc i, String a) {
        ifc = i;
        cmd = a;
        logger.startThread(this);
    }

    public void run() {
        pipeLine pl = new pipeLine(1024 * 1024, false);
        pipeSide pip = pl.getSide();
        pip.lineTx = pipeSide.modTyp.modeCRLF;
        pip.lineRx = pipeSide.modTyp.modeCRorLF;
        userRead rdr = new userRead(pip, null);
        pip.settingsPut(pipeSetting.height, 0);
        userExec exe = new userExec(pip, rdr);
        exe.privileged = false;
        pip.setTime(120000);
        String a = exe.repairCommand(cmd);
        exe.executeCommand(a);
        pip = pl.getSide();
        pl.setClose();
        pip.lineTx = pipeSide.modTyp.modeCRLF;
        pip.lineRx = pipeSide.modTyp.modeCRtryLF;
        List<String> txt = new ArrayList<String>();
        for (;;) {
            if (pip.ready2rx() < 1) {
                break;
            }
            a = pip.lineGet(1);
            if (a.length() < 1) {
                continue;
            }
            txt.add(a);
        }
        a = cfgInit.getRWpath() + "exe" + bits.randomD() + userUpgrade.tmpExt;
        if (bits.buf2txt(true, txt, a)) {
            logger.error("unable to save file");
            userFlash.delete(a);
            return;
        }
        txt.clear();
        ifc.doFile(a, packRedundancy.fnShow);
        userFlash.delete(a);
    }

}

class prtRedunXfer implements Runnable {

    public final prtRedunIfc ifc;

    public final String fn;

    public final String rfn;

    public prtRedunXfer(prtRedunIfc i, String a, String b) {
        ifc = i;
        fn = a;
        rfn = b;
        logger.startThread(this);
    }

    public void run() {
        ifc.doFile(fn, rfn);
    }

}
