package ifc;

import addr.addrMac;
import addr.addrType;
import cfg.cfgAll;
import cfg.cfgIfc;
import cfg.cfgInit;
import java.io.File;
import java.io.RandomAccessFile;
import pack.packHolder;
import user.userFormat;
import util.bits;
import util.counter;
import util.debugger;
import util.logger;
import util.state;
import util.syncInt;

/**
 * one interface handler thread
 *
 * @author matecsaba
 */
public abstract class ifcThread implements ifcDn, Runnable {

    /**
     * worker interface that gets the packets from this thread
     */
    protected ifcUp upper = new ifcNull();

    /**
     * hardware address of interface
     */
    protected addrType hwaddr;

    /**
     * current state of interface
     */
    protected state.states lastState = state.states.up;

    /**
     * have state packets
     */
    protected boolean haveState = false;

    /**
     * counter of this interface
     */
    protected counter cntr = new counter();

    /**
     * set to true if ethernet encapsulation in use
     */
    protected boolean etherEnc = false;

    /**
     * set true for redundancy links
     */
    public boolean booter = false;

    private int procRun = 0;

    private syncInt procNow = new syncInt(0);

    private int procCnt = 0;

    private int procLst = -1;

    private long procTim = 0;

    private RandomAccessFile logFile = null;

    private Thread started[] = new Thread[0];

    private boolean need2run = true;

    /**
     * get counter
     *
     * @return counter
     */
    public counter getCounter() {
        return cntr;
    }

    /**
     * check for stalled interfaces
     */
    public static void checkIfaces() {
        if (ifcEthTyp.loopDrops > 0) {
            logger.info(ifcEthTyp.loopDrops + " looping packets dropped");
            ifcEthTyp.loopDrops = 0;
        }
        String s = "";
        for (int i = cfgAll.ifaces.size() - 1; i >= 0; i--) {
            cfgIfc ntry = cfgAll.ifaces.get(i);
            if (ntry == null) {
                continue;
            }
            ntry.ethtyp.getHistory().update(ntry.ethtyp.getCounter());
            if (ntry.thread == null) {
                continue;
            }
            if (!ntry.thread.checkStalled()) {
                continue;
            }
            s += ntry + "=" + ntry.thread.stallPoint() + " ";
        }
        if (s.length() < 1) {
            return;
        }
        if (cfgInit.noStallCheck) {
            logger.debug(s + " stalled!");
            return;
        }
        cfgInit.stopRouter(false, 7, s + "stalled!");
    }

    /**
     * show interface stalls
     *
     * @return show output
     */
    public static userFormat showStalls() {
        userFormat res = new userFormat("|", "iface|pack|last|cfg|run|busy|time");
        for (int i = 0; i < cfgAll.ifaces.size(); i++) {
            cfgIfc ntry = cfgAll.ifaces.get(i);
            if (ntry.thread == null) {
                continue;
            }
            res.add(ntry.name + "|" + ntry.thread.procCnt + "|" + ntry.thread.procLst + "|" + ntry.thread.started.length + "|" + ntry.thread.procRun + "|" + ntry.thread.procNow + "|" + bits.timePast(ntry.thread.procTim));
        }
        return res;
    }

    /**
     * dump of stall point
     *
     * @return decoded stack trace
     */
    protected String stallPoint() {
        String s = "";
        for (int i = 0; i < started.length; i++) {
            try {
                StackTraceElement[] st = started[i].getStackTrace();
                s += logger.dumpStackTrace(st) + " ";
            } catch (Exception e) {
            }
        }
        return s.trim();
    }

    /**
     * check for watchdog
     *
     * @return true if interface stalled
     */
    public boolean checkStalled() {
        if (procRun != started.length) {
            return true;
        }
        long t = bits.getTime();
        if (procLst != procCnt) {
            procLst = procCnt;
            procTim = t;
            return false;
        }
        if (procNow.get() == 0) {
            procTim = t;
            return false;
        }
        return (t - procTim) > 10000;
    }

    /**
     * receive one packet from remote wait until it arrives
     *
     * @param buf buffer to write to
     * @param ofs offset in buffer
     * @return useful bytes in buffer
     * @throws java.lang.Exception
     */
    public abstract int rxOnePack(byte[] buf, int ofs) throws Exception;

    /**
     * send one packet to remote
     *
     * @param buf buffer to send
     * @param ofs offset in buffer
     * @param len useful bytes in buffer
     * @throws java.lang.Exception
     */
    public abstract void txOnePack(byte[] buf, int ofs, int len) throws Exception;

    /**
     * close the connection
     *
     * @throws java.lang.Exception
     */
    public abstract void rxtxClose() throws Exception;

    /**
     * get hardware address
     *
     * @return hw address
     */
    public addrType getHwAddr() {
        return hwaddr;
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
     * get state of interface
     *
     * @return state of line protocol
     */
    public state.states getState() {
        return lastState;
    }

    /**
     * change happened in my state
     *
     * @param stat new state
     */
    protected void setState(state.states stat) {
        if (lastState == stat) {
            return;
        }
        upper.setState(stat);
        cntr.stateChange(stat);
        lastState = stat;
    }

    /**
     * signal that upper going to terminate
     */
    public void closeDn() {
        need2run = false;
        try {
            setState(state.states.down);
        } catch (Exception e) {
        }
        lastState = state.states.close;
        try {
            bits.sleep(100);
        } catch (Exception e) {
        }
        try {
            upper.closeUp();
        } catch (Exception e) {
        }
        upper = null;
        try {
            rxtxClose();
        } catch (Exception e) {
        }
    }

    /**
     * send this packet
     *
     * @param pck packet to send
     */
    public void sendPack(packHolder pck) {
        if (cfgInit.booting && (!booter)) {
            return;
        }
        cntr.tx(pck);
        if (lastState != state.states.up) {
            cntr.drop(pck, counter.reasons.notUp);
            return;
        }
        if (etherEnc) {
            pck.merge2beg();
            pck.putAddr(0, pck.ETHtrg);
            pck.putAddr(addrMac.size, pck.ETHsrc);
            pck.putSkip(addrMac.sizeX2);
            pck.merge2beg();
            int i = 48 - pck.dataSize();
            if (i > 0) {
                pck.merge2beg();
                pck.putFill(0, i, 0);
                pck.putSkip(i);
                pck.merge2end();
            }
        }
        if (debugger.ifcThread) {
            logger.debug(this + " tx" + pck.dump());
        }
        if (logFile != null) {
            try {
                logFile.write(pck.convertToPcap(bits.getTime() + cfgAll.timeServerOffset, false));
            } catch (Exception e) {
            }
        }
        try {
            txOnePack(pck.getDataArray(), pck.dataOffset(), pck.dataSize());
        } catch (Exception e) {
        }
    }

    /**
     * the receiver loop it never exists, just when some error happened
     */
    public void doRecvLoop() {
        if (debugger.ifcThread) {
            logger.debug("started, addr=" + hwaddr);
        }
        doRounds();
        if (debugger.ifcThread) {
            logger.debug("stopped, addr=" + hwaddr);
        }
    }

    private void doRounds() {
        packHolder pck = new packHolder(true, true);
        for (;;) {
            if (!need2run) {
                return;
            }
            pck.clear();
            pck.setDataSize(1024);
            pck.setBytesLeft(0);
            try {
                pck.setDataSize(rxOnePack(pck.getDataArray(), pck.dataOffset()));
            } catch (Exception e) {
                continue;
            }
            if (cfgInit.booting && (!booter)) {
                continue;
            }
            if (haveState && (pck.dataSize() == 1)) {
                if (pck.getByte(0) == 0) {
                    setState(state.states.down);
                } else {
                    setState(state.states.up);
                }
                continue;
            }
            cntr.rx(pck);
            if (lastState != state.states.up) {
                cntr.drop(pck, counter.reasons.notUp);
                continue;
            }
            if (logFile != null) {
                try {
                    logFile.write(pck.convertToPcap(bits.getTime() + cfgAll.timeServerOffset, false));
                } catch (Exception e) {
                }
            }
            if (etherEnc) {
                try {
                    ifcEther.parseETHheader(pck, false);
                } catch (Exception e) {
                    continue;
                }
            }
            if (debugger.ifcThread) {
                logger.debug(this + " rx" + pck.dump());
            }
            procNow.add(+1);
            procCnt++;
            try {
                upper.recvPack(pck);
            } catch (Exception e) {
                logger.exception(e);
            }
            procNow.add(-1);
        }
    }

    /**
     * run the worker
     */
    public void run() {
        procRun++;
        try {
            doRecvLoop();
        } catch (Exception e) {
            logger.exception(e);
        }
        procRun--;
    }

    /**
     * start this interface receiver loop in background
     *
     * @param thrd worker threads
     */
    public void startLoop(int thrd) {
        if (thrd < 1) {
            thrd = 1;
        }
        started = new Thread[thrd];
        for (int i = 0; i < started.length; i++) {
            started[i] = new Thread(this);
            started[i].start();
        }
    }

    /**
     * reinit file logger
     *
     * @param s name of capture file
     * @return false if successful, true if error happened
     */
    public boolean initLog(String s) {
        try {
            logFile.close();
        } catch (Exception e) {
        }
        if (logFile != null) {
            logFile = null;
            return true;
        }
        try {
            RandomAccessFile f = new RandomAccessFile(new File(s), "rw");
            f.setLength(0);
            f.write(packHolder.getPcapHeader(etherEnc ? 1 : 9));
            logFile = f;
        } catch (Exception e) {
        }
        return logFile == null;
    }

}
