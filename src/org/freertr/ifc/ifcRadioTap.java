package org.freertr.ifc;

import java.util.ArrayList;
import java.util.List;
import java.util.Timer;
import java.util.TimerTask;
import org.freertr.addr.addrMac;
import org.freertr.cfg.cfgAll;
import org.freertr.pack.packHolder;
import org.freertr.tab.tabGen;
import org.freertr.util.bits;
import org.freertr.util.counter;
import org.freertr.util.logger;
import org.freertr.util.state;

/**
 * radio tap handler
 *
 * @author matecsaba
 */
public class ifcRadioTap implements ifcUp {

    /**
     * timeout value
     */
    public int timeOut = 60000;

    /**
     * log the events
     */
    public boolean logging = false;

    private ifcDn lower = new ifcNull();

    private counter cntr = new counter();

    private Timer keepTimer;

    private tabGen<ifcRadioTapNeigh> neighs = new tabGen<ifcRadioTapNeigh>();

    /**
     * create new instance
     */
    public ifcRadioTap() {
        restartTimer(false);
    }

    public String toString() {
        return "radiotap on " + lower;
    }

    public void recvPack(packHolder pck) {
        cntr.rx(pck);
        ifcEther.createETHheader(pck, false);
        if (pck.lsbGetW(0) != 0) { // version
            return;
        }
        int len = 4;
        int flg;
        for (;;) {
            if (len > pck.dataSize()) {
                return;
            }
            flg = pck.lsbGetD(len);
            len += 4;
            if ((flg & 0x80000000) == 0) {
                break;
            }
        }
        flg = pck.lsbGetD(4) & 0xff;
        int rate = 0;
        int chan = 0;
        int sign = 0;
        if ((flg & 0x1) != 0) {
            len += len & 4;
            len += 8; // tsft
        }
        if ((flg & 0x2) != 0) {
            len += 1; // flags
        }
        if ((flg & 0x4) != 0) {
            rate = pck.getByte(len) * 500;
            len += 1; // rate
        }
        if ((flg & 0x8) != 0) {
            len += len & 1;
            chan = pck.lsbGetW(len);
            len += 4; // channel
        }
        if ((flg & 0x10) != 0) {
            len += 1; // fhss
        }
        if ((flg & 0x20) != 0) {
            sign = pck.getByte(len) - 256;
            len += 1; // signal
        }
        len = pck.lsbGetW(2);
        if ((len + 22) > pck.dataSize()) {
            return;
        }
        pck.getAddr(pck.ETHsrc, len + 10);
        pck.getAddr(pck.ETHtrg, len + 4);
        ifcRadioTapNeigh nei = new ifcRadioTapNeigh(pck.ETHsrc);
        ifcRadioTapNeigh old = neighs.add(nei);
        long tim = bits.getTime();
        if (old == null) {
            if (logging) {
                logger.info("new device appeared " + nei.adr + " on " + lower);
            }
            nei.first = tim;
            old = nei;
        }
        old.last = tim;
        old.chan = chan;
        old.rate = rate;
        old.sign = sign;
    }

    public void setParent(ifcDn parent) {
        lower = parent;
    }

    public void setState(state.states stat) {
    }

    public void closeUp() {
    }

    public counter getCounter() {
        return cntr;
    }

    /**
     * setup timer thread
     *
     * @param shutdown set true to shut down
     */
    public void restartTimer(boolean shutdown) {
        try {
            keepTimer.cancel();
        } catch (Exception e) {
        }
        keepTimer = null;
        if (shutdown) {
            return;
        }
        keepTimer = new Timer();
        ifcRadioTapPurge task = new ifcRadioTapPurge(this);
        keepTimer.schedule(task, 500, 15000);
    }

    /**
     * purge old peers
     */
    protected void purgePeers() {
        long tim = bits.getTime();
        for (int i = neighs.size() - 1; i >= 0; i--) {
            ifcRadioTapNeigh ntry = neighs.get(i);
            if ((tim - ntry.last) < timeOut) {
                continue;
            }
            if (logging) {
                logger.info("device disappeared " + ntry.adr + " on " + lower);
            }
            neighs.del(ntry);
        }
    }

    /**
     * get show output
     *
     * @return list of neighbors
     */
    public List<String> getShNeigh() {
        List<String> res = new ArrayList<String>();
        for (int i = 0; i < neighs.size(); i++) {
            ifcRadioTapNeigh ntry = neighs.get(i);
            res.add(ntry.adr + "|" + ntry.rate + "|" + ntry.chan + "|" + ntry.sign + "|" + bits.timePast(ntry.first) + "|" + bits.time2str(cfgAll.timeZoneName, ntry.first + cfgAll.timeServerOffset, 3));
        }
        return res;
    }

}

class ifcRadioTapPurge extends TimerTask {

    private ifcRadioTap lower;

    public ifcRadioTapPurge(ifcRadioTap parent) {
        lower = parent;
    }

    public void run() {
        try {
            lower.purgePeers();
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

}

class ifcRadioTapNeigh implements Comparable<ifcRadioTapNeigh> {

    public final addrMac adr;

    public int rate;

    public int chan;

    public int sign;

    public long first;

    public long last;

    public ifcRadioTapNeigh(addrMac clnt) {
        adr = clnt.copyBytes();
    }

    public int compareTo(ifcRadioTapNeigh o) {
        return adr.compareTo(o.adr);
    }

}
