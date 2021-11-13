package net.freertr.ifc;

import java.util.List;
import net.freertr.addr.addrMac;
import net.freertr.addr.addrType;
import net.freertr.cfg.cfgInit;
import net.freertr.pack.packHolder;
import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeSide;
import net.freertr.user.userHelping;
import net.freertr.util.cmds;
import net.freertr.util.counter;
import net.freertr.util.logger;
import net.freertr.util.state;

/**
 * connect two interfaces
 *
 * @author matecsaba
 */
public class ifcHairpin {

    /**
     * interface is not ethernet
     */
    public boolean notEther;

    private ifcHairpinWorker s1;

    private ifcHairpinWorker s2;

    private pipeLine pip;

    /**
     * create new instance
     */
    public ifcHairpin() {
        s1 = new ifcHairpinWorker();
        s2 = new ifcHairpinWorker();
        s1.parent = this;
        s2.parent = this;
        pip = new pipeLine(128 * 1024, true);
        s1.queueRx = pip.getSide();
        s2.queueRx = pip.getSide();
        s1.queueTx = s1.queueRx;
        s2.queueTx = s2.queueRx;
    }

    /**
     * get side one
     *
     * @return interface handler
     */
    public ifcDn getSide1() {
        return s1;
    }

    /**
     * get side two
     *
     * @return interface handler
     */
    public ifcDn getSide2() {
        return s2;
    }

    /**
     * stop this hairpin
     */
    public void stopWork() {
        pip.setClose();
    }

    /**
     * start this hairpin
     */
    public void startWork() {
        new Thread(s1).start();
        new Thread(s2).start();
    }

    /**
     * get help text
     *
     * @param l storage
     */
    public static void getHelp(userHelping l) {
        l.add(null, "1 .  ethernet                       specify type of hairpin");
    }

    /**
     * get configuration
     *
     * @param l storage
     * @param beg beginning
     */
    public void getConfig(List<String> l, String beg) {
        cmds.cfgLine(l, notEther, beg, "ethernet", "");
    }

    /**
     * do configuration
     *
     * @param cmd command
     */
    public void doConfig(cmds cmd) {
        String s = cmd.word();
        if (s.equals("ethernet")) {
            notEther = false;
            return;
        }
        if (!s.equals("no")) {
            cmd.badCmd();
            return;
        }
        s = cmd.word();
        if (s.equals("ethernet")) {
            notEther = true;
            return;
        }
        cmd.badCmd();
    }

}

class ifcHairpinWorker implements ifcDn, Runnable {

    public ifcHairpin parent;

    public pipeSide queueRx;

    public pipeSide queueTx;

    private counter cntr = new counter();

    public addrType hwaddr = addrMac.getRandom();

    private ifcUp upper = new ifcNull();

    public counter getCounter() {
        return cntr;
    }

    public state.states getState() {
        return state.states.up;
    }

    public void setFilter(boolean promisc) {
    }

    public addrType getHwAddr() {
        return hwaddr;
    }

    public void setUpper(ifcUp server) {
        upper = server;
        upper.setParent(this);
    }

    public void closeDn() {
    }

    public void flapped() {
    }

    public void sendPack(packHolder pck) {
        if (cfgInit.booting) {
            return;
        }
        pck.merge2beg();
        if (!parent.notEther) {
            ifcEther.createETHheader(pck, false);
        }
        byte[] buf = pck.getCopy();
        queueTx.nonBlockPut(buf, 0, buf.length);
    }

    public int getMTUsize() {
        return 1500;
    }

    public long getBandwidth() {
        return 100000000;
    }

    public void doWork() {
        packHolder pck = new packHolder(true, true);
        byte[] buf = new byte[packHolder.maxHead];
        for (;;) {
            int i = queueRx.blockingGet(buf, 0, buf.length);
            if (i < 0) {
                return;
            }
            pck.clear();
            pck.putCopy(buf, 0, 0, i);
            pck.putSkip(i);
            pck.merge2beg();
            if (!parent.notEther) {
                ifcEther.parseETHheader(pck, false);
            }
            upper.recvPack(pck);
        }
    }

    public void run() {
        try {
            doWork();
        } catch (Exception e) {
            logger.exception(e);
        }
    }

}
