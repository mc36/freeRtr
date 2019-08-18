package ifc;

import addr.addrEmpty;
import addr.addrIPv4;
import addr.addrIPv6;
import addr.addrType;
import cfg.cfgIfc;
import java.util.List;
import java.util.Timer;
import java.util.TimerTask;
import pack.packHolder;
import user.userHelping;
import util.bits;
import util.cmds;
import util.counter;
import util.debugger;
import util.logger;
import util.state;

/**
 * serial encapsulation protocol
 *
 * @author matecsaba
 */
public class ifcSep implements ifcUp, ifcDn {

    /**
     * ethertype
     */
    public final static int ethTyp = 0xfa52;

    /**
     * address request
     */
    public final static int typReq = 1;

    /**
     * address reply
     */
    public final static int typRep = 2;

    /**
     * keepalive
     */
    public final static int typKeep = 3;

    /**
     * reset request
     */
    public final static int typRst = 4;

    /**
     * last known state
     */
    public state.states lastState = state.states.down;

    /**
     * counter of this interface
     */
    public counter cntr = new counter();

    /**
     * current keepalive interval (0=disabled)
     */
    public int keepaliveInterval = 5;

    /**
     * server that handler received packets
     */
    public ifcUp upper = new ifcNull();

    /**
     * server that sends our packets
     */
    public ifcDn lower = new ifcNull();

    /**
     * remote ipv4 address
     */
    public addrIPv4 rem4addr;

    /**
     * remote ipv6 address
     */
    public addrIPv6 rem6addr;

    /**
     * local ipv4 address
     */
    public addrIPv4 loc4addr;

    /**
     * local ipv6 address
     */
    public addrIPv6 loc6addr;

    /**
     * config class
     */
    public cfgIfc cfger;

    /**
     * mode: 0=peer, 1=server, 2=client
     */
    public int oprMode;

    private Timer keepTimer;

    private int curMode; // 0=addreq, 1=addrep, 2=keepalive

    private int echoSent;

    /**
     * type to string
     *
     * @param i type
     * @return string
     */
    public static String type2str(int i) {
        switch (i) {
            case typReq:
                return "req";
            case typRep:
                return "rep";
            case typKeep:
                return "keep";
            case typRst:
                return "rst";
            default:
                return "unknown=" + i;
        }
    }

    public counter getCounter() {
        return cntr;
    }

    public void setParent(ifcDn parent) {
        lower = parent;
    }

    public void closeUp() {
        setState(state.states.close);
        upper.closeUp();
    }

    public void closeDn() {
        setState(state.states.close);
        lower.closeDn();
    }

    public void flapped() {
        clearState();
    }

    /**
     * set upper layer
     *
     * @param server upper layer
     */
    public void setUpper(ifcUp server) {
        upper = server;
        upper.setParent(this);
        setState(lastState);
    }

    public state.states getState() {
        return lastState;
    }

    public void setFilter(boolean promisc) {
    }

    public addrType getHwAddr() {
        return new addrEmpty();
    }

    public void setState(state.states stat) {
        stat = state.toForceable(stat);
        if (checkPeerState(stat)) {
            return;
        }
        restartTimer(false);
    }

    public int getMTUsize() {
        return lower.getMTUsize();
    }

    public long getBandwidth() {
        return lower.getBandwidth();
    }

    /**
     * create new instance
     */
    public ifcSep() {
        clearState();
        restartTimer(false);
    }

    public String toString() {
        return "sep on " + lower;
    }

    /**
     * get help text
     *
     * @param l storage
     */
    public static void getHelp(userHelping l) {
        l.add("2 3     keepalive                   keepalive timer");
        l.add("3 .       <num>                     time in seconds");
        l.add("2 3     mode                        interface mode");
        l.add("3 .       peer                      peering mode");
        l.add("3 .       server                    server mode");
        l.add("3 .       client                    client mode");
    }

    /**
     * get configuration
     *
     * @param l storage
     * @param beg beginning
     */
    public void getConfig(List<String> l, String beg) {
        l.add(beg + "keepalive " + keepaliveInterval);
        String a;
        switch (oprMode) {
            case 0:
                a = "peer";
                break;
            case 1:
                a = "server";
                break;
            case 2:
                a = "client";
                break;
            default:
                a = "unknown";
                break;
        }
        l.add(beg + "mode " + a);
    }

    /**
     * do configuration
     *
     * @param cmd command
     */
    public void doConfig(cmds cmd) {
        String a = cmd.word();
        if (a.equals("keepalive")) {
            keepaliveInterval = bits.str2num(cmd.word());
            restartTimer(false);
            return;
        }
        if (a.equals("mode")) {
            a = cmd.word();
            if (a.equals("peer")) {
                oprMode = 0;
            }
            if (a.equals("server")) {
                oprMode = 1;
            }
            if (a.equals("client")) {
                oprMode = 2;
            }
            clearState();
            return;
        }
        cmd.badCmd();
    }

    /**
     * clear state info
     */
    public void clearState() {
        switch (oprMode) {
            case 0:
                curMode = 2;
                break;
            case 1:
                curMode = 1;
                break;
            case 2:
                curMode = 0;
                break;
        }
        echoSent = 100;
    }

    /**
     * check peer state from timers
     *
     * @param force try to force line protocol
     * @return true if state changed
     */
    public boolean checkPeerState(state.states force) {
        state.states stat = state.states.down;
        if (echoSent < 5) {
            stat = state.states.up;
        }
        if (keepaliveInterval < 1) {
            stat = state.states.up;
        }
        if (state.toForceable(force) == state.states.admin) {
            stat = state.states.admin;
        }
        if (lastState == stat) {
            return false;
        }
        if (stat != state.states.up) {
            clearState();
        }
        lastState = stat;
        if (debugger.ifcSepEvnt) {
            logger.debug("line proto=" + state.conv2string(stat));
        }
        if (stat != state.states.up) {
            lower.flapped();
        } else {
            sendKeepReq();
        }
        cntr.stateChange(stat);
        upper.setState(stat);
        return true;
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
        if (lastState == state.states.admin) {
            return;
        }
        if (shutdown) {
            return;
        }
        if (keepaliveInterval < 1) {
            return;
        }
        keepTimer = new Timer();
        ifcSepKeep task = new ifcSepKeep(this);
        keepTimer.schedule(task, 500, keepaliveInterval * 1000);
    }

    /**
     * send keepalive over lcp
     */
    public void sendKeepReq() {
        if (echoSent++ > 5) {
            clearState();
            checkPeerState(state.states.up);
        }
        int i = 0;
        switch (curMode) {
            case 0:
                i = typReq;
                break;
            case 1:
                i = typRst;
                break;
            case 2:
                i = typKeep;
                break;
        }
        if (debugger.ifcSepEvnt) {
            logger.debug("sending keepalive type " + type2str(i));
        }
        packHolder pck = new packHolder(true, true);
        pck.msbPutW(0, ethTyp);
        pck.putByte(2, i);
        pck.putSkip(3);
        pck.merge2beg();
        lower.sendPack(pck);
    }

    private void updateAddr() {
        if (cfger == null) {
            return;
        }
        if (loc4addr != null) {
            if (!loc4addr.isEmpty()) {
                cfger.addr4changed(loc4addr, cfger.mask4, rem4addr);
            }
        }
        if (loc6addr != null) {
            if (!loc6addr.isEmpty()) {
                cfger.addr6changed(loc6addr, cfger.mask6, rem6addr);
            }
        }
    }

    private void getAddr(packHolder pck, addrType adr) {
        pck.getAddr(adr, 0);
        pck.getSkip(adr.getSize());
    }

    public void recvPack(packHolder pck) {
        cntr.rx(pck);
        if (pck.msbGetW(0) == ethTyp) {
            int i = pck.getByte(2);
            pck.getSkip(3);
            if (debugger.ifcSepEvnt) {
                logger.debug("got keepalive type " + type2str(i));
            }
            switch (i) {
                case typReq:
                    if (debugger.ifcSepEvnt) {
                        logger.debug("sending reply " + loc4addr + " " + rem4addr + " " + loc6addr + " " + rem6addr);
                    }
                    pck.clear();
                    pck.msbPutW(0, ethTyp);
                    pck.putByte(2, typRep);
                    pck.putSkip(3);
                    pck.putFill(0, 128, 0);
                    if (rem4addr != null) {
                        pck.putAddr(0, loc4addr);
                        pck.putAddr(addrIPv4.size, rem4addr);
                    }
                    pck.putSkip(2 * addrIPv4.size);
                    if (rem6addr != null) {
                        pck.putAddr(0, loc6addr);
                        pck.putAddr(addrIPv6.size, rem6addr);
                    }
                    pck.putSkip(2 * addrIPv6.size);
                    pck.merge2beg();
                    lower.sendPack(pck);
                    updateAddr();
                    curMode = 2;
                    break;
                case typRep:
                    loc4addr = new addrIPv4();
                    rem4addr = new addrIPv4();
                    loc6addr = new addrIPv6();
                    rem6addr = new addrIPv6();
                    getAddr(pck, rem4addr);
                    getAddr(pck, loc4addr);
                    getAddr(pck, rem6addr);
                    getAddr(pck, loc6addr);
                    updateAddr();
                    curMode = 2;
                    break;
                case typKeep:
                    break;
                case typRst:
                    clearState();
                    break;
            }
            echoSent = 0;
            checkPeerState(state.states.up);
            return;
        }
        if (lastState != state.states.up) {
            cntr.drop(pck, counter.reasons.notUp);
            return;
        }
        upper.recvPack(pck);
    }

    public void sendPack(packHolder pck) {
        cntr.tx(pck);
        if (lastState != state.states.up) {
            cntr.drop(pck, counter.reasons.notUp);
            return;
        }
        lower.sendPack(pck);
    }

}

class ifcSepKeep extends TimerTask {

    private ifcSep lower;

    public ifcSepKeep(ifcSep parent) {
        lower = parent;
    }

    public void run() {
        try {
            lower.sendKeepReq();
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

}
