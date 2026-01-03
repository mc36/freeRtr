package org.freertr.ifc;

import java.util.List;
import org.freertr.addr.addrEmpty;
import org.freertr.addr.addrEui;
import org.freertr.addr.addrType;
import org.freertr.pack.packHolder;
import org.freertr.user.userHelp;
import org.freertr.util.cmds;
import org.freertr.util.counter;
import org.freertr.util.state;

/**
 * switched multi-megabit data service (rfc1209) encapsulation handler
 *
 * @author matecsaba
 */
public class ifcSmds implements ifcUp, ifcDn {

    /**
     * size of header
     */
    public final static int size = 38;

    /**
     * counter of this interface
     */
    public counter cntr = new counter();

    /**
     * server that handler received packets
     */
    public ifcUp upper = new ifcNull();

    /**
     * server that sends our packets
     */
    public ifcDn lower = new ifcNull();

    /**
     * dxi mode
     */
    public boolean dxiMode;

    /**
     * nec mode
     */
    public boolean necMode;

    /**
     * tx sequence
     */
    public int seqTx = 0;

    /**
     * rx sequence
     */
    public int seqRx = -1;

    /**
     * source address
     */
    public addrEui srcAddr = new addrEui();

    /**
     * target address
     */
    public addrEui trgAddr = new addrEui();

    /**
     * get counter
     *
     * @return counter
     */
    public counter getCounter() {
        return cntr;
    }

    /**
     * set parent
     *
     * @param parent parent
     */
    public void setParent(ifcDn parent) {
        lower = parent;
    }

    /**
     * close interface
     */
    public void closeUp() {
        setState(state.states.close);
        upper.closeUp();
    }

    /**
     * close interface
     */
    public void closeDn() {
        setState(state.states.close);
        lower.closeDn();
    }

    /**
     * flap interface
     */
    public void flapped() {
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
     * get state
     *
     * @return state
     */
    public state.states getState() {
        return state.states.up;
    }

    /**
     * set filter
     *
     * @param promisc promiscous mode
     */
    public void setFilter(boolean promisc) {
    }

    /**
     * get hw address
     *
     * @return hw address
     */
    public addrType getHwAddr() {
        return new addrEmpty();
    }

    /**
     * set state
     *
     * @param stat state
     */
    public void setState(state.states stat) {
    }

    /**
     * get mtu size
     *
     * @return mtu size
     */
    public int getMTUsize() {
        return lower.getMTUsize() - size;
    }

    /**
     * get bandwidth
     *
     * @return bandwidth
     */
    public long getBandwidth() {
        return lower.getBandwidth();
    }

    public String toString() {
        return "smds on " + lower;
    }

    /**
     * create new instance
     */
    public ifcSmds() {
    }

    /**
     * get help text
     *
     * @param l storage
     */
    public static void getHelp(userHelp l) {
        l.add(null, false, 2, new int[]{-1}, "dxi-mode", "set dxi mode");
        l.add(null, false, 2, new int[]{-1}, "nec-mode", "set nec mode");
        l.add(null, false, 2, new int[]{3}, "source", "set source address");
        l.add(null, false, 3, new int[]{-1}, "<addr>", "source address");
        l.add(null, false, 2, new int[]{3}, "target", "set target address");
        l.add(null, false, 3, new int[]{-1}, "<addr>", "source address");
    }

    /**
     * get configuration
     *
     * @param l storage
     * @param beg beginning
     */
    public void getConfig(List<String> l, String beg) {
        if (dxiMode) {
            l.add(beg + "dxi-mode");
        } else {
            l.add(cmds.tabulator + cmds.negated + beg + "dxi-mode");
        }
        if (necMode) {
            l.add(beg + "nec-mode");
        } else {
            l.add(cmds.tabulator + cmds.negated + beg + "nec-mode");
        }
        l.add(beg + "source " + srcAddr);
        l.add(beg + "target " + trgAddr);
    }

    /**
     * do configuration
     *
     * @param cmd command
     */
    public void doConfig(cmds cmd) {
        String a = cmd.word();
        if (a.equals("dxi-mode")) {
            dxiMode = true;
            return;
        }
        if (a.equals("nec-mode")) {
            necMode = true;
            return;
        }
        if (a.equals("source")) {
            srcAddr.fromString(cmd.word());
            return;
        }
        if (a.equals("target")) {
            trgAddr.fromString(cmd.word());
            return;
        }
        cmd.badCmd();
    }

    /**
     * undo configuration
     *
     * @param cmd command
     */
    public void unConfig(cmds cmd) {
        String a = cmd.word();
        if (a.equals("dxi-mode")) {
            dxiMode = false;
            return;
        }
        if (a.equals("nec-mode")) {
            necMode = false;
            return;
        }
        cmd.badCmd();
    }

    /**
     * receive packet
     *
     * @param pck packet
     */
    public void recvPack(packHolder pck) {
        cntr.rx(pck);
        if (dxiMode) {
            pck.getSkip(2);
        }
        if (pck.dataSize() < size) {
            cntr.drop(pck, counter.reasons.tooSmall);
            return;
        }
        seqRx = pck.getByte(3); // be tag
        int i = pck.msbGetW(4); // ba size
        if (pck.getByte(i + 7) != seqRx) { // trailer tag
            cntr.drop(pck, counter.reasons.badID);
            return;
        }
        if (pck.msbGetW(i + 8) != i) { // trailer size
            cntr.drop(pck, counter.reasons.badHdr);
            return;
        }
        if (pck.dataSize() < (i + 10)) {
            cntr.drop(pck, counter.reasons.badLen);
            return;
        }
        pck.setDataSize(i + 6);
        i = pck.getByte(22); // hlpi + pl
        if (!necMode) {
            i &= 3;
            pck.setDataSize(pck.dataSize() - i);
        }
        i = pck.getByte(23); // qos + cib + hel
        if ((i & 0x8) != 0) {
            pck.setDataSize(pck.dataSize() - 4); // crc32
        }
        pck.getSkip(size);
        ifcAtmEnc.snap2ethtyp(pck);
        upper.recvPack(pck);
    }

    /**
     * send packet
     *
     * @param pck packet
     */
    public void sendPack(packHolder pck) {
        cntr.tx(pck);
        int i = pck.dataSize();
        int o = (i + 2) & 3;
        if (o != 0) {
            o = 4 - o;
            pck.putFill(0, o, 0);
            pck.putSkip(o);
            pck.merge2end();
            i += o;
        }
        ifcAtmEnc.ethtyp2snap(pck);
        seqTx++;
        i += size;
        pck.msbPutW(0, 0); // reserved
        pck.putByte(2, 0); // reserved
        pck.putByte(3, seqTx); // be tag
        pck.msbPutW(4, i); // ba size
        pck.putAddr(6, trgAddr);
        pck.putAddr(14, srcAddr);
        if (necMode) {
            pck.putByte(22, 1); // hlpi + pl
        } else {
            pck.putByte(22, 4 | o); // hlpi + pl
        }
        pck.putByte(23, 3); // qos + cib + hel
        pck.msbPutW(24, 0); // bridge
        pck.msbPutD(26, 0x3000100); // header extension
        pck.msbPutD(30, 0); // unused head ext
        pck.msbPutD(34, 0); // unused head ext
        pck.putSkip(size);
        pck.merge2beg();
        pck.putByte(0, 0); // reserved
        pck.putByte(1, seqTx); // trailer tag
        pck.msbPutW(2, i); // trailer size
        pck.putSkip(4);
        pck.merge2end();
        if (dxiMode) {
            pck.msbPutW(0, 0x0503);
            pck.putSkip(2);
            pck.merge2beg();
        }
        lower.sendPack(pck);
    }

}
