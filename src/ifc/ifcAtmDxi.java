package ifc;

import addr.addrEmpty;
import addr.addrType;
import java.util.List;
import pack.packHolder;
import user.userHelping;
import util.bits;
import util.cmds;
import util.counter;
import util.debugger;
import util.logger;
import util.state;

/**
 * atm dxi encapsulation handler
 *
 * @author matecsaba
 */
public class ifcAtmDxi implements ifcUp, ifcDn {

    /**
     * vpi number
     */
    public int vpiNum;

    /**
     * vci number
     */
    public int vciNum;

    /**
     * size of header
     */
    public final static int size = 8;

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
        return "atmDxi on " + lower;
    }

    /**
     * create new instance
     */
    public ifcAtmDxi() {
    }

    /**
     * get help text
     *
     * @param l storage
     */
    public static void getHelp(userHelping l) {
        l.add("2 3     vpi                         set vpi number");
        l.add("3 .       <num>                     vpi number");
        l.add("2 3     vci                         set vci number");
        l.add("3 .       <num>                     vci number");
    }

    /**
     * get configuration
     *
     * @param l storage
     * @param beg beginning
     */
    public void getConfig(List<String> l, String beg) {
        l.add(beg + "vpi " + vpiNum);
        l.add(beg + "vci " + vciNum);
    }

    /**
     * do configuration
     *
     * @param cmd command
     */
    public void doConfig(cmds cmd) {
        String a = cmd.word();
        if (a.equals("vpi")) {
            vpiNum = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("vci")) {
            vciNum = bits.str2num(cmd.word());
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
        cmd.badCmd();
    }

    /**
     * receive packet
     *
     * @param pck packet
     */
    public void recvPack(packHolder pck) {
        cntr.rx(pck);
        if (pck.dataSize() < size) {
            cntr.drop(pck, counter.reasons.tooSmall);
            return;
        }
        int b0 = pck.getByte(0);
        int b1 = pck.getByte(1);
        pck.getSkip(size);
        int vpi = (b0 >>> 2) & 0xf;
        int vci = ((b0 >>> 2) & 0x30) | (b1 >>> 4);
        if (debugger.ifcAtmDxiEvnt) {
            logger.debug("vpi=" + vpi + " vci=" + vci);
        }
        if ((vpi != vpiNum) || (vci != vciNum)) {
            cntr.drop(pck, counter.reasons.badVlan);
            return;
        }
        upper.recvPack(pck);
    }

    /**
     * send packet
     *
     * @param pck packet
     */
    public void sendPack(packHolder pck) {
        cntr.tx(pck);
        pck.putByte(0, ((vciNum & 0x30) << 6) | ((vpiNum & 0xf) << 2));
        pck.putByte(1, (vciNum << 4) | 1);
        pck.putByte(2, 0xaa);
        pck.putByte(3, 0xaa);
        pck.putByte(4, 0x03);
        pck.putByte(5, 0);
        pck.putByte(6, 0);
        pck.putByte(7, 0);
        pck.putSkip(size);
        pck.merge2beg();
        lower.sendPack(pck);
    }

}
