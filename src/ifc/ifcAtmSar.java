package ifc;

import addr.addrEmpty;
import addr.addrType;
import cry.cryHashCrc32;
import cry.cryHashHec8;
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
 * atm sar encapsulation handler
 *
 * @author matecsaba
 */
public class ifcAtmSar implements ifcUp, ifcDn {

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
    public final static int headSize = 5;

    /**
     * size of payload
     */
    public final static int paySize = 48;

    /**
     * size of cell
     */
    public final static int cellSize = headSize + paySize;

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

    private packHolder assem = new packHolder(true, true);

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

    public state.states getState() {
        return state.states.up;
    }

    public void setFilter(boolean promisc) {
    }

    public addrType getHwAddr() {
        return new addrEmpty();
    }

    public void setState(state.states stat) {
    }

    public int getMTUsize() {
        return lower.getMTUsize() - headSize;
    }

    public long getBandwidth() {
        return lower.getBandwidth();
    }

    public String toString() {
        return "atmSar on " + lower;
    }

    /**
     * create new instance
     */
    public ifcAtmSar() {
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

    private void sendCell(packHolder pck, boolean last) {
        pck.merge2beg();
        byte[] buf = new byte[4];
        bits.msbPutD(buf, 0, (vpiNum << 20) | ((vciNum & 0xffff) << 4));
        if (last) {
            buf[3] |= 2;
        }
        cryHashHec8 hec = new cryHashHec8();
        hec.init();
        hec.update(buf);
        pck.putCopy(buf, 0, 0, buf.length);
        buf = hec.finish();
        pck.putByte(4, buf[0]);
        pck.putSkip(headSize);
        pck.merge2beg();
        lower.sendPack(pck);
    }

    public synchronized void sendPack(packHolder pck) {
        pck.putByte(0, 0xaa);
        pck.putByte(1, 0xaa);
        pck.putByte(2, 0x03);
        pck.putByte(3, 0);
        pck.putByte(4, 0);
        pck.putByte(5, 0);
        pck.putSkip(6);
        pck.merge2beg();
        int o = pck.dataSize();
        int i = paySize - (o % paySize);
        if (i < 8) {
            i += paySize;
        }
        pck.putFill(0, i, 0);
        pck.msbPutW(i - 6, o);
        pck.putSkip(i - 4);
        pck.merge2end();
        byte[] buf = pck.getCopy();
        cryHashCrc32 crc = new cryHashCrc32();
        crc.init();
        crc.update(buf, 0, buf.length);
        buf = crc.finish();
        pck.putCopy(buf, 0, 0, buf.length);
        pck.putSkip(buf.length);
        pck.merge2end();
        o = pck.dataSize();
        for (i = 0; i < o;) {
            buf = new byte[paySize];
            pck.getCopy(buf, 0, i, buf.length);
            i += buf.length;
            packHolder p = new packHolder(true, true);
            p.putCopy(buf, 0, 0, buf.length);
            p.putSkip(buf.length);
            sendCell(p, i >= o);
        }
    }

    public void recvPack(packHolder pck) {
        if (pck.dataSize() != cellSize) {
            cntr.drop(pck, counter.reasons.badSiz);
            return;
        }
        int vpi = pck.msbGetW(0) >>> 4;
        int vci = (pck.msbGetD(0) >>> 4) & 0xffff;
        boolean last = (pck.getByte(3) & 2) != 0;
        cryHashHec8 hec = new cryHashHec8();
        hec.init();
        for (int i = 0; i < 4; i++) {
            hec.update(pck.getByte(i));
        }
        if ((hec.finish()[0] & 0xff) != pck.getByte(4)) {
            cntr.drop(pck, counter.reasons.badSum);
            return;
        }
        pck.getSkip(headSize);
        if (debugger.ifcAtmSarEvnt) {
            logger.debug("vpi=" + vpi + " vci=" + vci + " last=" + last);
        }
        if ((vpi != vpiNum) || (vci != vciNum)) {
            cntr.drop(pck, counter.reasons.badVlan);
            return;
        }
        byte[] buf1 = pck.getCopy();
        assem.putCopy(buf1, 0, 0, buf1.length);
        assem.putSkip(buf1.length);
        assem.merge2end();
        if (!last) {
            return;
        }
        int o = assem.dataSize();
        cryHashCrc32 crc = new cryHashCrc32();
        crc.init();
        crc.update(assem.getCopy(), 0, o - 4);
        buf1 = crc.finish();
        byte[] buf2 = new byte[buf1.length];
        assem.getCopy(buf2, 0, o - 4, buf2.length);
        if (bits.byteComp(buf1, 0, buf2, 0, buf1.length) != 0) {
            cntr.drop(assem, counter.reasons.badSum);
            assem.clear();
            return;
        }
        int i = assem.msbGetW(o - 6);
        if (i > o) {
            cntr.drop(assem, counter.reasons.tooSmall);
            assem.clear();
            return;
        }
        assem.setDataSize(i);
        assem.getSkip(6);
        upper.recvPack(assem);
        assem.clear();
    }

}
