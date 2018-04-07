package ifc;

import addr.addrMac;
import ip.ipIfc4;
import ip.ipIfc6;
import ip.ipMpls;
import pack.packHolder;
import util.counter;
import util.debugger;
import util.logger;
import util.state;

/**
 * network service header (rfc8300) encapsulation handler
 *
 * @author matecsaba
 */
public class ifcNshFwd implements ifcUp {

    /**
     * ethertype
     */
    public final static int type = 0x894f;

    /**
     * size
     */
    public final static int size = 8;

    /**
     * ipv4 packet
     */
    public static final int protIp4 = 0x01;

    /**
     * ipv6 packet
     */
    public static final int protIp6 = 0x02;

    /**
     * ethernet packet
     */
    public static final int protEth = 0x03;

    /**
     * nsh packet
     */
    public static final int protNsh = 0x04;

    /**
     * mpls packet
     */
    public static final int protMpls = 0x05;

    /**
     * gbp packet
     */
    public static final int protGbp = 0x06;

    /**
     * counter of this interface
     */
    public counter cntr = new counter();

    /**
     * server that sends our packets
     */
    public ifcDn lower = new ifcNull();

    /**
     * hardware address
     */
    public addrMac hwaddr = addrMac.getBroadcast();

    /**
     * parse nsh header
     *
     * @param pck packet to parse
     * @return false on success, true on error
     */
    public static boolean parseNSHheader(packHolder pck) {
        if (pck.dataSize() < size) {
            return true;
        }
        int len = pck.msbGetW(0);
        if ((len & 0xe000) != 0) { // version + oam
            return true;
        }
        pck.NSHttl = (len >>> 6) & 0x3f;
        len = (len & 0x3f) * 4;
        if (len < size) {
            return true;
        }
        if (pck.dataSize() < len) {
            return true;
        }
        pck.NSHmdt = pck.getByte(2) & 0xf;
        pck.IPprt = pck.getByte(3);
        pck.NSHsi = pck.msbGetD(4);
        pck.NSHsp = pck.NSHsi >>> 8;
        pck.NSHsi &= 0xff;
        pck.getSkip(size);
        pck.NSHmdv = new byte[len - size];
        pck.getCopy(pck.NSHmdv, 0, 0, pck.NSHmdv.length);
        pck.getSkip(pck.NSHmdv.length);
        return false;
    }

    /**
     * create nsh header
     *
     * @param pck packet to parse
     */
    public static void createNSHheader(packHolder pck) {
        pck.msbPutW(0, type);
        pck.putSkip(2);
        pck.msbPutW(0, (pck.NSHttl << 6) | ((size + pck.NSHmdv.length + 3) / 4));
        pck.putByte(2, pck.NSHmdt);
        pck.putByte(3, pck.IPprt);
        pck.msbPutD(4, (pck.NSHsp << 8) | (pck.NSHsi & 0xff));
        pck.putSkip(size);
        pck.putCopy(pck.NSHmdv, 0, 0, pck.NSHmdv.length);
        pck.putSkip(pck.NSHmdv.length);
        if ((pck.NSHmdv.length & 3) != 0) {
            int i = 4 - (pck.NSHmdv.length & 3);
            pck.putFill(0, i, 0);
            pck.putSkip(i);
        }
        pck.merge2beg();
    }

    /**
     * convert to ethertype
     *
     * @param pck packet to convert
     * @return ethertype, -1 on error
     */
    public static int convert2ethtyp(packHolder pck) {
        int i;
        switch (pck.IPprt) {
            case protEth:
                ifcEther.parseETHheader(pck, true);
                i = pck.ETHtype;
                break;
            case protIp4:
                i = ipIfc4.type;
                break;
            case protIp6:
                i = ipIfc6.type;
                break;
            case protMpls:
                i = ipMpls.typeU;
                break;
            case protNsh:
                i = type;
                break;
            default:
                return -1;
        }
        pck.msbPutW(0, i);
        pck.putSkip(2);
        pck.merge2beg();
        pck.ETHtype = i;
        return i;
    }

    public void setParent(ifcDn parent) {
        lower = parent;
        try {
            hwaddr = (addrMac) lower.getHwAddr();
        } catch (Exception e) {
        }
    }

    public counter getCounter() {
        return cntr;
    }

    public void closeUp() {
    }

    public void setState(state.states stat) {
    }

    public String toString() {
        return "nsh on " + lower;
    }

    public void recvPack(packHolder pck) {
        cntr.rx(pck);
        if (pck.msbGetW(0) != type) {
            return;
        }
        pck.getSkip(2);
        if (parseNSHheader(pck)) {
            cntr.drop(pck, counter.reasons.badHdr);
            return;
        }
        if (debugger.ifcNshEvnt) {
            logger.debug("rx sp=" + pck.NSHsp + " si=" + pck.NSHsi + " prt=" + pck.IPprt + " ttl=" + pck.NSHttl + " meta=" + pck.NSHmdt + "," + pck.NSHmdv.length);
        }
        ipMpls.gotNshPack(null, pck);
    }

    /**
     * send one packet
     *
     * @param pck packet to send
     * @param trg target address
     */
    public void doTxNsh(packHolder pck, addrMac trg) {
        if (debugger.ifcNshEvnt) {
            logger.debug("tx sp=" + pck.NSHsp + " si=" + pck.NSHsi + " prt=" + pck.IPprt + " ttl=" + pck.NSHttl + " meta=" + pck.NSHmdt + "," + pck.NSHmdv.length);
        }
        createNSHheader(pck);
        pck.ETHsrc.setAddr(hwaddr);
        pck.ETHtrg.setAddr(trg);
        lower.sendPack(pck);
    }

    /**
     * send one packet
     *
     * @param pck packet to send
     * @param trg target address
     */
    public void doTxRaw(packHolder pck, addrMac trg) {
        if (debugger.ifcNshEvnt) {
            logger.debug("tx sp=" + pck.NSHsp + " si=" + pck.NSHsi + " prt=" + pck.IPprt + " ttl=" + pck.NSHttl + " meta=" + pck.NSHmdt + "," + pck.NSHmdv.length);
        }
        if (trg != null) {
            pck.ETHsrc.setAddr(hwaddr);
            pck.ETHtrg.setAddr(trg);
        }
        lower.sendPack(pck);
    }

}
