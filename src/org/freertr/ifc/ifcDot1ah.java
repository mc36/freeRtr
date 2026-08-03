package org.freertr.ifc;

import org.freertr.addr.addrMac;
import org.freertr.pack.packHolder;
import org.freertr.util.bits;
import org.freertr.util.debugger;
import org.freertr.util.logger;

/**
 * ieee 802.1ah protocol
 *
 * @author matecsaba
 */
public class ifcDot1ah extends ifcVlan {

    /**
     * ethertype of these packets
     */
    public final static int type = 0x88e7;

    /**
     * size of header
     */
    public final static int size = 18;

    /**
     * parse header
     *
     * @param pck packet to parse
     * @return false on success, true on error
     */
    public boolean parseHeader(packHolder pck) {
        if (pck.msbGetW(0) != type) {
            return true;
        }
        int i = pck.msbGetD(2); // i tag
        pck.ETHvlan = i & 0xffffff; // id
        pck.ETHcos = (i >>> 29) & 7; // cos
        pck.getAddr(pck.ETHtrg, 6); // c-dst
        pck.getAddr(pck.ETHsrc, 12); // c-src
        pck.getSkip(size);
        if (debugger.ifcDot1ahTraf) {
            logger.debug("rx vlan=" + pck.ETHvlan);
        }
        return false;
    }

    /**
     * create header
     *
     * @param pck packet to update
     */
    public void createHeader(packHolder pck) {
        if (debugger.ifcDot1ahTraf) {
            logger.debug("tx vlan=" + pck.ETHvlan);
        }
        pck.msbPutW(0, type); // ether type
        pck.msbPutD(2, (pck.ETHvlan & 0xffffff) | ((pck.ETHcos & 7) << 29)); // i tag
        pck.putAddr(6, pck.ETHtrg); // c dst
        pck.putAddr(12, pck.ETHsrc); // c src
        pck.putSkip(size);
        pck.merge2beg();
    }

    /**
     * create destination b-mac address for flooded traffic
     *
     * @param isid isid to use
     * @return dst bmac
     */
    public static addrMac dstBmac4flood(int isid) {
        byte[] buf = new byte[6];
        bits.msbPutD(buf, 2, isid);
        buf[0] = 0x01;
        buf[1] = 0x1e;
        buf[2] = (byte) 0x83;
        addrMac a = new addrMac();
        a.fromBuf(buf, 0);
        return a;
    }

    public String toString() {
        return "dot1ah on " + lower;
    }

    /**
     * register ethertype
     *
     * @param ethtyp handler
     */
    public void reg2ethTyp(ifcEthTyp ethtyp) {
        cntr.dropper = ethtyp.getCounter();
        ethtyp.addET(type, "dot1ah", this);
        ethtyp.updateET(type, this);
    }

    /**
     * unregister ethertype
     *
     * @param ethtyp handler
     */
    public void unreg2ethTyp(ifcEthTyp ethtyp) {
        vLans.clear();
        ethtyp.delET(type);
    }

    /**
     * create new multiplexer
     */
    public ifcDot1ah() {
        if (debugger.ifcDot1ahTraf) {
            logger.debug("started");
        }
    }

    /**
     * get size of mtu
     *
     * @return mtu size
     */
    public int remainingMtu() {
        return lower.getMTUsize() - size;
    }

}
