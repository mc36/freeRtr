package org.freertr.ifc;

import org.freertr.pack.packHolder;
import org.freertr.util.debugger;
import org.freertr.util.logger;

/**
 * ieee 802.1br protocol
 *
 * @author matecsaba
 */
public class ifcDot1br extends ifcVlan {

    /**
     * ethertype of these packets
     */
    public final static int type = 0x893f;

    /**
     * size of header
     */
    public final static int size = 8;

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
        int i = pck.msbGetW(2) & 0xfff; // ingress
        int o = pck.msbGetW(4) & 0xfff; // egress
        i <<= 8;
        o <<= 8;
        i |= pck.getByte(6); // ingress
        o |= pck.getByte(7); // egress
        pck.ETHvlan = (o << 20) | i;
        pck.getSkip(size);
        if (debugger.ifcDot1brTraf) {
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
        if (debugger.ifcDot1brTraf) {
            logger.debug("tx vlan=" + pck.ETHvlan);
        }
        pck.merge2beg();
        pck.msbPutW(0, type);
        pck.msbPutW(2, (pck.ETHvlan >>> 8) & 0xfff); // ingress
        pck.msbPutW(4, (pck.ETHvlan >>> 28) & 0xfff); // egress
        pck.putByte(6, pck.ETHvlan); // ingress
        pck.putByte(7, pck.ETHvlan >>> 20); // egress
        pck.putSkip(size);
        pck.merge2beg();
    }

    /**
     * convert to string
     *
     * @return string
     */
    public String toString() {
        return "dot1br on " + lower;
    }

    /**
     * register ethertype
     *
     * @param ethtyp handler
     */
    public void reg2ethTyp(ifcEthTyp ethtyp) {
        cntr.dropper = ethtyp.getCounter();
        ethtyp.addET(type, "dot1br", this);
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
    public ifcDot1br() {
        if (debugger.ifcDot1brTraf) {
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
