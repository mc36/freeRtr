package org.freertr.ifc;

import org.freertr.pack.packHolder;
import org.freertr.util.debugger;
import org.freertr.util.logger;

/**
 * trill fine-grained label (rfc7172) protocol
 *
 * @author matecsaba
 */
public class ifcTrillFgl extends ifcVlan {

    /**
     * ethertype of these packets
     */
    public final static int type = 0x893b;

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
        if (pck.msbGetW(4) != type) {
            return true;
        }
        int o = pck.msbGetW(2); // vlan tag
        int i = pck.msbGetW(6); // vlan tag
        pck.ETHvlan = o & 0xfff; // id
        pck.ETHvlan <<= 12;
        pck.ETHvlan |= i & 0xfff; // id
        pck.ETHcos = (i >>> 13) & 7; // cos
        pck.getSkip(size);
        if (debugger.ifcTrillFglTraf) {
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
        if (debugger.ifcTrillFglTraf) {
            logger.debug("tx vlan=" + pck.ETHvlan);
        }
        int i = (pck.ETHcos & 7) << 13;
        pck.msbPutW(0, type); // ether type
        pck.msbPutW(2, ((pck.ETHvlan >>> 12) & 0xfff) | i); // vlan tag
        pck.msbPutW(4, type); // ether type
        pck.msbPutW(6, (pck.ETHvlan & 0xfff) | i); // vlan tag
        pck.putSkip(size);
        pck.merge2beg();
    }

    /**
     * convert to string
     *
     * @return string
     */
    public String toString() {
        return "trill-fgl on " + lower;
    }

    /**
     * register ethertype
     *
     * @param ethtyp handler
     */
    public void reg2ethTyp(ifcEthTyp ethtyp) {
        cntr.dropper = ethtyp.getCounter();
        ethtyp.addET(type, "trill-fgl", this);
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
    public ifcTrillFgl() {
        if (debugger.ifcTrillFglTraf) {
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
