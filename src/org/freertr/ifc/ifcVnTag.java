package org.freertr.ifc;

import org.freertr.pack.packHolder;
import org.freertr.util.debugger;
import org.freertr.util.logger;

/**
 * vn tag protocol
 *
 * @author matecsaba
 */
public class ifcVnTag extends ifcVlan {

    /**
     * ethertype of these packets
     */
    public final static int type = 0x8926;

    /**
     * size of header
     */
    public final static int size = 6;

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
        int i = pck.msbGetW(2) & 0xfff; // target
        int o = pck.msbGetW(4) & 0xfff; // source
        pck.ETHvlan = (o << 12) | i;
        pck.getSkip(size);
        if (debugger.ifcVnTagTraf) {
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
        if (debugger.ifcVnTagTraf) {
            logger.debug("tx vlan=" + pck.ETHvlan);
        }
        pck.merge2beg();
        pck.msbPutW(0, type);
        pck.msbPutW(2, pck.ETHvlan & 0xfff); // target
        pck.msbPutW(4, (pck.ETHvlan >>> 12) & 0xfff); // source
        pck.putSkip(size);
        pck.merge2beg();
    }

    /**
     * convert to string
     *
     * @return string
     */
    public String toString() {
        return "vntag on " + lower;
    }

    /**
     * register ethertype
     *
     * @param ethtyp handler
     */
    public void reg2ethTyp(ifcEthTyp ethtyp) {
        cntr.dropper = ethtyp.getCounter();
        ethtyp.addET(type, "vntag", this);
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
    public ifcVnTag() {
        if (debugger.ifcVnTagTraf) {
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
