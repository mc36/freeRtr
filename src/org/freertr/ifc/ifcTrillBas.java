package org.freertr.ifc;

import org.freertr.pack.packHolder;
import org.freertr.util.debugger;
import org.freertr.util.logger;

/**
 * trill (rfc6325) base header
 *
 * @author matecsaba
 */
public class ifcTrillBas extends ifcVlan {

    /**
     * ethertype
     */
    public final static int type = 0x22f3;

    /**
     * size
     */
    public final static int size = 20;

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
        if ((pck.getByte(2) & 0xc0) != 0) {
            return true;
        }
        pck.ETHcos = pck.getByte(3) & 0x1f; // ttl
        int i = pck.msbGetW(4); // egress
        int o = pck.msbGetW(6); // ingress
        pck.ETHvlan = (o << 16) | i;
        pck.getAddr(pck.ETHtrg, 8); // c-dst
        pck.getAddr(pck.ETHsrc, 14); // c-src
        pck.getSkip(size);
        if (debugger.ifcTrillBasTraf) {
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
        if (debugger.ifcTrillBasTraf) {
            logger.debug("tx vlan=" + pck.ETHvlan);
        }
        pck.merge2beg();
        pck.msbPutW(0, type);
        pck.msbPutW(2, 0x1f); // ttl
        pck.msbPutW(4, pck.ETHvlan); // egress
        pck.msbPutW(6, pck.ETHvlan >>> 16); // ingress
        pck.putAddr(8, pck.ETHtrg); // c dst
        pck.putAddr(14, pck.ETHsrc); // c src
        pck.putSkip(size);
        pck.merge2beg();
    }

    /**
     * convert to string
     *
     * @return string
     */
    public String toString() {
        return "trill-bas on " + lower;
    }

    /**
     * register ethertype
     *
     * @param ethtyp handler
     */
    public void reg2ethTyp(ifcEthTyp ethtyp) {
        cntr.dropper = ethtyp.getCounter();
        ethtyp.addET(type, "trill-bas", this);
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
    public ifcTrillBas() {
        if (debugger.ifcTrillBasTraf) {
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
