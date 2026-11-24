package org.freertr.ifc;

import org.freertr.addr.addrEmpty;
import org.freertr.pack.packHolder;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabIndex;
import org.freertr.util.bits;
import org.freertr.util.debugger;
import org.freertr.util.logger;
import org.freertr.util.smalist;

/**
 * security group tag (rfcXXXX) protocol
 *
 * @author matecsaba
 */
public class ifcSgt {

    /**
     * create instance
     *
     * @param et handler
     */
    public ifcSgt(ifcEthTyp et) {
        upper = et;
    }

    private final ifcEthTyp upper;

    /**
     * optional value
     */
    public int optional = -1;

    /**
     * allowed values
     */
    public smalist allowIn = null;

    /**
     * allowed values
     */
    public smalist allowOut = null;

    /**
     * forbidden values
     */
    public smalist forbidIn = null;

    /**
     * forbidden values
     */
    public smalist forbidOut = null;

    /**
     * ethertype of these packets
     */
    public final static int type = 0x8909;

    /**
     * magic of these packets
     */
    public final static int magic = 0x01010001;

    /**
     * size of header
     */
    public final static int size = 8;

    /**
     * encode one packet
     *
     * @param pck packet to encrypt
     * @return false on success, true on error
     */
    public boolean doEncode(packHolder pck) {
        if (debugger.ifcSgtTraf) {
            logger.debug("tx sgt=" + pck.SGTid);
        }
        if (forbidOut != null) {
            if (forbidOut.binarySearch(pck.SGTid) >= 0) {
                return true;
            }
        }
        if (allowOut != null) {
            if (allowOut.binarySearch(pck.SGTid) < 0) {
                return true;
            }
        }
        pck.msbPutW(0, type);
        pck.msbPutD(2, magic);
        pck.msbPutW(6, pck.SGTid);
        pck.putSkip(size);
        pck.merge2beg();
        return false;
    }

    /**
     * decode one packet
     *
     * @param pck packet to decrypt
     * @param allowClear allot cleartext also
     * @return false on success, true on error
     */
    public boolean doDecode(packHolder pck, boolean allowClear) {
        int typ = pck.msbGetW(0);
        if (typ != type) {
            if (allowClear) {
                return false;
            }
            if (optional < 0) {
                logger.info("bad type (" + bits.toHexW(typ) + ") on " + upper);
                return true;
            }
            pck.SGTid = optional;
            return false;
        }
        if (pck.dataSize() < size) {
            return true;
        }
        if (pck.msbGetD(2) != magic) {
            return true;
        }
        pck.SGTid = pck.msbGetW(6);
        pck.getSkip(size);
        if (debugger.ifcSgtTraf) {
            logger.debug("rx sgt=" + pck.SGTid);
        }
        if (forbidIn != null) {
            if (forbidIn.binarySearch(pck.SGTid) >= 0) {
                return true;
            }
        }
        if (allowIn == null) {
            return false;
        }
        return allowIn.binarySearch(pck.SGTid) < 0;
    }

}
