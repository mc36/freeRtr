package net.freertr.ifc;

import net.freertr.addr.addrEmpty;
import net.freertr.pack.packHolder;
import net.freertr.tab.tabGen;
import net.freertr.tab.tabIndex;
import net.freertr.util.debugger;
import net.freertr.util.logger;

/**
 * security group tag (rfcXXXX) protocol
 *
 * @author matecsaba
 */
public class ifcSgt {

    /**
     * create instance
     */
    public ifcSgt() {
    }

    /**
     * optional value
     */
    public int optional = 0;

    /**
     * allowed values
     */
    public tabGen<tabIndex<addrEmpty>> allowIn = null;

    /**
     * allowed values
     */
    public tabGen<tabIndex<addrEmpty>> allowOut = null;

    /**
     * forbidden values
     */
    public tabGen<tabIndex<addrEmpty>> forbidIn = null;

    /**
     * forbidden values
     */
    public tabGen<tabIndex<addrEmpty>> forbidOut = null;

    /**
     * ethertype of these packets
     */
    public static final int type = 0x8909;

    /**
     * magic of these packets
     */
    public static final int magic = 0x01010001;

    /**
     * size of header
     */
    public static final int size = 8;

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
            tabIndex<addrEmpty> cur = new tabIndex<addrEmpty>(pck.SGTid, null);
            if (forbidOut.find(cur) != null) {
                return true;
            }
        }
        if (allowOut != null) {
            tabIndex<addrEmpty> cur = new tabIndex<addrEmpty>(pck.SGTid, null);
            if (allowOut.find(cur) == null) {
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
     * @return false on success, true on error
     */
    public boolean doDecode(packHolder pck) {
        if (pck.msbGetW(0) != type) {
            if (optional < 1) {
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
            tabIndex<addrEmpty> cur = new tabIndex<addrEmpty>(pck.SGTid, null);
            if (forbidIn.find(cur) != null) {
                return true;
            }
        }
        if (allowIn == null) {
            return false;
        }
        tabIndex<addrEmpty> cur = new tabIndex<addrEmpty>(pck.SGTid, null);
        return allowIn.find(cur) == null;
    }

}
