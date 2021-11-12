package net.freertr.ifc;

import net.freertr.addr.addrMac;
import net.freertr.cry.cryHashCrc32;
import net.freertr.pack.packHolder;
import net.freertr.util.bits;
import net.freertr.util.debugger;
import net.freertr.util.logger;

/**
 * inter switch link protocol
 *
 * @author matecsaba
 */
public class ifcIsl extends ifcVlan {

    /**
     * ethertype of these packets
     */
    public final static int type = 0x00000c;

    /**
     * size of header
     */
    public final static int size = 18;

    /**
     * target address
     */
    protected addrMac targetAddr;

    public String toString() {
        return "isl on " + lower;
    }

    /**
     * parse header
     *
     * @param pck packet to parse
     * @return false on success, true on error
     */
    public boolean parseHeader(packHolder pck) {
        pck.ETHcos = 0;
        pck.ETHvlan = pck.msbGetW(0) >>> 1; // vlan tag
        pck.getSkip(size - addrMac.sizeX2);
        ifcEther.parseETHheader(pck, false);
        pck.setDataSize(pck.dataSize() - 4);
        if (debugger.ifcIslTraf) {
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
        if (debugger.ifcIslTraf) {
            logger.debug("tx vlan=" + pck.ETHvlan);
        }
        ifcEther.createETHheader(pck, false);
        cryHashCrc32 sum = new cryHashCrc32(cryHashCrc32.polyCrc32i);
        sum.init();
        pck.hashData(sum, 0, pck.dataSize());
        pck.lsbPutD(0, bits.msbGetD(sum.finish(), 0)); // crc
        pck.putSkip(4);
        pck.merge2end();
        pck.msbPutW(0, pck.ETHvlan << 1); // vlan tag
        pck.msbPutD(2, 0); // index
        pck.putSkip(ifcIsl.size - addrMac.sizeX2);
        pck.merge2beg();
        pck.ETHtrg.setAddr(targetAddr);
    }

    /**
     * register ethertype
     *
     * @param ethtyp handler
     */
    public void reg2ethTyp(ifcEthTyp ethtyp) {
        cntr.dropper = ethtyp.getCounter();
        ethtyp.addSNAP(type, "isl", this);
        ethtyp.updateSNAP(type, this);
    }

    /**
     * unregister ethertype
     *
     * @param ethtyp handler
     */
    public void unreg2ethTyp(ifcEthTyp ethtyp) {
        vLans.clear();
        ethtyp.delSNAP(type);
    }

    /**
     * create new multiplexer
     */
    public ifcIsl() {
        if (debugger.ifcIslTraf) {
            logger.debug("started");
        }
        targetAddr = new addrMac();
        targetAddr.fromString("0100:0c00:0000");
    }

    /**
     * get size of mtu
     *
     * @return mtu size
     */
    public int remainingMtu() {
        return lower.getMTUsize() - size - 4;
    }

}
