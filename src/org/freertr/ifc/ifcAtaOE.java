package org.freertr.ifc;

import org.freertr.addr.addrMac;
import org.freertr.enc.encIde;
import org.freertr.pack.packHolder;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.counter;
import org.freertr.util.debugger;
import org.freertr.util.logger;
import org.freertr.util.state;

/**
 * ata over ethernet protocol server handler
 *
 * @author matecsaba
 */
public class ifcAtaOE implements ifcUp {

    /**
     * ethertype to use
     */
    public final static int ethTyp = 0x88a2;

    /**
     * header size
     */
    public final static int size = 12;

    /**
     * create instance
     *
     * @param cmd parameters
     */
    public ifcAtaOE(cmds cmd) {
        shlfId = bits.str2num(cmd.word());
        slotId = bits.str2num(cmd.word());
        file = new encIde(cmd.word());
        file.doUpdate(false);
    }

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
    public addrMac hwaddr = addrMac.getRandom();

    /**
     * client address
     */
    public addrMac clnMac = new addrMac();

    /**
     * shelf id
     */
    public int shlfId;

    /**
     * slot id
     */
    public int slotId;

    /**
     * file to use
     */
    public encIde file;

    public String toString() {
        return shlfId + " " + slotId + " " + file.name;
    }

    public void recvPack(packHolder pck) {
        if (pck.msbGetW(0) != ethTyp) {
            cntr.drop(pck, counter.reasons.badTyp);
            return;
        }
        if (pck.dataSize() < size) {
            cntr.drop(pck, counter.reasons.tooSmall);
            return;
        }
        if (pck.getByte(2) != 0x10) { // version
            cntr.drop(pck, counter.reasons.badVer);
            return;
        }
        int t = pck.msbGetD(8); // tag
        int i = pck.getByte(7); // class
        pck.getSkip(size);
        clnMac.setAddr(pck.ETHsrc);
        if (debugger.ifcAtaOE) {
            logger.debug("rx tag=" + t + " cmd=" + i);
        }
        switch (i) {
            case 0: // ata/ide
                break;
            case 1:
                pck.clear();
                pck.putStart();
                pck.ETHsrc.setAddr(hwaddr);
                pck.ETHtrg.setAddr(clnMac);
                pck.msbPutW(0, ethTyp);
                pck.putByte(2, 0x18); // version
                pck.putByte(3, 0); // error
                pck.msbPutW(4, shlfId);
                pck.putByte(6, slotId);
                pck.putByte(7, 1); // class
                pck.msbPutD(8, t); // tag
                pck.putSkip(size);
                pck.putFill(0, size, 0);
                pck.msbPutW(0, 16); // buffer count
                pck.msbPutW(2, 0x102); // firmware version
                pck.putByte(4, (int) (lower.getMTUsize() / file.blkSiz)); // scnt
                pck.putByte(5, 0x10); // cmd
                pck.msbPutW(6, 0); // csl
                pck.putSkip(size);
                pck.merge2beg();
                lower.sendPack(pck);
                if (debugger.ifcAtaOE) {
                    logger.debug("tx tag=" + t + " cmd=" + i);
                }
                return;
            default:
                cntr.drop(pck, counter.reasons.badCod);
                return;
        }
        byte[] cmd = new byte[12];
        pck.getCopy(cmd, 0, 0, cmd.length);
        pck.getSkip(cmd.length);
        byte[] res = file.doIde(cmd, pck.getCopy());
        pck.clear();
        pck.putStart();
        pck.ETHsrc.setAddr(hwaddr);
        pck.ETHtrg.setAddr(clnMac);
        pck.msbPutW(0, ethTyp);
        if (res == null) {
            res = new byte[0];
            pck.putByte(2, 0x1c); // version
            pck.putByte(3, 1); // error
            if (debugger.ifcAtaOE) {
                logger.debug("tx tag=" + t + " err");
            }
        } else {
            pck.putByte(2, 0x18); // version
            pck.putByte(3, 0); // error
            if (debugger.ifcAtaOE) {
                logger.debug("tx tag=" + t + " ok");
            }
        }
        pck.msbPutW(4, shlfId);
        pck.putByte(6, slotId);
        pck.putByte(7, 0); // class
        pck.msbPutD(8, t); // tag
        pck.putSkip(size);
        pck.putCopy(cmd, 0, 0, cmd.length);
        pck.putSkip(cmd.length);
        pck.merge2end();
        pck.putCopy(res, 0, 0, res.length);
        pck.putSkip(res.length);
        pck.merge2end();
        lower.sendPack(pck);
    }

    public void setParent(ifcDn parent) {
        lower = parent;
        hwaddr = (addrMac) lower.getHwAddr();
        clnMac = new addrMac();
    }

    public counter getCounter() {
        return cntr;
    }

    public void setState(state.states stat) {
    }

    public void closeUp() {
    }

}
