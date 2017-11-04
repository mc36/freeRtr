package pack;

import addr.addrMac;

/**
 * wake on lan packet
 *
 * @author matecsaba
 */
public class packWol {

    /**
     * host to wake up
     */
    public addrMac addr = new addrMac();

    /**
     * unicast encapsulation
     */
    public boolean unicast;

    /**
     * create one packet
     *
     * @param pck packet to update
     */
    public void createPayload(packHolder pck) {
        pck.ETHsrc.setAddr(addr);
        if (unicast) {
            pck.ETHtrg.setAddr(addr);
        } else {
            pck.ETHtrg.setAddr(addrMac.getBroadcast());
        }
        pck.ETHtype = 0x0842;
        pck.msbPutW(0, pck.ETHtype);
        pck.msbPutW(2, 0xffff);
        pck.msbPutW(4, 0xffff);
        pck.msbPutW(6, 0xffff);
        pck.putSkip(8);
        for (int i = 0; i < 16; i++) {
            pck.putAddr(0, addr);
            pck.putSkip(addrMac.size);
        }
    }

    /**
     * parse one packet
     *
     * @param pck packet to parse
     * @return false on success, true on error
     */
    public boolean parsePayload(packHolder pck) {
        unicast = pck.ETHtrg.isUnicast();
        for (int i = 0; i < pck.dataSize() - addrMac.sizeX2; i++) {
            if (pck.msbGetW(i + 0) != 0xffff) {
                continue;
            }
            if (pck.msbGetW(i + 2) != 0xffff) {
                continue;
            }
            if (pck.msbGetW(i + 4) != 0xffff) {
                continue;
            }
            pck.getAddr(addr, i + 6);
            return false;
        }
        return true;
    }

}
