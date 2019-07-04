package ifc;

import addr.addrMac;
import pack.packHolder;
import user.userFormat;

/**
 * packet loss detector
 *
 * @author matecsaba
 */
public class ifcLossDet {

    /**
     * size of header
     */
    public final static int size = 8;

    private addrMac myaddr = addrMac.getRandom();

    private addrMac bcast = addrMac.getBroadcast();

    private int rxMine;

    private int txMine;

    private int rxRem;

    private int txRem;

    public String toString() {
        return "";
    }

    /**
     * initialize the engine
     *
     * @param eth ethertype to use
     */
    public void doInit(ifcEthTyp eth) {
        try {
            myaddr = (addrMac) eth.getHwAddr().copyBytes();
        } catch (Exception e) {
            myaddr = addrMac.getBroadcast();
        }
    }

    /**
     * get show
     *
     * @param l list
     */
    public void getShow(userFormat l) {
        l.add("local|" + txMine + "|" + rxMine);
        l.add("remote|" + txRem + "|" + rxRem);
    }

    /**
     * encode one packet
     *
     * @param pck packet to encrypt
     * @return false on success, true on error
     */
    public synchronized boolean doEncode(packHolder pck) {
        pck.msbPutD(0, rxMine);
        pck.msbPutD(4, txMine);
        pck.putSkip(size);
        pck.merge2beg();
        txMine++;
        return false;
    }

    /**
     * decode one packet
     *
     * @param pck packet to decrypt
     * @return false on success, true on error
     */
    public synchronized boolean doDecode(packHolder pck) {
        if (pck.dataSize() < size) {
            return true;
        }
        rxMine++;
        rxRem = pck.msbGetD(0);
        txRem = pck.msbGetD(4);
        pck.getSkip(size);
        return false;
    }

    /**
     * generate sync packet
     *
     * @return packet to send, null if nothing
     */
    public synchronized packHolder doSync() {
        packHolder pck = new packHolder(true, true);
        pck.msbPutW(0, -1);
        pck.putSkip(2);
        pck.merge2beg();
        pck.ETHsrc.setAddr(myaddr);
        pck.ETHtrg.setAddr(bcast);
        doEncode(pck);
        return pck;
    }

}
