package ifc;

import addr.addrMac;
import pack.packHolder;

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

    private addrMac myaddr;

    private int hello;

    public String toString() {
        return "" + hello;
    }

    /**
     * initialize the crypter
     *
     * @param eth ethertype to use
     * @param tim value to use
     */
    public void doInit(ifcEthTyp eth, int tim) {
        hello = tim;
        try {
            myaddr = (addrMac) eth.getHwAddr().copyBytes();
        } catch (Exception e) {
            myaddr = addrMac.getBroadcast();
        }
    }

    /**
     * encrypt one packet
     *
     * @param pck packet to encrypt
     * @return false on success, true on error
     */
    public synchronized boolean doEncode(packHolder pck) {
//////////////////////////////////
        return false;
    }

    /**
     * decrypt one packet
     *
     * @param pck packet to decrypt
     * @return false on success, true on error
     */
    public synchronized boolean doDecode(packHolder pck) {
        //////////////////////////
        return false;
    }

    /**
     * generate sync packet
     *
     * @return packet to send, null if nothing
     */
    public synchronized packHolder doSync() {
        packHolder pck = new packHolder(true, true);
        doEncode(pck);
        return pck;
    }

}
