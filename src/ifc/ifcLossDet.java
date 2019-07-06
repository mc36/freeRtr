package ifc;

import addr.addrMac;
import pack.packHolder;
import user.userFormat;
import util.bits;
import util.logger;

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

    /**
     * lost packets
     */
    public int packets;

    /**
     * time for block
     */
    public int blocking;

    private long blocked;

    private addrMac myaddr = addrMac.getRandom();

    private addrMac bcast = addrMac.getBroadcast();

    private int rxMine;

    private int txMine;

    private int rxRem;

    private int txRem;

    private int rxMineO;

    private int txMineO;

    private int rxRemO;

    private int txRemO;
    
    private int rxBlock;
    
    private int txBlock;

    private ifcEthTyp upper;

    public String toString() {
        if (packets < 1) {
            return "";
        }
        return packets + " " + blocking;
    }

    /**
     * initialize the engine
     *
     * @param eth ethertype to use
     */
    public void doInit(ifcEthTyp eth) {
        upper = eth;
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
        l.add("block|" + txBlock + "|" + rxBlock);
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
        return blocked > 0;
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
        long tim = bits.getTime();
        if (packets > 0) {
            int i = rxMine - rxMineO;
            int o = txRem - txRemO;
            if ((o - i) > packets) {
                logger.info("blocking " + upper + " because of rx loss");
                rxBlock++;
                blocked = tim;
            }
            i = rxRem - rxRemO;
            o = txMine - txMineO;
            if ((o - i) > packets) {
                logger.info("blocking " + upper + " because of tx loss");
                txBlock++;
                blocked = tim;
            }
        }
        if (blocked > 0) {
            if ((tim - blocked) > blocking) {
                logger.info("unblocking " + upper);
                blocked = 0;
            }
        }
        rxMineO = rxMine;
        txMineO = txMine;
        rxRemO = rxRem;
        txRemO = txRem;
        return pck;
    }

}
