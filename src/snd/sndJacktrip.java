package snd;

import pack.packHolder;

/**
 * jacktrip protocol
 *
 * @author matecsaba
 */
public class sndJacktrip {

    /**
     * size of header
     */
    public final static int size = 16;

    /**
     * parse one packet
     *
     * @param pck packet to read
     * @return false on success, true on error
     */
    public static boolean parseHeader(packHolder pck) {
//  long timeStamp = pck.msbGetQ(0); // time stamp
//  int seq = pck.msbGetW(8); // sequence number
        int siz = pck.msbGetW(10); // bytes / packet
        if (siz < size) {
            return true;
        }
        if (pck.dataSize() < siz) {
            return true;
        }
        if (pck.getByte(12) != 8) { // sampling rate id
            return true;
        }
        if (pck.getByte(13) != 8) { // bit / sample
            return true;
        }
        if (pck.getByte(14) != 1) { // in channels
            return true;
        }
        if (pck.getByte(15) != 1) { // out channels
            return true;
        }
        pck.getSkip(size);
        pck.setBytesLeft(siz);
        return false;
    }

    /**
     * create header
     *
     * @param pck packet to update
     * @param seq sequence number
     * @param tim time stamp
     */
    public static void createHeader(packHolder pck, int seq, long tim) {
        pck.msbPutQ(0, tim); // time stamp
        pck.msbPutW(8, seq); // sequence number
        pck.msbPutW(10, size + pck.dataSize()); // bytes / packet
        pck.putByte(12, 8); // sampling rate id
        pck.putByte(13, 8); // bit / sample
        pck.putByte(14, 1); // in channels
        pck.putByte(15, 1); // out channels
        pck.putSkip(size);
        pck.merge2beg();
    }

}
