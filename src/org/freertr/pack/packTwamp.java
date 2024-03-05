package org.freertr.pack;

/**
 * two way measurement protocol (rfc5357) packet
 *
 * @author matecsaba
 */
public class packTwamp {

    /**
     * create instance
     */
    public packTwamp() {
    }

    /**
     * port number
     */
    public final static int port = 862;

    /**
     * header size
     */
    public final static int size = 41;

    /**
     * error magic
     */
    public final static int errMag = 0x3fff;

    /**
     * sequence number
     */
    public int sequence;

    /**
     * timestamp
     */
    public long timestmp;

    /**
     * error estimate
     */
    public int errEst;

    /**
     * receive timestamp
     */
    public long timesRx;

    /**
     * sender sequence number
     */
    public int seqTx;

    /**
     * sender timestamp
     */
    public long timesTx;

    /**
     * sender error estimate
     */
    public int errTx;

    /**
     * sender ttl
     */
    public int ttlTx;

    /**
     * parse header
     *
     * @param pck packet to read
     * @return return false if successful, true if error happened
     */
    public boolean parseHeader(packHolder pck) {
        if (pck.dataSize() < size) {
            return true;
        }
        sequence = pck.msbGetD(0);
        timestmp = pck.msbGetQ(4);
        errEst = pck.msbGetW(12);
        timesRx = pck.msbGetQ(16);
        seqTx = pck.msbGetD(24);
        timesTx = pck.msbGetQ(28);
        errTx = pck.msbGetW(36);
        ttlTx = pck.getByte(40);
        pck.getSkip(size);
        return false;
    }

    /**
     * create header
     *
     * @param pck packet to read
     */
    public void createHeader(packHolder pck) {
        pck.msbPutD(0, sequence);
        pck.msbPutQ(4, timestmp);
        pck.msbPutW(12, errEst);
        pck.msbPutW(14, 0); // mbz
        pck.msbPutQ(16, timesRx);
        pck.msbPutD(24, seqTx);
        pck.msbPutQ(28, timesTx);
        pck.msbPutW(36, errTx);
        pck.msbPutW(38, 0); // mbz
        pck.putByte(40, ttlTx);
        pck.putSkip(size);
        pck.merge2beg();
    }

}
