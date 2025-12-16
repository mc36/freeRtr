package org.freertr.pack;

import org.freertr.pipe.pipeSide;

/**
 * streaming telemetry packet
 *
 * @author matecsaba
 */
public class packStreamingMdt {

    /**
     * size of header
     */
    public final static int size = 12;

    /**
     * message type, 1=report
     */
    public int typ;

    /**
     * encapsulation, 1=(kv)gpb, 2=json
     */
    public int encap;

    /**
     * header version, 1=curr
     */
    public int vers;

    /**
     * flags, 0=none
     */
    public int flags;

    private pipeSide pipe;

    private packHolder pack;

    /**
     * create new instance
     *
     * @param conn pipeline
     * @param pck packet to use
     */
    public packStreamingMdt(pipeSide conn, packHolder pck) {
        pipe = conn;
        pack = pck;
    }

    /**
     * receive one packet
     *
     * @return false on success, true on error
     */
    public boolean recvPack() {
        pack.clear();
        if (pack.pipeRecv(pipe, 0, size, 144) != size) {
            pipe.setClose();
            return true;
        }
        typ = pack.msbGetW(0);
        encap = pack.msbGetW(2);
        vers = pack.msbGetW(4);
        flags = pack.msbGetW(6);
        int siz = pack.msbGetD(8);
        pack.clear();
        if (pack.pipeRecv(pipe, 0, siz, 144) != siz) {
            pipe.setClose();
            return true;
        }
        return false;
    }

    /**
     * send one packet
     *
     * @return true on error, false on success
     */
    public boolean sendPack() {
        pack.merge2beg();
        pack.msbPutW(0, typ);
        pack.msbPutW(2, encap);
        pack.msbPutW(4, vers);
        pack.msbPutW(6, flags);
        pack.msbPutD(8, pack.dataSize());
        pack.putSkip(size);
        pack.merge2beg();
        pack.pipeSend(pipe, 0, pack.dataSize(), 3);
        return false;
    }

}
