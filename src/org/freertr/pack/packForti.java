package org.freertr.pack;

import org.freertr.pipe.pipeSide;

/**
 * forti protocol
 *
 * @author matecsaba
 */
public class packForti {

    /**
     * size of header
     */
    public final static int size = 6;

    /**
     * header magic
     */
    public final static int magic = 0x5050;

    private final pipeSide pipe;

    /**
     * create new instance
     *
     * @param conn connection to use
     */
    public packForti(pipeSide conn) {
        pipe = conn;
    }

    /**
     * send one packet
     *
     * @param pck packet to end
     */
    public void sendPack(packHolder pck) {
        pck.msbPutW(0, pck.dataSize() + 6);
        pck.msbPutW(2, magic);
        pck.msbPutW(4, pck.dataSize());
        pck.putSkip(size);
        pck.merge2beg();
        pck.pipeSend(pipe, 0, pck.dataSize(), 3);
    }

    /**
     * receive one packet
     *
     * @param pck packet to receive
     * @return false on success, true on error
     */
    public boolean recvPack(packHolder pck) {
        pck.clear();
        if (pck.pipeRecv(pipe, 0, size, 144) != size) {
            return true;
        }
        if (pck.msbGetW(2) != magic) {
            return true;
        }
        int len = pck.msbGetW(0) - size;
        if (pck.msbGetW(4) != len) {
            return true;
        }
        pck.getSkip(size);
        if (len < 1) {
            return false;
        }
        if (pck.pipeRecv(pipe, 0, len, 144) != len) {
            return true;
        }
        return false;
    }

}
