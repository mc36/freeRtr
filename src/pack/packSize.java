package pack;

import pipe.pipeSide;

/**
 * pipe interface blockifier
 *
 * @author matecsaba
 */
public class packSize {

    private pipeSide pipe; // pipeline side

    private int siz; // sizeof header

    private boolean msb; // byte order

    private int mul; // multiplied

    private int sub; // substract

    /**
     * create pipe blockifier
     *
     * @param stream stream to use
     * @param size size of header
     * @param order true for msb, false for lsb
     * @param mult multiplier value
     * @param subs substract value
     */
    public packSize(pipeSide stream, int size, boolean order, int mult, int subs) {
        pipe = stream;
        siz = size;
        msb = order;
        mul = mult;
        sub = subs;
    }

    /**
     * receive one packet
     *
     * @param pck packet to update
     * @return false on success, true on error
     */
    public boolean recvPacket(packHolder pck) {
        pck.clear();
        if (pck.pipeRecv(pipe, 0, siz, 144) != siz) {
            return true;
        }
        int i = 0;
        switch (siz) {
            case 1:
                i = pck.getByte(0);
                break;
            case 2:
                if (msb) {
                    i = pck.msbGetW(0);
                } else {
                    i = pck.lsbGetW(0);
                }
                break;
            case 4:
                if (msb) {
                    i = pck.msbGetD(0);
                } else {
                    i = pck.lsbGetD(0);
                }
                break;
            default:
                return true;
        }
        i = (i - sub) * mul;
        pck.clear();
        return pck.pipeRecv(pipe, 0, i, 144) != i;
    }

    /**
     * receive one packet
     *
     * @return packet if received, null=error
     */
    public packHolder recvPacket() {
        packHolder pck = new packHolder(true, true);
        if (recvPacket(pck)) {
            return null;
        }
        return pck;
    }

    /**
     * send one packet
     *
     * @param pck packet to use
     * @return false on success, true on error
     */
    public boolean sendPacket(packHolder pck) {
        pck.merge2beg();
        int i = pck.dataSize();
        i = (i / mul) + sub;
        switch (siz) {
            case 1:
                pck.putByte(0, i);
                break;
            case 2:
                if (msb) {
                    pck.msbPutW(0, i);
                } else {
                    pck.lsbPutW(0, i);
                }
                break;
            case 4:
                if (msb) {
                    pck.msbPutD(0, i);
                } else {
                    pck.lsbPutD(0, i);
                }
                break;
            default:
                return true;
        }
        pck.putSkip(siz);
        pck.merge2beg();
        if (pck.pipeSend(pipe, 0, pck.dataSize(), 3) != pck.dataSize()) {
            return true;
        }
        return false;
    }

}
