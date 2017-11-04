package pipe;

import ifc.ifcDn;
import ifc.ifcNull;
import ifc.ifcUp;
import pack.packHolder;
import util.counter;
import util.state;

/**
 * interface to pipe converter
 *
 * @author matecsaba
 */
public class pipeIface implements ifcUp, Runnable {

    private ifcDn lower = new ifcNull();

    private counter cntr;

    private pipeLine pipe;

    private pipeSide pipC;

    private pipeSide pipS;

    /**
     * create handler
     */
    public pipeIface() {
        cntr = new counter();
        pipe = new pipeLine(65536, true);
        pipC = pipe.getSide();
        pipS = pipe.getSide();
        new Thread(this).start();
    }

    /**
     * get client pipe side
     *
     * @return pipe side
     */
    public pipeSide getPipe() {
        return pipC;
    }

    public void setParent(ifcDn parent) {
        lower = parent;
    }

    public void setState(state.states stat) {
    }

    public void closeUp() {
        pipe.setClose();
    }

    public counter getCounter() {
        return cntr;
    }

    public void recvPack(packHolder pck) {
        byte[] buf = pck.getCopy();
        pipS.nonBlockPut(buf, 0, buf.length);
    }

    public void run() {
        for (;;) {
            packHolder pck = new packHolder(true, true);
            if (pck.pipeRecv(pipS, 0, 4096, 143) < 1) {
                break;
            }
            lower.sendPack(pck);
        }
    }

}
