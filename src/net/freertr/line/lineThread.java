package net.freertr.line;

import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeSide;
import net.freertr.util.debugger;
import net.freertr.util.logger;

/**
 * one line handler thread
 *
 * @author matecsaba
 */
public abstract class lineThread {

    /**
     * create instance
     */
    public lineThread() {
    }

    /**
     * data carrier detect
     */
    public static final int signalDCDi = 0x1;

    /**
     * ring indicator
     */
    public static final int signalRIi = 0x2;

    /**
     * data set ready
     */
    public static final int signalDSRi = 0x10;

    /**
     * data terminal ready
     */
    public static final int signalDTRo = 0x20;

    /**
     * clear to send
     */
    public static final int signalCTSi = 0x100;

    /**
     * ready to send
     */
    public static final int signalRTSo = 0x200;

    /**
     * all input signals
     */
    public static final int ctrlAllIn = signalDSRi | signalCTSi | signalDCDi | signalRIi;

    /**
     * all output signals
     */
    public static final int ctrlAllOut = signalDTRo | signalRTSo;

    /**
     * presence signals
     */
    public static final int ctrlDtrRsr = signalDTRo | signalDSRi;

    /**
     * flow control signals
     */
    public static final int ctrlRtsCts = signalRTSo | signalCTSi;

    /**
     * all signals
     */
    public static final int ctrlAll = ctrlAllIn | ctrlAllOut;

    /**
     * pipe line handler
     */
    protected pipeLine pipe;

    /**
     * server side of pipe
     */
    protected pipeSide pipS;

    /**
     * client side of pipe
     */
    protected pipeSide pipC;

    /**
     * set control bits
     *
     * @param ctrl control bits
     */
    protected abstract void txCtrlBit(int ctrl);

    /**
     * set flow control
     *
     * @param ctrl control bits
     */
    protected abstract void txFlowCtrl(int ctrl);

    /**
     * close the connection
     */
    protected abstract void rxtxClose();

    /**
     * open the connection
     */
    protected abstract void rxtxOpen();

    /**
     * make new pipelines
     *
     * @return pipe side of client
     */
    public synchronized pipeSide getPipe() {
        if (debugger.lineThreadTraf) {
            logger.debug("reopening session");
        }
        rxtxClose();
        pipe = new pipeLine(65536, false);
        pipS = pipe.getSide();
        pipC = pipe.getSide();
        pipS.setReady();
        rxtxOpen();
        return pipC;
    }

}
