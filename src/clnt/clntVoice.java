package clnt;

import cfg.cfgAll;
import cfg.cfgDial;
import pack.packRtp;
import pipe.pipeLine;
import pipe.pipeSide;
import snd.sndCodec;
import snd.sndCodecG711aLaw;
import snd.sndCodecG711uLaw;
import snd.sndScript;

/**
 *
 * @author matecsaba
 */
public class clntVoice {

    /**
     * called number
     */
    public String called = null;

    /**
     * calling number
     */
    public String calling = null;

    private cfgDial pipePer;

    private packRtp pipeRtp;

    private pipeSide pipeUsr;

    private pipeSide pipeScr;

    /**
     * get pipe side
     *
     * @return pipe side
     */
    public pipeSide getPipe() {
        return pipeUsr;
    }

    /**
     * stop call
     */
    public void callStop() {
        try {
            pipePer.stopCall();
        } catch (Exception e) {
        }
        try {
            pipeRtp.setClose();
        } catch (Exception e) {
        }
        try {
            pipeUsr.setClose();
        } catch (Exception e) {
        }
        try {
            pipeScr.setClose();
        } catch (Exception e) {
        }
    }

    /**
     * start call
     *
     * @return false on success, true on error
     */
    public boolean callStart() {
        pipePer = cfgAll.dialFind(calling, called, null);
        if (pipePer == null) {
            return true;
        }
        if (pipePer.makeCall(calling, called)) {
            return true;
        }
        pipeRtp = pipePer.getCall();
        sndCodec codec = pipePer.getCodec();
        pipeLine pip = new pipeLine(32768, false);
        pipeUsr = pip.getSide();
        pipeScr = pip.getSide();
        pipeScr.timeout = 120000;
        pipeScr.lineTx = pipeSide.modTyp.modeCRLF;
        pipeScr.lineRx = pipeSide.modTyp.modeCRtryLF;
        new sndScript(pipeScr, codec, pipeRtp, calling, called);
        return false;
    }

}
