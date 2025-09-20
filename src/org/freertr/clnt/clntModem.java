package org.freertr.clnt;

import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgDial;
import org.freertr.pack.packRtp;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeModem;
import org.freertr.pipe.pipeSide;
import org.freertr.enc.encCodec;

/**
 * modulator demodulator client
 *
 * @author matecsaba
 */
public class clntModem {

    /**
     * create instance
     */
    public clntModem() {
    }

    /**
     * called number
     */
    public String called = null;

    /**
     * calling number
     */
    public String calling = null;

    private cfgDial pipePer;

    private String pipeRcd;

    private packRtp pipeRtp;

    private pipeSide pipeUsr;

    private pipeSide pipeMdm;

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
            pipePer.stopCall(pipeRcd);
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
            pipeMdm.setClose();
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
        pipeRcd = pipePer.makeCall(calling, called);
        if (pipeRcd == null) {
            return true;
        }
        pipeRtp = pipePer.getCall(pipeRcd);
        encCodec codec = pipePer.getCodec();
        pipeLine pip = new pipeLine(32768, false);
        pipeUsr = pip.getSide();
        pipeMdm = pip.getSide();
        pipeMdm.setReady();
        pipeModem.originate(pipeMdm, codec, pipeRtp);
        return false;
    }

}
