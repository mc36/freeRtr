package org.freertr.ifc;

import org.freertr.pack.packHolder;
import org.freertr.enc.encTlv;

/**
 * encryption control protocol (rfc1968) for ppp
 *
 * @author matecsaba
 */
public class ifcPppCrypt extends ifcPppNcp {

    /**
     * ppp name
     */
    public final static String pppName = "ecp";

    /**
     * ppp data type
     */
    public final static int pppData = 0x53;

    /**
     * ppp control type
     */
    public final static int pppCtrl = 0x8053;

    /**
     * the constructor
     *
     * @param prnt parent
     */
    public ifcPppCrypt(ifcPpp prnt) {
        parent = prnt;
    }

    public String getPPPname() {
        return pppName;
    }

    public int getPPPctrl() {
        return pppCtrl;
    }

    public void clearState() {
        clearUpperState(false);
    }

    public Object getOneConfig(boolean txReq) {
        ifcPppCryptConf cfg = new ifcPppCryptConf();
        if (!txReq) {
            return cfg;
        }
        return cfg;
    }

    public boolean readOption(Object config, encTlv tlv) {
        return true;
    }

    public void writeOptions(packHolder pck, Object data) {
    }

    public boolean gotConfAck(Object data) {
        return false;
    }

    public Object gotConfReq(Object data, boolean rej) {
        if (rej) {
            return null;
        }
        return null;
    }

    public void gotConfNak(Object data) {
    }

    public void gotConfRej(Object data) {
        gotConfNak(data);
    }

    public boolean gotUnknownCode(packHolder pck, int code, int id) {
        return true;
    }

}

class ifcPppCryptConf {

    public String toString() {
        return "";
    }

}
