package ifc;

import pack.packHolder;
import rtr.rtrIsis;
import util.typLenVal;

/**
 * open systems interconnect (rfc1377) for ppp
 *
 * @author matecsaba
 */
public class ifcPppOsi extends ifcPppNcp {

    /**
     * ethertype
     */
    public final static int ethTyp = rtrIsis.ethTyp;

    /**
     * ppp name
     */
    public final static String pppName = "osicp";

    /**
     * ppp data type
     */
    public final static int pppData = 0x0023;

    /**
     * ppp control type
     */
    public final static int pppCtrl = 0x8023;

    /**
     * the constructor
     *
     * @param prnt parent
     */
    public ifcPppOsi(ifcPpp prnt) {
        parent = prnt;
    }

    public String getPPPname() {
        return pppName;
    }

    public int getPPPctrl() {
        return pppCtrl;
    }

    public int getPPPdata() {
        return pppData;
    }

    public int getPPPetyp() {
        return ethTyp;
    }

    public void clearState() {
        clearUpperState();
    }

    public Object getOneConfig(boolean txReq) {
        ifcPppOsiConf cfg = new ifcPppOsiConf();
        if (!txReq) {
            return cfg;
        }
        return cfg;
    }

    public boolean readOption(Object config, typLenVal tlv) {
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

class ifcPppOsiConf {

    public String toString() {
        return "";
    }

}
