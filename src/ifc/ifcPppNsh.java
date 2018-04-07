package ifc;

import pack.packHolder;
import util.typLenVal;

/**
 * network service header control protocol (rfc2043) for ppp
 *
 * @author matecsaba
 */
public class ifcPppNsh extends ifcPppNcp {

    /**
     * ethertype
     */
    public final static int ethTyp = ifcNshFwd.type;

    /**
     * ppp name
     */
    public final static String pppName = "scp";

    /**
     * ppp data type
     */
    public final static int pppData = 0x4b;

    /**
     * ppp control type
     */
    public final static int pppCtrl = 0x804b;

    /**
     * the constructor
     *
     * @param prnt parent
     */
    public ifcPppNsh(ifcPpp prnt) {
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
        ifcPppNshConf cfg = new ifcPppNshConf();
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

class ifcPppNshConf {

    public String toString() {
        return "";
    }

}
