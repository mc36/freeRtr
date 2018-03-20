package ifc;

import ip.ipMpls;
import pack.packHolder;
import util.typLenVal;

/**
 * multiprotocol label switching control protocol (rfc3032) for ppp
 *
 * @author matecsaba
 */
public class ifcPppMpls extends ifcPppNcp {

    /**
     * ethertype (unicast)
     */
    public final static int ethTypU = ipMpls.typeU;

    /**
     * ethertype (multicast)
     */
    public final static int ethTypM = ipMpls.typeM;

    /**
     * ppp name
     */
    public final static String pppName = "mplscp";

    /**
     * ppp data type (unicast)
     */
    public final static int pppDataU = 0x0281;

    /**
     * ppp data type (multicast)
     */
    public final static int pppDataM = 0x0283;

    /**
     * ppp control type
     */
    public final static int pppCtrl = 0x8281;

    /**
     * the constructor
     *
     * @param prnt parent
     */
    public ifcPppMpls(ifcPpp prnt) {
        parent = prnt;
    }

    public String getPPPname() {
        return pppName;
    }

    public int getPPPctrl() {
        return pppCtrl;
    }

    public int getPPPdata() {
        return pppDataU;
    }

    public int getPPPetyp() {
        return ethTypU;
    }

    public void clearState() {
        clearUpperState();
    }

    public Object getOneConfig(boolean txReq) {
        ifcPppMplsConf cfg = new ifcPppMplsConf();
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

class ifcPppMplsConf {

    public String toString() {
        return "";
    }

}
