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

    /**
     * get name
     *
     * @return name
     */
    public String getPPPname() {
        return pppName;
    }

    /**
     * get control code
     *
     * @return code
     */
    public int getPPPctrl() {
        return pppCtrl;
    }

    /**
     * get data code
     *
     * @return code
     */
    public int getPPPdata() {
        return pppDataU;
    }

    /**
     * get ethertype
     *
     * @return ethertype
     */
    public int getPPPetyp() {
        return ethTypU;
    }

    /**
     * clear state
     */
    public void clearState() {
        clearUpperState();
    }

    /**
     * get one config
     *
     * @param txReq send request
     * @return data
     */
    public Object getOneConfig(boolean txReq) {
        ifcPppMplsConf cfg = new ifcPppMplsConf();
        if (!txReq) {
            return cfg;
        }
        return cfg;
    }

    /**
     * read options
     *
     * @param config config
     * @param tlv tlv
     * @return false on success, true on error
     */
    public boolean readOption(Object config, typLenVal tlv) {
        return true;
    }

    /**
     * write options
     *
     * @param pck packet
     * @param data data
     */
    public void writeOptions(packHolder pck, Object data) {
    }

    /**
     * got configure ack
     *
     * @param data data
     * @return false on success, true on error
     */
    public boolean gotConfAck(Object data) {
        return false;
    }

    /**
     * got configure request
     *
     * @param data data
     * @param rej rejected
     * @return data
     */
    public Object gotConfReq(Object data, boolean rej) {
        if (rej) {
            return null;
        }
        return null;
    }

    /**
     * get configure nak
     *
     * @param data data
     */
    public void gotConfNak(Object data) {
    }

    /**
     * get configure reject
     *
     * @param data data
     */
    public void gotConfRej(Object data) {
        gotConfNak(data);
    }

    /**
     * get unknown code
     *
     * @param pck packet
     * @param code code
     * @param id id
     * @return false on success, true on error
     */
    public boolean gotUnknownCode(packHolder pck, int code, int id) {
        return true;
    }

}

class ifcPppMplsConf {

    public String toString() {
        return "";
    }

}
