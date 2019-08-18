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
        return pppData;
    }

    /**
     * get ethertype
     *
     * @return ethertype
     */
    public int getPPPetyp() {
        return ethTyp;
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
        ifcPppOsiConf cfg = new ifcPppOsiConf();
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
     * got configure nak
     *
     * @param data data
     */
    public void gotConfNak(Object data) {
    }

    /**
     * got configure reject
     *
     * @param data data
     */
    public void gotConfRej(Object data) {
        gotConfNak(data);
    }

    /**
     * got unknown code
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

class ifcPppOsiConf {

    public String toString() {
        return "";
    }

}
