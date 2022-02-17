package net.freertr.ifc;

import net.freertr.pack.packHolder;
import net.freertr.util.typLenVal;

/**
 * mpolka control protocol (rfc2043) for ppp
 *
 * @author matecsaba
 */
public class ifcPppMpolka extends ifcPppNcp {

    /**
     * ethertype
     */
    public final static int ethTyp = ifcMpolka.type;

    /**
     * ppp name
     */
    public final static String pppName = "mpolkacp";

    /**
     * ppp data type
     */
    public final static int pppData = 0x4c;

    /**
     * ppp control type
     */
    public final static int pppCtrl = 0x804c;

    /**
     * the constructor
     *
     * @param prnt parent
     */
    public ifcPppMpolka(ifcPpp prnt) {
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
        ifcPppMpolkaConf cfg = new ifcPppMpolkaConf();
        if (!txReq) {
            return cfg;
        }
        return cfg;
    }

    /**
     * read option
     *
     * @param config config
     * @param tlv tlv
     * @return false on success, true on error
     */
    public boolean readOption(Object config, typLenVal tlv) {
        return true;
    }

    /**
     * write option
     *
     * @param pck packet
     * @param data data
     */
    public void writeOptions(packHolder pck, Object data) {
    }

    /**
     * get configure ack
     *
     * @param data data
     * @return false on success, true on error
     */
    public boolean gotConfAck(Object data) {
        return false;
    }

    /**
     * get configure request
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

class ifcPppMpolkaConf {

    public String toString() {
        return "";
    }

}
