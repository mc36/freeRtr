package org.freertr.ifc;

import org.freertr.pack.packHolder;
import org.freertr.enc.encTlv;

/**
 * network service header control protocol (rfc2043) for ppp
 *
 * @author matecsaba
 */
public class ifcPppNsh extends ifcPppNcp {

    /**
     * ppp name
     */
    public final static String pppName = "nshcp";

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
     * clear state
     */
    public void clearState() {
        clearUpperState(false);
    }

    /**
     * get one config
     *
     * @param txReq send request
     * @return data
     */
    public Object getOneConfig(boolean txReq) {
        ifcPppNshConf cfg = new ifcPppNshConf();
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
    public boolean readOption(Object config, encTlv tlv) {
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

class ifcPppNshConf {

    public String toString() {
        return "";
    }

}
