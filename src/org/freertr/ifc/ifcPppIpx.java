package org.freertr.ifc;

import org.freertr.pack.packHolder;
import org.freertr.enc.encTlv;

/**
 * ipx (rfc1552) for ppp
 *
 * @author matecsaba
 */
public class ifcPppIpx extends ifcPppNcp {

    /**
     * ppp name
     */
    public final static String pppName = "ipxcp";

    /**
     * ppp data type
     */
    public final static int pppData = 0x002b;

    /**
     * ppp control type
     */
    public final static int pppCtrl = 0x802b;

    /**
     * the constructor
     *
     * @param prnt parent
     */
    public ifcPppIpx(ifcPpp prnt) {
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
        ifcPppIpxConf cfg = new ifcPppIpxConf();
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

class ifcPppIpxConf {

    public String toString() {
        return "";
    }

}
