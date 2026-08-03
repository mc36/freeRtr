package org.freertr.serv;

import org.freertr.cfg.cfgBrdg;
import org.freertr.ifc.ifcBridgeAdr;
import org.freertr.ifc.ifcBridgeIfc;
import org.freertr.rtr.rtrBgpEvpnPeer;
import org.freertr.tab.tabGen;

/**
 * one p4lang bridge
 *
 * @author matecsaba
 */
public class servP4langBr implements Comparable<servP4langBr> {

    /**
     * bridge id
     */
    protected final int id;

    /**
     * configuration
     */
    protected cfgBrdg br;

    /**
     * routing bridge
     */
    protected servP4langIfc routed;

    /**
     * label sent
     */
    protected boolean sentLab;

    /**
     * exported macs
     */
    protected tabGen<ifcBridgeAdr> macs = new tabGen<ifcBridgeAdr>();

    /**
     * exported interfaces
     */
    protected tabGen<ifcBridgeIfc> ifcs = new tabGen<ifcBridgeIfc>();

    /**
     * exported labels
     */
    protected tabGen<servP4langBrLab> labs = new tabGen<servP4langBrLab>();

    /**
     * create instance
     *
     * @param i id
     */
    protected servP4langBr(int i) {
        id = i;
    }

    public int compareTo(servP4langBr o) {
        if (id < o.id) {
            return -1;
        }
        if (id > o.id) {
            return +1;
        }
        return 0;
    }

    /**
     * find interface
     *
     * @param lab label
     * @return true if found, false if not
     */
    protected boolean findIfc(int lab) {
        for (int i = 0; i < ifcs.size(); i++) {
            try {
                rtrBgpEvpnPeer ifc = (rtrBgpEvpnPeer) ifcs.get(i).lowerIf;
                if (ifc.getLabelLoc() == lab) {
                    return true;
                }
            } catch (Exception e) {
            }
        }
        return false;
    }

    /**
     * clear everything
     */
    protected void doClear() {
        ifcs = new tabGen<ifcBridgeIfc>();
        macs = new tabGen<ifcBridgeAdr>();
        labs = new tabGen<servP4langBrLab>();
        sentLab = false;
    }

}
