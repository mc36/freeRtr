package net.freertr.serv;

import java.util.Comparator;
import net.freertr.cfg.cfgBrdg;
import net.freertr.ifc.ifcBridgeAdr;
import net.freertr.ifc.ifcBridgeIfc;
import net.freertr.rtr.rtrBgpEvpnPeer;
import net.freertr.tab.tabGen;

/**
 * one p4lang bridge
 *
 * @author matecsaba
 */
public class servP4langBr implements Comparator<servP4langBr> {

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
    protected boolean routed;

    /**
     * exported macs
     */
    protected tabGen<ifcBridgeAdr> macs = new tabGen<ifcBridgeAdr>();

    /**
     * exported interfaces
     */
    protected tabGen<ifcBridgeIfc> ifcs = new tabGen<ifcBridgeIfc>();

    /**
     * create instance
     *
     * @param i id
     */
    protected servP4langBr(int i) {
        id = i;
    }

    public int compare(servP4langBr o1, servP4langBr o2) {
        if (o1.id < o2.id) {
            return -1;
        }
        if (o1.id > o2.id) {
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
    }

}
