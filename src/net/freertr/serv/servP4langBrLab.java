package net.freertr.serv;

import java.util.Comparator;
import net.freertr.ifc.ifcBridgeIfc;

/**
 * one p4lang bridge label
 *
 * @author matecsaba
 */
public class servP4langBrLab implements Comparator<servP4langBrLab> {

    /**
     * interface
     */
    protected final ifcBridgeIfc ifc;

    /**
     * label
     */
    protected int lab;

    /**
     * create instance
     *
     * @param i interface
     * @param l label
     */
    public servP4langBrLab(ifcBridgeIfc i, int l) {
        ifc = i;
        lab = l;
    }

    public int compare(servP4langBrLab o1, servP4langBrLab o2) {
        return o1.ifc.compare(o1.ifc, o2.ifc);
    }

}
