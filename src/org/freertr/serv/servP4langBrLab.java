package org.freertr.serv;

import org.freertr.ifc.ifcBridgeIfc;

/**
 * one p4lang bridge label
 *
 * @author matecsaba
 */
public class servP4langBrLab implements Comparable<servP4langBrLab> {

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

    public int compareTo(servP4langBrLab o) {
        return ifc.compareTo(o.ifc);
    }

}
