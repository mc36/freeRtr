package org.freertr.serv;

import org.freertr.ifc.ifcDn;
import org.freertr.ifc.ifcEthTyp;
import org.freertr.ifc.ifcEther;
import org.freertr.ifc.ifcNull;
import org.freertr.ifc.ifcUp;
import org.freertr.pack.packHolder;
import org.freertr.util.counter;
import org.freertr.util.state;

/**
 * one p4lang downlink cpuport
 *
 * @author matecsaba
 */
public class servP4langDlnk implements Comparable<servP4langDlnk>, ifcUp {

    /**
     * interface id
     */
    protected final int id;

    private final servP4lang lower;

    private final counter cntr = new counter();

    /**
     * interface handler
     */
    protected ifcDn parent = new ifcNull();

    /**
     * ethertype handler
     */
    protected ifcEthTyp ifc;

    /**
     * create instance
     *
     * @param prnt parent
     * @param num interface id
     */
    protected servP4langDlnk(servP4lang prnt, int num) {
        id = num;
        lower = prnt;
    }

    public int compareTo(servP4langDlnk o) {
        if (id < o.id) {
            return -1;
        }
        if (id > o.id) {
            return +1;
        }
        return 0;
    }

    public void recvPack(packHolder pck) {
        cntr.rx(pck);
        ifcEther.createETHheader(pck, false);
        int i = pck.msbGetD(0);
        pck.getSkip(4);
        ifcEther.parseETHheader(pck, false);
        lower.sendPack(i, pck);
    }

    public void setParent(ifcDn prnt) {
        parent = prnt;
    }

    public void setState(state.states stat) {
    }

    public void closeUp() {
    }

    public counter getCounter() {
        return cntr;
    }

}
