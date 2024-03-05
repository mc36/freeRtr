package org.freertr.prt;

import org.freertr.addr.addrIP;
import org.freertr.addr.addrMac;
import org.freertr.addr.addrType;
import org.freertr.ip.ipIfc;
import org.freertr.pack.packHolder;
import org.freertr.util.bits;

/**
 * arp/nd ping
 *
 * @author matecsaba
 */
public class prtArping {

    private final ipIfc ipi;

    private final addrIP trg;

    /**
     * time to wait before querying
     */
    public int delay = 1000;

    /**
     * create instance
     *
     * @param ipif ip interface to use
     * @param addr address to ping
     */
    public prtArping(ipIfc ipif, addrIP addr) {
        ipi = ipif;
        trg = addr;
    }

    /**
     * do one round
     *
     * @return old then current, both could be null
     */
    public addrType[] doRound() {
        addrType o = ipi.getL2info(trg);
        ipi.updateL2info(2, new addrMac(), trg);
        ipi.createETHheader(new packHolder(true, true), trg, 0);
        bits.sleep(delay);
        addrType n = ipi.getL2info(trg);
        addrType[] res = new addrType[2];
        res[0] = o;
        res[1] = n;
        return res;
    }

}
