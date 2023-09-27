package net.freertr.sec;

import net.freertr.addr.addrIP;
import net.freertr.ip.ipFwd;
import net.freertr.ip.ipPrt;
import net.freertr.pipe.pipeSide;
import net.freertr.prt.prtGenConn;

/**
 * generic information closer
 *
 * @author matecsaba
 */
public class secInfoCls {

    protected final ipFwd fwder;

    protected final pipeSide closeP;

    protected final prtGenConn closeC;

    protected final ipPrt protIp;

    protected final addrIP remote;

    protected final addrIP local;

    protected final int protNum;

    /**
     * 
     * @param clsP
     * @param clsC
     * @param prtI
     * @param fwd forwarder to use
     * @param adr address to check
     * @param prt protocol number to check
     * @param loc local address
     */
    public secInfoCls(pipeSide clsP, prtGenConn clsC, ipPrt prtI, ipFwd fwd, addrIP rem, int prt, addrIP loc) {
        fwder = fwd;
        closeC = clsC;
        closeP = clsP;
        protIp = prtI;
        protNum = prt;
        remote = rem.copyBytes();
        local = loc.copyBytes();
    }

}
