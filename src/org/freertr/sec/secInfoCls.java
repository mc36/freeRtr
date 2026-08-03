package org.freertr.sec;

import org.freertr.addr.addrIP;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipPrt;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGenConn;

/**
 * generic information closer
 *
 * @author matecsaba
 */
public class secInfoCls {

    /**
     * forwarder used
     */
    protected final ipFwd fwder;

    /**
     * pipeside to close
     */
    protected final pipeSide closeP;

    /**
     * connection to close
     */
    protected final prtGenConn closeC;

    /**
     * protocol used
     */
    protected final ipPrt protIp;

    /**
     * remote address
     */
    protected final addrIP remote;

    /**
     * source address
     */
    protected final addrIP local;

    /**
     * protocol number
     */
    protected final int protNum;

    /**
     * create instance
     *
     * @param clsP pipe to clear
     * @param clsC connection to clear
     * @param prtI protocol used
     * @param fwd forwarder to use
     * @param rem address to check
     * @param prtN protocol number to check
     * @param loc local address
     */
    public secInfoCls(pipeSide clsP, prtGenConn clsC, ipPrt prtI, ipFwd fwd, addrIP rem, int prtN, addrIP loc) {
        if ((prtN == 0) && (prtI != null)) {
            prtN = prtI.getProtoNum();
        }
        fwder = fwd;
        closeC = clsC;
        closeP = clsP;
        protIp = prtI;
        protNum = prtN;
        remote = rem.copyBytes();
        local = loc.copyBytes();
    }

}
