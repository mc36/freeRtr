package net.freertr.sec;

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

    protected final ipPrt ipProt;

    public secInfoCls(pipeSide clsP, prtGenConn clsC, ipPrt prtI, ipFwd fwd) {
        fwder = fwd;
        closeC = clsC;
        closeP = clsP;
        ipProt = prtI;
////, addrIP adr, int prt, addrIP loc) {
        //////////////////////////
    }

}
