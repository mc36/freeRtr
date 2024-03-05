package org.freertr.serv;

import org.freertr.addr.addrIP;
import org.freertr.pack.packHolder;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGenConn;

/**
 * bmp server connection
 *
 * @author matecsaba
 */
public class servBmp2mrtBmp extends servBmp2mrtLstn {

    /**
     * create instance
     *
     * @param pip pipe to use
     * @param prnt parent to use
     * @param id connection to use
     */
    public servBmp2mrtBmp(pipeSide pip, servBmp2mrt prnt, prtGenConn id) {
        super(pip, prnt, id);
    }

    protected void doStart() {
    }

    protected void doDump(int as, addrIP from, addrIP peer, int typ, packHolder pck) {
        pck = pck.copyBytes(true, true);
        pck.putByte(0, 3); // version
        pck.msbPutD(1, servBmp2mrt.size + pck.dataSize()); // length
        pck.putByte(5, typ); // type
        pck.putSkip(servBmp2mrt.size);
        pck.merge2beg();
        pck.pipeSend(pipe, 0, pck.dataSize(), 1);
    }

}
