package org.freertr.serv;

import org.freertr.addr.addrIP;
import org.freertr.clnt.clntRis;
import org.freertr.pack.packHolder;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGenConn;
import org.freertr.rtr.rtrBgpMon;

/**
 * bmp server connection
 *
 * @author matecsaba
 */
public class servBmp2mrtRis extends servBmp2mrtLstn {

    private final clntRis ris;

    /**
     * create instance
     *
     * @param pip pipe to use
     * @param prnt parent to use
     * @param id connection to use
     */
    public servBmp2mrtRis(pipeSide pip, servBmp2mrt prnt, prtGenConn id) {
        super(pip, prnt, id);
        ris = new clntRis(pip, id.peerAddr.copyBytes());
    }

    protected void doStart() {
        ris.servConnect();
    }

    protected void doDump(int as, addrIP from, addrIP peer, int typ, packHolder pck) {
        if (typ != rtrBgpMon.typMon) {
            return;
        }
        ris.writePacket(pck);
    }

}
