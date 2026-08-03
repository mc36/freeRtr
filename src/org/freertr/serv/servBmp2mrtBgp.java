package org.freertr.serv;

import org.freertr.addr.addrIP;
import org.freertr.pack.packHolder;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGenConn;
import org.freertr.rtr.rtrBgp;
import org.freertr.rtr.rtrBgpMon;
import org.freertr.rtr.rtrBgpNeigh;
import org.freertr.rtr.rtrBgpParam;
import org.freertr.rtr.rtrBgpSpeak;
import org.freertr.rtr.rtrBgpUtil;

/**
 * bmp server connection
 *
 * @author matecsaba
 */
public class servBmp2mrtBgp extends servBmp2mrtLstn {

    /**
     * create instance
     *
     * @param pip pipe to use
     * @param prnt parent to use
     * @param id connection to use
     */
    public servBmp2mrtBgp(pipeSide pip, servBmp2mrt prnt, prtGenConn id) {
        super(pip, prnt, id);
    }

    protected void doStart() {
        int safi;
        if (conn.peerAddr.isIPv4()) {
            safi = rtrBgpUtil.safiIp4uni;
        } else {
            safi = rtrBgpUtil.safiIp6uni;
        }
        rtrBgp bgp = new rtrBgp(lower.srvVrf.getFwd(conn.peerAddr), lower.srvVrf, null, 0);
        rtrBgpNeigh nei = new rtrBgpNeigh(bgp, conn.peerAddr.copyBytes());
        nei.localAs = lower.listenAsn;
        nei.addrFams = rtrBgpParam.boolsSet(false);
        nei.addrFams[bgp.safi2idx(safi)] = true;
        rtrBgpSpeak spk = new rtrBgpSpeak(bgp, nei, pipe, 0);
        spk.sendOpen();
        spk.sendKeepAlive();
    }

    protected void doDump(int as, addrIP from, addrIP peer, int typ, packHolder pck) {
        if (typ != rtrBgpMon.typMon) {
            return;
        }
        pck = pck.copyBytes(true, true);
        pck.pipeSend(pipe, 0, pck.dataSize(), 1);
    }

}
