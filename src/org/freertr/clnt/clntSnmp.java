package org.freertr.clnt;

import org.freertr.addr.addrIP;
import org.freertr.enc.encAsn1;
import org.freertr.pack.packHolder;
import org.freertr.pack.packSnmp;
import org.freertr.pipe.pipeDiscard;
import org.freertr.pipe.pipeSide;
import org.freertr.serv.servGeneric;
import org.freertr.util.bits;
import org.freertr.util.debugger;

/**
 * simple network management protocol (rfc1157) client
 *
 * @author matecsaba
 */
public class clntSnmp {

    private final clntProxy proxy;

    private final String server;

    private final pipeSide console;

    /**
     * /**
     * create new client
     *
     * @param con console to log
     * @param srv server to use
     * @param prx proxy to use
     */
    public clntSnmp(pipeSide con, clntProxy prx, String srv) {
        console = pipeDiscard.needAny(con);
        server = srv;
        proxy = prx;
    }

    /**
     * community to use
     */
    public String community;

    /**
     * oid
     */
    private String oid;

    /**
     * result
     */
    public String result;

    /**
     * do one get
     *
     * @param o oid
     * @return false on success, true on error
     */
    public boolean doGet(String o) {
        oid = o;
        return doQuery(packSnmp.typGetReq);
    }

    /**
     * do one get
     *
     * @param o oid
     * @return false on success, true on error
     */
    public boolean doNext(String o) {
        oid = o;
        return doQuery(packSnmp.typGetNext);
    }

    private boolean doQuery(int cmd) {
        console.linePut("querying " + packSnmp.type2string(cmd) + " " + oid + " at " + server);
        addrIP trg = clntDns.justResolv(server, 0);
        if (trg == null) {
            return true;
        }
        if (proxy == null) {
            return true;
        }
        pipeSide pipe = proxy.doConnect(servGeneric.protoUdp, trg, packSnmp.port, "snmp");
        if (pipe == null) {
            return true;
        }
        pipe.setTime(8000);
        packHolder pckBin = new packHolder(true, true);
        packSnmp pckDat = new packSnmp();
        pckDat.version = 1;
        pckDat.type = cmd;
        pckDat.community = community;
        encAsn1 cur = new encAsn1();
        cur.oid = encAsn1.str2oid(oid);
        pckDat.res.add(cur);
        pckDat.reqId = bits.randomW();
        if (pckDat.createPacket(pckBin)) {
            return true;
        }
        if (debugger.clntSnmpTraf) {
            console.linePut("" + pckDat);
        }
        pckBin.pipeSend(pipe, 0, pckBin.dataSize(), 2);
        pckBin = pipe.readPacket(true);
        pipe.setClose();
        if (pckBin == null) {
            console.linePut("got no packet");
            return true;
        }
        if (pckDat.parsePacket(pckBin)) {
            console.linePut("got bad packet");
            return true;
        }
        if (debugger.clntSnmpTraf) {
            console.linePut("" + pckDat);
        }
        console.linePut("" + pckDat.res);
        return false;
    }

}
