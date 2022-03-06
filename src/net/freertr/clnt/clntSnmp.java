package net.freertr.clnt;

import net.freertr.addr.addrIP;
import net.freertr.cfg.cfgAll;
import net.freertr.cry.cryAsn1;
import net.freertr.pack.packHolder;
import net.freertr.pack.packSnmp;
import net.freertr.pipe.pipeDiscard;
import net.freertr.pipe.pipeSide;
import net.freertr.serv.servGeneric;
import net.freertr.user.userTerminal;
import net.freertr.util.bits;
import net.freertr.util.debugger;

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
     * @return false on success, true on error
     */
    public boolean doGet(String o) {
        oid = o;
        return doQuery(packSnmp.typGetReq);
    }

    /**
     * do one get
     *
     * @return false on success, true on error
     */
    public boolean doNext(String o) {
        oid = o;
        return doQuery(packSnmp.typGetNext);
    }

    private boolean doQuery(int cmd) {
        console.linePut("querying " + packSnmp.type2string(cmd) + " " + oid + " at " + server);
        addrIP trg = userTerminal.justResolv(server, 0);
        if (trg == null) {
            return true;
        }
        clntProxy prx = cfgAll.getClntPrx(null);
        if (prx == null) {
            return true;
        }
        pipeSide pipe = prx.doConnect(servGeneric.protoUdp, trg, packSnmp.port, "snmp");
        if (pipe == null) {
            return true;
        }
        pipe.setTime(8000);
        packHolder pckBin = new packHolder(true, true);
        packSnmp pckDat = new packSnmp();
        pckDat.version = 1;
        pckDat.type = cmd;
        pckDat.community = community;
        cryAsn1 cur = new cryAsn1();
        cur.oid = cryAsn1.str2oid(oid);
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
