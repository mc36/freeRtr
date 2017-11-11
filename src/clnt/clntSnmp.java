package clnt;

import cry.cryAsn1;
import pack.packHolder;
import pack.packSnmp;
import pipe.pipeProgress;
import pipe.pipeSide;
import serv.servGeneric;
import user.userTerminal;
import util.bits;
import util.debugger;

/**
 * simple network management protocol (rfc1157) client
 *
 * @author matecsaba
 */
public class clntSnmp {

    /**
     * console to use
     */
    public pipeProgress cons;

    /**
     * host to query
     */
    public String host;

    /**
     * community to use
     */
    public String community;

    /**
     * oid to query
     */
    public String oid;

    /**
     * result
     */
    public String result;

    /**
     * do one get
     *
     * @return false on success, true on error
     */
    public boolean doGet() {
        return doQuery(packSnmp.typGetReq);
    }

    /**
     * do one get
     *
     * @return false on success, true on error
     */
    public boolean doNext() {
        return doQuery(packSnmp.typGetNext);
    }

    private boolean doQuery(int cmd) {
        pipeSide pipe = new userTerminal(cons).resolvAndConn(servGeneric.protoUdp, host, packSnmp.port, "snmp");
        if (pipe == null) {
            return true;
        }
        pipe.timeout = 8000;
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
            cons.debugTx("" + pckDat);
        }
        pckBin.pipeSend(pipe, 0, pckBin.dataSize(), 2);
        pckBin = pipe.readPacket(true);
        pipe.setClose();;
        if (pckBin == null) {
            cons.debugRes("got no packet");
            return true;
        }
        if (pckDat.parsePacket(pckBin)) {
            cons.debugRes("got bad packet");
            return true;
        }
        if (debugger.clntSnmpTraf) {
            cons.debugRx("" + pckDat);
        }
        cons.debugRes("" + pckDat.res);
        return false;
    }

}
