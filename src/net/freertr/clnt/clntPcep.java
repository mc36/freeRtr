package net.freertr.clnt;

import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgIfc;
import net.freertr.cfg.cfgVrf;
import net.freertr.pack.packPcep;
import net.freertr.pipe.pipeSide;
import net.freertr.serv.servGeneric;
import net.freertr.tab.tabHop;
import net.freertr.user.userTerminal;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.debugger;
import net.freertr.util.logger;

/**
 * path computation element protocol (rfc5440) client
 *
 * @author matecsaba
 */
public class clntPcep {

    /**
     * create instance
     */
    public clntPcep() {
    }

    /**
     * proxy profile
     */
    public clntProxy proxy;

    /**
     * peer to use
     */
    public String target;

    private pipeSide pipe;

    /**
     * set target and proxy
     *
     * @param s string
     */
    public void setTarget(String s) {
        cmds cmd = new cmds("trg", s);
        target = cmd.word();
        proxy = cfgAll.getClntPrx(null);
        cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
        if (vrf == null) {
            return;
        }
        cfgIfc ifc = cfgAll.ifcFind(cmd.word(), 0);
        proxy = clntProxy.makeTemp(vrf, ifc);
    }

    /**
     * connect peer
     *
     * @return false on success, true on error
     */
    public boolean doConnect() {
        if (debugger.clntPcepTraf) {
            logger.debug("resolving " + target);
        }
        addrIP trg = userTerminal.justResolv(target, proxy.prefer);
        if (trg == null) {
            return true;
        }
        if (debugger.clntPcepTraf) {
            logger.debug("connecting " + trg);
        }
        pipe = proxy.doConnect(servGeneric.protoTcp, trg, packPcep.port, "pcep");
        if (pipe == null) {
            return true;
        }
        pipe.setTime(120000);
        pipe.lineRx = pipeSide.modTyp.modeCR;
        pipe.lineTx = pipeSide.modTyp.modeCR;
        packPcep pck = new packPcep();
        pck.pipe = pipe;
        pck.createOpen();
        if (debugger.clntPcepTraf) {
            logger.debug("tx " + pck);
        }
        pck.sendPack();
        pck.createKeep();
        if (debugger.clntPcepTraf) {
            logger.debug("tx " + pck);
        }
        pck.sendPack();
        if (pck.recvPack()) {
            pipe.setClose();
            return true;
        }
        if (debugger.clntPcepTraf) {
            logger.debug("rx " + pck);
        }
        if (pck.msgTyp != packPcep.typOpen) {
            pipe.setClose();
            return true;
        }
        return false;
    }

    /**
     * close peer
     */
    public void doClose() {
        if (pipe == null) {
            return;
        }
        pipe.setClose();
    }

    /**
     * keepalive peer
     */
    public void doKeepalive() {
        if (pipe == null) {
            return;
        }
        packPcep pck = new packPcep();
        pck.pipe = pipe;
        pck.createKeep();
        if (debugger.clntPcepTraf) {
            logger.debug("tx " + pck);
        }
        pck.sendPack();
    }

    /**
     * query peer
     *
     * @param setupType setup type, 0=te, 1=sr
     * @param srcAddr source address
     * @param trgAddr target address
     * @param exclAny exclude any
     * @param inclAny include any
     * @param inclAll include all
     * @param priSet setup priority
     * @param priHld hold priority
     * @param bandwidth bandwidth
     * @param metTyp metric type, 1=igp, 2=te, 3=hops
     * @param metVal metric value
     * @return list of hops, null on error
     */
    public List<tabHop> doCompute(int setupType, addrIP srcAddr, addrIP trgAddr, int exclAny, int inclAny, int inclAll, int priSet, int priHld, float bandwidth, int metTyp, int metVal) {
        if (pipe == null) {
            return null;
        }
        int reqId = bits.randomD();
        packPcep pck = new packPcep();
        pck.pipe = pipe;
        pck.setupType = setupType;
        pck.srcAddr = srcAddr;
        pck.trgAddr = trgAddr;
        pck.exclAny = exclAny;
        pck.inclAny = inclAny;
        pck.inclAll = inclAll;
        pck.priSet = priSet;
        pck.priHld = priHld;
        pck.bandwidth = bandwidth;
        pck.metTyp = metTyp;
        pck.metVal = metVal;
        pck.loose = true;
        pck.isIP4 = trgAddr.isIPv4();
        pck.reqId = reqId;
        pck.plspId = (bits.randomW() << 16) | 0x18;
        pck.createRequest();
        if (debugger.clntPcepTraf) {
            logger.debug("tx " + pck);
        }
        pck.sendPack();
        for (;;) {
            pck = new packPcep();
            pck.pipe = pipe;
            if (pck.recvPack()) {
                return null;
            }
            if (debugger.clntPcepTraf) {
                logger.debug("rx " + pck);
            }
            if (pck.msgTyp == packPcep.typRep) {
                break;
            }
            if (pck.msgTyp == packPcep.typErr) {
                break;
            }
        }
        if (pck.reqId != reqId) {
            return null;
        }
        return pck.ero;
    }

}
