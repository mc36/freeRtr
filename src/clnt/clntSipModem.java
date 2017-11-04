package clnt;

import addr.addrIP;
import cfg.cfgAll;
import cfg.cfgIfc;
import cfg.cfgVrf;
import ip.ipFwdIface;
import ip.ipFwdTab;
import pack.packRtp;
import pack.packSip;
import pipe.pipeLine;
import pipe.pipeModem;
import pipe.pipeSide;
import prt.prtUdp;
import serv.servSipModem;
import snd.sndCodec;
import snd.sndCodecG711aLaw;
import snd.sndCodecG711uLaw;
import user.userTerminal;
import util.bits;
import util.cmds;
import util.debugger;
import util.logger;

/**
 * session initiation protocol (rfc3261) client
 *
 * @author matecsaba
 */
public class clntSipModem implements Runnable {

    /**
     * target server
     */
    public String target = null;

    /**
     * called number
     */
    public String called = null;

    /**
     * calling number
     */
    public String calling = null;

    /**
     * vrf of target
     */
    public cfgVrf vrf = null;

    /**
     * source interface
     */
    public cfgIfc srcIfc = null;

    /**
     * codec, true=alaw, false=ulaw
     */
    public boolean aLaw = true;

    private pipeSide pipeSip;

    private packRtp pipeRtp;

    private int cid;

    private pipeSide pipeUsr;

    private pipeSide pipeMdm;

    /**
     * get pipe side
     *
     * @return pipe side
     */
    public pipeSide getPipe() {
        return pipeUsr;
    }

    private String getCID() {
        return cid + "@" + cfgAll.hostName;
    }

    /**
     * stop call
     */
    public void callStop() {
        try {
            packSip sip = new packSip(pipeSip);
            pipeSip.timeout = 10000;
            sip.makeReq("BYE", calling, called, null, getCID(), cid + 2);
            sip.writeDown();
            if (debugger.clntSipModemTraf) {
                sip.dump("tx");
            }
            sip.readUp();
            if (debugger.clntSipModemTraf) {
                sip.dump("rx");
            }
        } catch (Exception e) {
        }
        try {
            pipeSip.setClose();
        } catch (Exception e) {
        }
        try {
            pipeRtp.setClose();
        } catch (Exception e) {
        }
        try {
            pipeUsr.setClose();
        } catch (Exception e) {
        }
        try {
            pipeMdm.setClose();
        } catch (Exception e) {
        }
    }

    /**
     * start call
     *
     * @return false on success, true on error
     */
    public boolean callStart() {
        if (vrf == null) {
            return true;
        }
        sndCodec codec;
        if (aLaw) {
            codec = new sndCodecG711aLaw();
        } else {
            codec = new sndCodecG711uLaw();
        }
        pipeLine pip = new pipeLine(32768, false);
        pipeUsr = pip.getSide();
        pipeMdm = pip.getSide();
        pipeMdm.setReady();
        if (target == null) {
            return true;
        }
        addrIP trg = userTerminal.justResolv(target, 0);
        if (trg == null) {
            return true;
        }
        ipFwdIface fwdIfc;
        if (srcIfc != null) {
            fwdIfc = srcIfc.getFwdIfc(trg);
        } else {
            fwdIfc = ipFwdTab.findSendingIface(vrf.getFwd(trg), trg);
        }
        if (fwdIfc == null) {
            return true;
        }
        prtUdp prt = vrf.getUdp(trg);
        servSipModem srv = new servSipModem();
        pipeSip = prt.streamConnect(new pipeLine(65536, false), fwdIfc, 0, trg, srv.srvPort(), srv.srvName(), null, -1);
        if (pipeSip == null) {
            return true;
        }
        cid = bits.randomD();
        int locP = bits.randomW();
        pipeSip.timeout = 180000;
        pipeSip.lineRx = pipeSide.modTyp.modeCRtryLF;
        pipeSip.lineTx = pipeSide.modTyp.modeCRLF;
        packSip sip = new packSip(pipeSip);
        pipeRtp = new packRtp();
        sip.makeReq("INVITE", calling, called, null, getCID(), cid + 0);
        sip.makeSdp(fwdIfc.addr, locP, aLaw);
        sip.writeDown();
        if (debugger.clntSipModemTraf) {
            sip.dump("tx");
        }
        for (;;) {
            if (sip.readUp()) {
                return true;
            }
            if (debugger.clntSipModemTraf) {
                sip.dump("rx");
            }
            cmds cmd = new cmds("sip", sip.command);
            cmd.word();
            int i = bits.str2num(cmd.word());
            if (i == 200) {
                break;
            }
        }
        int remP = sip.sdpGetMediaEP(trg);
        if (remP < 0) {
            return true;
        }
        if (pipeRtp.startConnect(prt, new pipeLine(65536, true), fwdIfc, locP, trg, remP)) {
            return true;
        }
        sip.makeReq("ACK", calling, called, null, getCID(), cid + 1);
        sip.writeDown();
        if (debugger.clntSipModemTraf) {
            sip.dump("tx");
        }
        pipeModem.originate(pipeMdm, codec, pipeRtp);
        new Thread(this).start();
        return false;
    }

    public void run() {
        try {
            for (;;) {
                bits.sleep(30000);
                if (pipeSip.isClosed() != 0) {
                    break;
                }
                packSip sip = new packSip(pipeSip);
                sip.writeDown();
                if (debugger.clntSipModemTraf) {
                    sip.dump("tx");
                }
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

}
