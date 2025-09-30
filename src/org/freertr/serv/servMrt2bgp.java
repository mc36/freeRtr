package org.freertr.serv;

import java.io.File;
import java.io.RandomAccessFile;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv4;
import org.freertr.pack.packHolder;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGenConn;
import org.freertr.prt.prtServS;
import org.freertr.rtr.rtrBgp;
import org.freertr.rtr.rtrBgpMrt;
import org.freertr.rtr.rtrBgpNeigh;
import org.freertr.rtr.rtrBgpSpeak;
import org.freertr.rtr.rtrBgpUtil;
import org.freertr.user.userFilter;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.logger;

/**
 * mrt to bgp
 *
 * @author matecsaba
 */
public class servMrt2bgp extends servGeneric implements prtServS {

    /**
     * create instance
     */
    public servMrt2bgp() {
    }

    /**
     * defaults text
     */
    public final static userFilter[] defaultF = {
        new userFilter("server mrt2bgp .*", cmds.tabulator + "port " + rtrBgp.port, null),
        new userFilter("server mrt2bgp .*", cmds.tabulator + "protocol " + proto2string(protoAllStrm), null)
    };

    /**
     * local as
     */
    public int localAs;

    /**
     * router id
     */
    public addrIPv4 routerID = new addrIPv4();

    /**
     * mrt file
     */
    public String mrtFile;

    public userFilter[] srvDefFlt() {
        return defaultF;
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.setTime(120000);
        pipe.lineRx = pipeSide.modTyp.modeCRtryLF;
        pipe.lineTx = pipeSide.modTyp.modeCRLF;
        new servMrt2bgpConn(this, pipe, id.peerAddr);
        return false;
    }

    public void srvShRun(String beg, List<String> l, int filter) {
        l.add(beg + "router-id " + routerID);
        l.add(beg + "local-as " + bits.num2str(localAs));
        cmds.cfgLine(l, mrtFile == null, beg, "mrt-file", mrtFile);
    }

    public boolean srvCfgStr(cmds cmd) {
        String s = cmd.word();
        if (s.equals("mrt-file")) {
            mrtFile = cmd.getRemaining();
            return false;
        }
        if (s.equals("router-id")) {
            routerID.fromString(cmd.word());
            return false;
        }
        if (s.equals("local-as")) {
            localAs = bits.str2num(cmd.word());
            return false;
        }
        return true;
    }

    public void srvHelp(userHelp l) {
        l.add(null, false, 1, new int[]{2}, "local-as", "set local asn");
        l.add(null, false, 2, new int[]{-1}, "<num>", "as number");
        l.add(null, false, 1, new int[]{2}, "router-id", "set router id");
        l.add(null, false, 2, new int[]{-1}, "<addr>", "router id");
        l.add(null, false, 1, new int[]{2}, "mrt-file", "set data to serve");
        l.add(null, false, 2, new int[]{-1}, "<nam>", "file name");
    }

    public String srvName() {
        return "mrt2bgp";
    }

    public int srvPort() {
        return rtrBgp.port;
    }

    public int srvProto() {
        return protoAllStrm;
    }

    public boolean srvInit() {
        return genStrmStart(this, new pipeLine(32768, false), 0);
    }

    public boolean srvDeinit() {
        return genericStop(0);
    }

}

class servMrt2bgpConn implements Runnable {

    private servMrt2bgp lower;

    private pipeSide pipe;

    private addrIP peer;

    private int safi;

    public servMrt2bgpConn(servMrt2bgp parent, pipeSide stream, addrIP remote) {
        lower = parent;
        pipe = stream;
        peer = remote.copyBytes();
        new Thread(this).start();
    }

    private void sendTable(rtrBgpSpeak spk, packHolder pck) {
        RandomAccessFile fs = null;
        try {
            fs = new RandomAccessFile(new File(lower.mrtFile), "r");
        } catch (Exception e) {
            spk.sendNotify(1, 3);
            pipe.setClose();
            return;
        }
        packHolder tmp = new packHolder(true, true);
        packHolder hlp = new packHolder(true, true);
        for (;;) {
            int i = rtrBgpMrt.readNextMrt(spk, hlp, tmp, pck, fs);
            if (i == 1) {
                break;
            }
            if (i == 2) {
                continue;
            }
            if (pck.getByte(rtrBgpUtil.sizeU - 1) != rtrBgpUtil.msgUpdate) {
                continue;
            }
            pck.getSkip(rtrBgpUtil.sizeU);
            if (pck.ETHtype != safi) {
                continue;
            }
            spk.packSend(pck, rtrBgpUtil.msgUpdate);
        }
        try {
            fs.close();
        } catch (Exception e) {
        }
        spk.sendEndOfRib(safi);
    }

    public void run() {
        logger.warn("neighbor " + peer + " up");
        try {
            if (peer.isIPv4()) {
                safi = rtrBgpUtil.safiIp4uni;
            } else {
                safi = rtrBgpUtil.safiIp6uni;
            }
            rtrBgp bgp = new rtrBgp(lower.srvVrf.getFwd(peer), lower.srvVrf, null, 0);
            rtrBgpNeigh nei = new rtrBgpNeigh(bgp, peer);
            nei.localAs = lower.localAs;
            nei.addrFams = safi;
            rtrBgpSpeak spk = new rtrBgpSpeak(bgp, nei, pipe, 0);
            packHolder pck = new packHolder(true, true);
            spk.sendOpen();
            if (spk.packRecv(pck) != rtrBgpUtil.msgOpen) {
                spk.sendNotify(1, 3);
                pipe.setClose();
                return;
            }
            spk.sendKeepAlive();
            sendTable(spk, pck);
            for (int o = 1000;; o++) {
                int i = pipe.ready2tx();
                if (i < 0) {
                    break;
                }
                i = pipe.ready2rx();
                if (i > 0) {
                    if (spk.packRecv(pck) != rtrBgpUtil.msgRefrsh) {
                        continue;
                    }
                    sendTable(spk, pck);
                }
                if (o < 30) {
                    bits.sleep(1000);
                    continue;
                }
                spk.sendKeepAlive();
                o = 0;
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
        pipe.setClose();
        logger.error("neighbor " + peer + " down");
    }

}
