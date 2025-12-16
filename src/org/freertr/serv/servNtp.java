package org.freertr.serv;

import java.util.List;
import org.freertr.addr.addrIPv4;
import org.freertr.cfg.cfgAll;
import org.freertr.pack.packHolder;
import org.freertr.pack.packNtp;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGenConn;
import org.freertr.prt.prtServS;
import org.freertr.tab.tabGen;
import org.freertr.user.userFilter;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.debugger;
import org.freertr.util.logger;

/**
 * network time protocol (rfc5905) server
 *
 * @author matecsaba
 */
public class servNtp extends servGeneric implements prtServS {

    /**
     * create instance
     */
    public servNtp() {
    }

    /**
     * stratum
     */
    public int stratum = 2;

    /**
     * reference id
     */
    public addrIPv4 refId = new addrIPv4();

    /**
     * adjustment
     */
    public long adjust = 0;

    /**
     * defaults text
     */
    public final static userFilter[] defaultF = {
        new userFilter("server ntp .*", cmds.tabulator + "port " + packNtp.port, null),
        new userFilter("server ntp .*", cmds.tabulator + "protocol " + proto2string(protoAll), null),
        new userFilter("server ntp .*", cmds.tabulator + "stratum 2", null),
        new userFilter("server ntp .*", cmds.tabulator + "adjust 0", null)
    };

    public userFilter[] srvDefFlt() {
        return defaultF;
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.setTime(5000);
        new servNtpConn(pipe, this);
        return false;
    }

    public void srvShRun(String beg, List<String> lst, int filter) {
        lst.add(beg + "adjust " + adjust);
        lst.add(beg + "stratum " + stratum);
        lst.add(beg + "reference " + refId);
    }

    public boolean srvCfgStr(cmds cmd) {
        String s = cmd.word();
        if (s.equals("adjust")) {
            adjust = bits.str2long(cmd.word());
            return false;
        }
        if (s.equals("stratum")) {
            stratum = bits.str2num(cmd.word());
            return false;
        }
        if (s.equals("reference")) {
            refId = new addrIPv4();
            refId.fromString(cmd.word());
            return false;
        }
        return true;
    }

    public void srvHelp(userHelp l) {
        l.add(null, false, 1, new int[]{2}, "adjust", "set adjustment time");
        l.add(null, false, 2, new int[]{-1}, "<num>", "adjustment in millis");
        l.add(null, false, 1, new int[]{2}, "stratum", "set stratum number");
        l.add(null, false, 2, new int[]{-1}, "<num>", "stratum value");
        l.add(null, false, 1, new int[]{2}, "reference", "set reference value");
        l.add(null, false, 2, new int[]{-1}, "<addr>", "reference address");
    }

    public String srvName() {
        return "ntp";
    }

    public int srvPort() {
        return packNtp.port;
    }

    public int srvProto() {
        return protoAll;
    }

    public boolean srvInit() {
        return genStrmStart(this, new pipeLine(32768, true), 0);
    }

    public boolean srvDeinit() {
        return genericStop(0);
    }

}

class servNtpConn implements Runnable {

    private pipeSide pipe;

    private servNtp lower;

    public servNtpConn(pipeSide conn, servNtp parent) {
        pipe = conn;
        lower = parent;
        new Thread(this).start();
    }

    public void run() {
        try {
            doWork();
        } catch (Exception e) {
            logger.traceback(e);
        }
        pipe.setClose();
    }

    private void doWork() {
        packHolder pckBin = pipe.readPacket(true);
        if (pckBin == null) {
            return;
        }
        packNtp pckNtp = new packNtp();
        if (pckNtp.parsePacket(pckBin)) {
            return;
        }
        if (debugger.servNtpTraf) {
            logger.debug("rx " + pckNtp);
        }
        pckNtp.leap = 0;
        pckNtp.mode = packNtp.modServ;
        pckNtp.stratum = lower.stratum;
        pckNtp.poll = 8;
        pckNtp.precise = 192;
        pckNtp.rotDel = 64;
        pckNtp.rotDis = 128;
        pckNtp.refId = lower.refId.copyBytes();
        long tim = bits.getTime() + cfgAll.timeServerOffset + lower.adjust;
        pckNtp.refTime = packNtp.encode(tim);
        pckNtp.origTime = pckNtp.sendTime;
        pckNtp.recvTime = packNtp.encode(tim);
        pckNtp.sendTime = packNtp.encode(tim);
        pckBin = new packHolder(true, true);
        pckNtp.createPacket(pckBin);
        pckBin.pipeSend(pipe, 0, pckBin.dataSize(), 2);
        if (debugger.servNtpTraf) {
            logger.debug("tx " + pckNtp);
        }
        pipe.setClose();
    }

}
