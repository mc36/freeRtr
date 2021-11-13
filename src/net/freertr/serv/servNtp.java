package net.freertr.serv;

import java.util.List;
import net.freertr.addr.addrIPv4;
import net.freertr.cfg.cfgAll;
import net.freertr.pack.packHolder;
import net.freertr.pack.packNtp;
import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeSide;
import net.freertr.prt.prtGenConn;
import net.freertr.prt.prtServS;
import net.freertr.tab.tabGen;
import net.freertr.user.userFilter;
import net.freertr.user.userHelping;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.debugger;
import net.freertr.util.logger;

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
     * defaults text
     */
    public final static String[] defaultL = {
        "server ntp .*! port " + packNtp.port,
        "server ntp .*! protocol " + proto2string(protoAll),
        "server ntp .*! stratum 2"
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public tabGen<userFilter> srvDefFlt() {
        return defaultF;
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.setTime(5000);
        new servNtpConn(pipe, this);
        return false;
    }

    public void srvShRun(String beg, List<String> lst, int filter) {
        lst.add(beg + "stratum " + stratum);
        lst.add(beg + "reference " + refId);
    }

    public boolean srvCfgStr(cmds cmd) {
        String s = cmd.word();
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

    public void srvHelp(userHelping l) {
        l.add(null, "1 2  stratum                      set stratum number");
        l.add(null, "2 .    <num>                      stratum value");
        l.add(null, "1 2  reference                    set reference value");
        l.add(null, "2 .    <addr>                     reference address");
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
        long tim = bits.getTime() + cfgAll.timeServerOffset;
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
    }

}
