package serv;

import addr.addrIP;
import cfg.cfgAll;
import java.io.File;
import java.io.RandomAccessFile;
import java.util.Comparator;
import java.util.List;
import pack.packHolder;
import pipe.pipeLine;
import pipe.pipeSide;
import prt.prtGenConn;
import prt.prtServS;
import rtr.rtrBgpMon;
import rtr.rtrBgpMrt;
import tab.tabGen;
import user.userFilter;
import user.userFlash;
import user.userFormat;
import user.userHelping;
import util.bits;
import util.cmds;
import util.logger;

/**
 * bgp monitoring (rfc7854) protocol to multi-threaded routing (rfc6396) toolkit
 *
 * @author matecsaba
 */
public class servBmp2mrt extends servGeneric implements prtServS {

    /**
     * port number
     */
    public static final int port = 17971;

    /**
     * header size
     */
    public static final int size = 6;

    private final tabGen<servBmp2mrtStat> stats = new tabGen<servBmp2mrtStat>();

    private String fileName;

    private String backupName;

    private boolean local;

    private int maxTime;

    private int maxPack;

    private int maxByte;

    private RandomAccessFile fileHandle;

    private long started;

    private int bytes;

    private int packs;

    /**
     * defaults text
     */
    public final static String defaultL[] = {
        "server bmp2mrt .*! port " + port,
        "server bmp2mrt .*! protocol " + proto2string(protoAllStrm),
        "server bmp2mrt .*! no local",
        "server bmp2mrt .*! max-time 0",
        "server bmp2mrt .*! max-pack 0",
        "server bmp2mrt .*! max-byte 0",
        "server bmp2mrt .*! no file",
        "server bmp2mrt .*! no backup"
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public tabGen<userFilter> srvDefFlt() {
        return defaultF;
    }

    public void srvShRun(String beg, List<String> l) {
        l.add(beg + "max-time " + maxTime);
        l.add(beg + "max-pack " + maxPack);
        l.add(beg + "max-byte " + maxByte);
        cmds.cfgLine(l, !local, beg, "local", "");
        cmds.cfgLine(l, backupName == null, beg, "backup", backupName);
        cmds.cfgLine(l, fileName == null, beg, "file", fileName);
    }

    public boolean srvCfgStr(cmds cmd) {
        String s = cmd.word();
        if (s.equals("local")) {
            local = true;
            return false;
        }
        if (s.equals("max-time")) {
            maxTime = bits.str2num(cmd.word());
            return false;
        }
        if (s.equals("max-pack")) {
            maxPack = bits.str2num(cmd.word());
            return false;
        }
        if (s.equals("max-byte")) {
            maxByte = bits.str2num(cmd.word());
            return false;
        }
        if (s.equals("backup")) {
            backupName = cmd.getRemaining();
            return false;
        }
        if (s.equals("file")) {
            fileName = cmd.getRemaining();
            try {
                fileHandle.close();
            } catch (Exception e) {
            }
            fileHandle = null;
            if (backupName != null) {
                userFlash.rename(fileName, backupName, true, true);
            }
            started = bits.getTime();
            bytes = 0;
            packs = 0;
            try {
                fileHandle = new RandomAccessFile(new File(fileName), "rw");
                fileHandle.setLength(0);
            } catch (Exception e) {
                logger.traceback(e);
            }
            return false;
        }
        if (!s.equals("no")) {
            return true;
        }
        s = cmd.word();
        if (s.equals("local")) {
            local = false;
            return false;
        }
        if (s.equals("max-time")) {
            maxTime = 0;
            return false;
        }
        if (s.equals("max-pack")) {
            maxPack = 0;
            return false;
        }
        if (s.equals("max-byte")) {
            maxByte = 0;
            return false;
        }
        if (s.equals("backup")) {
            backupName = null;
            return false;
        }
        if (s.equals("file")) {
            try {
                fileHandle.close();
            } catch (Exception e) {
            }
            fileName = null;
            fileHandle = null;
            return false;
        }
        return true;
    }

    public void srvHelp(userHelping l) {
        l.add("1 2    file                      log to file");
        l.add("2 2,.    <file>                  name of file");
        l.add("1 .    local                     log to syslog");
        l.add("1 2    max-time                  maximum time");
        l.add("2 .      <num>                   ms between backups");
        l.add("1 2    max-byte                  maximum bytes");
        l.add("2 .      <num>                   bytes between backups");
        l.add("1 2    max-pack                  maximum packets");
        l.add("2 .      <num>                   packets between backups");
        l.add("1 2    backup                    backup to file");
        l.add("2 2,.    <file>                  name of file");
    }

    public String srvName() {
        return "bmp2mrt";
    }

    public int srvPort() {
        return port;
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

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.timeout = 120000;
        new servBmp2mrtConn(pipe, this, id);
        return false;
    }

    private servBmp2mrtStat getStat(addrIP from, addrIP peer) {
        servBmp2mrtStat res = new servBmp2mrtStat();
        res.from = from.copyBytes();
        res.peer = peer.copyBytes();
        servBmp2mrtStat old = stats.add(res);
        if (old != null) {
            return old;
        } else {
            return res;
        }
    }

    /**
     * got state
     *
     * @param as as number
     * @param src source of packet
     * @param spk got from speaker
     * @param st state
     */
    public void gotState(int as, addrIP src, addrIP spk, boolean st) {
        servBmp2mrtStat stat = getStat(spk, src);
        stat.as = as;
        stat.state = st;
        stat.since = bits.getTime();
        stat.change++;
    }

    /**
     * got update
     *
     * @param as as number
     * @param src source of packet
     * @param spk got from speaker
     * @param dir direction: false=rx, true=tx
     * @param dat bgp message
     */
    public synchronized void gotMessage(int as, addrIP src, addrIP spk, boolean dir, byte[] dat) {
        servBmp2mrtStat stat = getStat(spk, src);
        stat.as = as;
        if (dir) {
            stat.packOut++;
            stat.byteOut += dat.length;
        } else {
            stat.packIn++;
            stat.byteIn += dat.length;
        }
        stat.packLast = bits.getTime();
        if (local) {
            logger.info((dir ? "tx" : "rx") + " " + as + " " + src + " " + spk + " " + bits.byteDump(dat, 0, -1));
        }
        if (backupName != null) {
            boolean ned = false;
            if (maxTime > 0) {
                ned |= (bits.getTime() - started) > maxTime;
            }
            if (maxPack > 0) {
                ned |= packs > maxPack;
            }
            if (maxByte > 0) {
                ned |= bytes > maxByte;
            }
            if (ned) {
                try {
                    fileHandle.close();
                } catch (Exception e) {
                    logger.traceback(e);
                }
                fileHandle = null;
                packs = 0;
                bytes = 0;
                userFlash.rename(fileName, backupName, true, true);
                started = bits.getTime();
                try {
                    fileHandle = new RandomAccessFile(new File(fileName), "rw");
                    fileHandle.setLength(0);
                } catch (Exception e) {
                    logger.traceback(e);
                }
            }
        }
        byte[] hdr = new byte[128];
        int len = rtrBgpMrt.putMrtHeader(hdr, dir, as, 0, src, spk, dat.length);
        packs++;
        bytes += len + dat.length;
        if (fileHandle == null) {
            return;
        }
        try {
            fileHandle.write(hdr, 0, len);
            fileHandle.write(dat, 0, dat.length);
            return;
        } catch (Exception e) {
            logger.traceback(e);
        }
        try {
            fileHandle.close();
        } catch (Exception e) {
            logger.traceback(e);
        }
        fileHandle = null;
    }

    /**
     * get show
     *
     * @return result
     */
    public userFormat getShow() {
        userFormat res = new userFormat("|", "from|peer|as|state|change|last");
        for (int i = 0; i < stats.size(); i++) {
            res.add("" + stats.get(i));
        }
        return res;
    }

    /**
     * get show
     *
     * @param frm from
     * @param per peer
     * @return result
     */
    public userFormat getShow(addrIP frm, addrIP per) {
        userFormat res = new userFormat("|", "category|value");
        servBmp2mrtStat stat = new servBmp2mrtStat();
        stat.from = frm;
        stat.peer = per;
        stat = stats.find(stat);
        if (stat == null) {
            return null;
        }
        res.add("from|" + stat.from);
        res.add("peer|" + stat.peer);
        res.add("as|" + stat.as);
        res.add("state|" + stat.state);
        res.add("since|" + bits.time2str(cfgAll.timeZoneName, stat.since + cfgAll.timeServerOffset, 3) + " (" + bits.timePast(stat.since) + " ago)");
        res.add("change|" + stat.change);
        res.add("pack in|" + stat.packIn);
        res.add("pack out|" + stat.packOut);
        res.add("byte in|" + stat.byteIn);
        res.add("byte out|" + stat.byteOut);
        res.add("pack last|" + bits.time2str(cfgAll.timeZoneName, stat.packLast + cfgAll.timeServerOffset, 3) + " (" + bits.timePast(stat.packLast) + " ago)");
        return res;
    }

}

class servBmp2mrtStat implements Comparator<servBmp2mrtStat> {

    public addrIP from;

    public addrIP peer;

    public int as;

    public int packIn;

    public int packOut;

    public long packLast;

    public int byteIn;

    public int byteOut;

    public boolean state = true;

    public long since;

    public int change;

    public int compare(servBmp2mrtStat o1, servBmp2mrtStat o2) {
        int i = o1.from.compare(o1.from, o2.from);
        if (i != 0) {
            return i;
        }
        return o1.peer.compare(o1.peer, o2.peer);
    }

    public String toString() {
        return from + "|" + peer + "|" + as + "|" + state + "|" + change + "|" + bits.timePast(since);
    }

}

class servBmp2mrtConn implements Runnable {

    private pipeSide pipe;

    private servBmp2mrt lower;

    private addrIP peer;

    public servBmp2mrtConn(pipeSide pip, servBmp2mrt prnt, prtGenConn id) {
        pipe = pip;
        lower = prnt;
        peer = id.peerAddr.copyBytes();
        new Thread(this).start();
    }

    public void run() {
        try {
            doer();
        } catch (Exception e) {
            logger.traceback(e);
        }
        pipe.setClose();
    }

    private void doer() {
        logger.warn("neighbor " + peer + " up");
        packHolder pck = new packHolder(true, true);
        addrIP adr = new addrIP();
        for (;;) {
            pck.clear();
            if (pck.pipeRecv(pipe, 0, servBmp2mrt.size, 144) != servBmp2mrt.size) {
                break;
            }
            if (pck.getByte(0) != 3) { // version
                break;
            }
            int len = pck.msbGetD(1) - servBmp2mrt.size; // length
            int typ = pck.getByte(5); // type
            if (len < 1) {
                continue;
            }
            pck.clear();
            if (pck.pipeRecv(pipe, 0, len, 144) != len) {
                break;
            }
            // typ = pck.getByte(0); // type
            int flg = pck.getByte(1); // flags
            // int rd = pck.msbGetQ(2); // distinguisher
            pck.getAddr(adr, 10);
            int as = pck.msbGetD(26); // asnum
            // pck.getAddr(rtr, 30); // rtrid
            // int tim = pck.msbGetD(34); // time
            // int nano = pck.msbGetD(38); // time
            pck.getSkip(rtrBgpMon.size - servBmp2mrt.size);
            if ((flg & 0x80) == 0) {
                adr.fromIPv4addr(adr.toIPv4());
            }
            switch (typ) {
                case rtrBgpMon.typMon:
                    break;
                case rtrBgpMon.typPerUp:
                    pck.getSkip(20);
                    lower.gotState(as, adr, peer, true);
                    break;
                case rtrBgpMon.typPerDn:
                    pck.getSkip(1);
                    lower.gotState(as, adr, peer, false);
                    break;
                default:
                    continue;
            }
            lower.gotMessage(as, adr, peer, (flg & 0x40) != 0, pck.getCopy());
        }
        logger.error("neighbor " + peer + " down");
    }

}
