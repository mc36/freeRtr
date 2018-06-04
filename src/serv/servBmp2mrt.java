package serv;

import addr.addrIP;
import java.io.File;
import java.io.RandomAccessFile;
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

    /**
     * got update
     *
     * @param dir direction: false=rx, true=tx
     * @param as as number
     * @param src source of packet
     * @param spk got from speaker
     * @param dat bgp message
     */
    public synchronized void gotMessage(boolean dir, int as, addrIP src, addrIP spk, byte[] dat) {
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
            if (typ != rtrBgpMon.typMon) {
                continue;
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
            lower.gotMessage((flg & 0x40) != 0, as, adr, peer, pck.getCopy());
        }
        logger.error("neighbor " + peer + " down");
    }

}
