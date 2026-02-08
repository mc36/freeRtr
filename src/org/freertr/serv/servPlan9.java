package org.freertr.serv;

import java.io.File;
import java.io.RandomAccessFile;
import java.util.ArrayList;
import java.util.List;
import org.freertr.cry.cryHashCrc32;
import org.freertr.enc.encUrl;
import org.freertr.pack.packHolder;
import org.freertr.pack.packPlan9;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGenConn;
import org.freertr.prt.prtServS;
import org.freertr.tab.tabGen;
import org.freertr.user.userFilter;
import org.freertr.user.userFlash;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.debugger;
import org.freertr.util.logger;

/**
 * plan9 file protocol
 *
 * @author matecsaba
 */
public class servPlan9 extends servGeneric implements prtServS {

    /**
     * create instance
     */
    public servPlan9() {
    }

    /**
     * defaults text
     */
    public final static userFilter[] defaultF = {
        new userFilter("server plan9 .*", cmds.tabulator + "port " + packPlan9.port, null),
        new userFilter("server plan9 .*", cmds.tabulator + "protocol " + proto2string(protoAllStrm), null),
        new userFilter("server plan9 .*", cmds.tabulator + "user nobody", null),
        new userFilter("server plan9 .*", cmds.tabulator + "readonly", null)
    };

    public userFilter[] srvDefFlt() {
        return defaultF;
    }

    /**
     * root folder
     */
    public String rootFolder = "/data/";

    /**
     * user name
     */
    public String userName = "nobody";

    /**
     * read only server
     */
    public boolean readOnly = true;

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.setTime(120000);
        new servPlan9conn(this, pipe);
        return false;
    }

    public void srvShRun(String beg, List<String> l, int filter) {
        cmds.cfgLine(l, !readOnly, beg, "readonly", "");
        l.add(beg + "path " + rootFolder);
        l.add(beg + "user " + userName);
    }

    public boolean srvCfgStr(cmds cmd) {
        String s = cmd.word();
        if (s.equals("user")) {
            userName = cmd.word();
            return false;
        }
        if (s.equals("path")) {
            rootFolder = "/" + encUrl.normalizePath(cmd.word() + "/");
            return false;
        }
        if (s.equals("readonly")) {
            readOnly = true;
            return false;
        }
        if (!s.equals(cmds.negated)) {
            return true;
        }
        s = cmd.word();
        if (s.equals("user")) {
            userName = "nobody";
            return false;
        }
        if (s.equals("path")) {
            rootFolder = "/data/";
            return false;
        }
        if (s.equals("readonly")) {
            readOnly = false;
            return false;
        }
        return true;
    }

    public void srvHelp(userHelp l) {
        l.add(null, false, 1, new int[]{2}, "user", "set user name");
        l.add(null, false, 2, new int[]{-1}, "<str>", "name of user");
        l.add(null, false, 1, new int[]{2}, "path", "set root folder");
        l.add(null, false, 2, new int[]{-1}, "<path>", "name of root folder");
        l.add(null, false, 1, new int[]{-1}, "readonly", "set write protection");
    }

    public String srvName() {
        return "plan9";
    }

    public int srvPort() {
        return packPlan9.port;
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

class servPlan9file implements Comparable<servPlan9file> {

    public final int fid;

    public String name;

    public File file;

    public RandomAccessFile facc;

    public boolean dir;

    public File[] ctxd;

    public int ctxp;

    public servPlan9file(int i) {
        fid = i;
    }

    public int compareTo(servPlan9file o) {
        if (fid < o.fid) {
            return -1;
        }
        if (fid > o.fid) {
            return +1;
        }
        return 0;
    }

    public void getQid(packHolder pck) {
        if (dir) {
            pck.putByte(0, 0x80);
        } else {
            pck.putByte(0, 0);
        }
        pck.lsbPutD(1, 1); // version
        cryHashCrc32 hsh = new cryHashCrc32(cryHashCrc32.polyCrc32i);
        hsh.init();
        hsh.update(name.getBytes());
        pck.lsbPutQ(5, hsh.getCrc()); // qid
        pck.putSkip(13);
    }

    public void getStat(packHolder pck) {
        int i = userFlash.getFilePerm(file);
        i = ((i & 1) << 2) | (i & 2) | ((i & 4) >>> 2);
        i = i | (i << 3) | (i << 6);
        if (dir) {
            i |= 0x80000000;
        }
        pck.lsbPutD(0, i); // mode
        i = (int) (file.lastModified() / 1000);
        pck.lsbPutD(4, i); // atime
        pck.lsbPutD(8, i); // mtime
        pck.lsbPutQ(12, file.length()); // length
        pck.putSkip(20);
    }

    public void close() {
        if (facc == null) {
            return;
        }
        try {
            facc.close();
        } catch (Exception e) {
        }
        facc = null;
    }

}

class servPlan9conn implements Runnable {

    private final servPlan9 lower;

    private final pipeSide conn;

    private final tabGen<servPlan9file> fids = new tabGen<servPlan9file>();

    public servPlan9conn(servPlan9 parent, pipeSide pipe) {
        lower = parent;
        conn = pipe;
        logger.startThread(this);
    }

    private boolean doWork() {
        packPlan9 pckP = new packPlan9();
        packHolder pckB = new packHolder(true, true);
        pckP.pipe = conn;
        pckP.pack = pckB;
        if (pckP.packRecv()) {
            return true;
        }
        if (debugger.servPlan9traf) {
            logger.debug("rx " + pckP.dumpHdr());
        }
        servPlan9file fil;
        switch (pckP.opcode) {
            case packPlan9.opcTversion:
                pckB.clear();
                pckB.lsbPutD(0, packHolder.maxHead); // max message
                pckB.putSkip(4);
                pckP.putStr("9P2000");
                pckP.opcode = packPlan9.opcRversion;
                break;
            case packPlan9.opcTattach:
                fil = new servPlan9file(pckB.lsbGetD(0));
                fil.name = "/";
                fil.file = new File(lower.rootFolder);
                fil.dir = true;
                if (fids.add(fil) != null) {
                    pckB.clear();
                    pckP.putStr("already exists");
                    pckP.opcode = packPlan9.opcRerror;
                    break;
                }
                pckB.clear();
                fil.getQid(pckB);
                pckP.opcode = packPlan9.opcRattach;
                break;
            case packPlan9.opcTstat:
                fil = new servPlan9file(pckB.lsbGetD(0));
                fil = fids.find(fil);
                if (fil == null) {
                    pckB.clear();
                    pckP.putStr("not exists");
                    pckP.opcode = packPlan9.opcRerror;
                    break;
                }
                pckB.clear();
                pckB.lsbPutW(0, 0); // type
                pckB.lsbPutD(2, 0); // device
                pckB.putSkip(6);
                fil.getQid(pckB);
                fil.getStat(pckB);
                pckP.putStr(fil.name); // name
                pckP.putStr(lower.userName); // user
                pckP.putStr(lower.userName); // group
                pckP.putStr(lower.userName); // muid
                pckB.merge2end();
                pckB.lsbPutW(0, pckB.dataSize() + 2);
                pckB.lsbPutW(2, pckB.dataSize());
                pckB.putSkip(4);
                pckB.merge2beg();
                pckP.opcode = packPlan9.opcRstat;
                break;
            case packPlan9.opcTwalk:
                fil = new servPlan9file(pckB.lsbGetD(0));
                fil = fids.find(fil);
                if (fil == null) {
                    pckB.clear();
                    pckP.putStr("not exists");
                    pckP.opcode = packPlan9.opcRerror;
                    break;
                }
                int o = pckB.lsbGetD(4); // newfid
                if (o == fil.fid) {
                    fil.close();
                    fids.del(fil);
                }
                String a = fil.name;
                fil = new servPlan9file(o);
                o = pckB.lsbGetW(8); // number walks
                pckB.getSkip(10);
                for (int i = 0; i < o; i++) {
                    String s = pckP.getStr();
                    if (s == null) {
                        return true;
                    }
                    a += "/" + s;
                }
                a = "/" + encUrl.normalizePath(a);
                fil.name = a;
                fil.file = new File(lower.rootFolder + a);
                pckB.clear();
                fil.dir = fil.file.isDirectory();
                if (fids.add(fil) != null) {
                    pckB.clear();
                    pckP.putStr("already exists");
                    pckP.opcode = packPlan9.opcRerror;
                    break;
                }
                pckP.opcode = packPlan9.opcRwalk;
                if (o < 1) {
                    pckB.lsbPutW(0, 0); // nr quids
                    pckB.putSkip(2);
                    break;
                }
                pckB.lsbPutW(0, 1); // nr quids
                pckB.putSkip(2);
                fil.getQid(pckB);
                break;
            case packPlan9.opcTclunk:
                fil = new servPlan9file(pckB.lsbGetD(0));
                fil = fids.del(fil);
                if (fil == null) {
                    pckB.clear();
                    pckP.putStr("not exists");
                    pckP.opcode = packPlan9.opcRerror;
                    break;
                }
                fil.close();
                pckB.clear();
                pckP.opcode = packPlan9.opcRclunk;
                break;
            case packPlan9.opcTopen:
                fil = new servPlan9file(pckB.lsbGetD(0));
                fil = fids.find(fil);
                if (fil == null) {
                    pckB.clear();
                    pckP.putStr("not exists");
                    pckP.opcode = packPlan9.opcRerror;
                    break;
                }
                o = pckB.getByte(4); // mode
                if (lower.readOnly && (o != 0)) {
                    pckB.clear();
                    pckP.putStr("not allowed");
                    pckP.opcode = packPlan9.opcRerror;
                    break;
                }
                pckB.clear();
                fil.getQid(pckB);
                pckB.lsbPutD(0, packHolder.maxHead >>> 1); // max io size
                pckB.putSkip(4);
                pckP.opcode = packPlan9.opcRopen;
                if (fil.dir) {
                    fil.ctxd = userFlash.dirList(lower.rootFolder + fil.name);
                    if (fil.ctxd == null) {
                        fil.ctxd = new File[0];
                    }
                    fil.ctxp = 0;
                    break;
                }
                if (fil.facc != null) {
                    break;
                }
                if (o == 0) {
                    try {
                        fil.facc = new RandomAccessFile(fil.file, "r");
                    } catch (Exception e) {
                    }
                } else {
                    try {
                        fil.facc = new RandomAccessFile(fil.file, "rw");
                    } catch (Exception e) {
                    }
                }
                if (fil.facc == null) {
                    pckB.clear();
                    pckP.putStr("error opening");
                    pckP.opcode = packPlan9.opcRerror;
                    break;
                }
                break;
            case packPlan9.opcTread:
                fil = new servPlan9file(pckB.lsbGetD(0));
                fil = fids.find(fil);
                if (fil == null) {
                    pckB.clear();
                    pckP.putStr("not exists");
                    pckP.opcode = packPlan9.opcRerror;
                    break;
                }
                long p = pckB.lsbGetQ(4);
                o = pckB.lsbGetD(12);
                if (p < 0) {
                    p = 0;
                }
                if (o > packHolder.maxHead) {
                    o = packHolder.maxHead;
                }
                if (o < 0) {
                    o = 0;
                }
                if (fil.ctxd != null) {
                    o = fil.ctxd.length - fil.ctxp;
                    if (o > 8) {
                        o = 8;
                    }
                    byte[] buf = new byte[0];
                    for (int i = 0; i < o; i++) {
                        pckB.clear();
                        servPlan9file cf = new servPlan9file(0);
                        cf.file = fil.ctxd[fil.ctxp];
                        cf.name = cf.file.getName();
                        cf.dir = cf.file.isDirectory();
                        pckB.lsbPutW(0, 0); // type
                        pckB.lsbPutD(2, 0); // device
                        pckB.putSkip(6);
                        cf.getQid(pckB);
                        cf.getStat(pckB);
                        pckP.putStr(cf.name); // name
                        pckP.putStr(lower.userName); // user
                        pckP.putStr(lower.userName); // group
                        pckP.putStr(lower.userName); // muid
                        pckB.merge2end();
                        pckB.lsbPutW(0, pckB.dataSize());
                        pckB.putSkip(2);
                        pckB.merge2beg();
                        fil.ctxp++;
                        buf = bits.byteConcat(buf, pckB.getCopy());
                    }
                    pckB.clear();
                    pckP.opcode = packPlan9.opcRread;
                    pckB.lsbPutD(0, buf.length);
                    pckB.putSkip(4);
                    pckB.putCopy(buf, 0, 0, buf.length);
                    pckB.putSkip(buf.length);
                    break;
                }
                if (fil.facc == null) {
                    pckB.clear();
                    pckP.putStr("not open");
                    pckP.opcode = packPlan9.opcRerror;
                    break;
                }
                try {
                    fil.facc.seek(p);
                    p = fil.facc.length() - p;
                } catch (Exception e) {
                    pckB.clear();
                    pckP.putStr("error seeking");
                    pckP.opcode = packPlan9.opcRerror;
                    break;
                }
                if (p < 0) {
                    p = 0;
                }
                if (o > p) {
                    o = (int) p;
                }
                byte[] buf = new byte[o];
                try {
                    o = fil.facc.read(buf);
                } catch (Exception e) {
                    o = 0;
                }
                pckB.clear();
                pckP.opcode = packPlan9.opcRread;
                pckB.lsbPutD(0, o);
                pckB.putSkip(4);
                pckB.putCopy(buf, 0, 0, o);
                pckB.putSkip(o);
                break;
            case packPlan9.opcTwrite:
                fil = new servPlan9file(pckB.lsbGetD(0));
                fil = fids.find(fil);
                if (fil == null) {
                    pckB.clear();
                    pckP.putStr("not exists");
                    pckP.opcode = packPlan9.opcRerror;
                    break;
                }
                if (fil.facc == null) {
                    pckB.clear();
                    pckP.putStr("not open");
                    pckP.opcode = packPlan9.opcRerror;
                    break;
                }
                if (lower.readOnly) {
                    pckB.clear();
                    pckP.putStr("not allowed");
                    pckP.opcode = packPlan9.opcRerror;
                    break;
                }
                p = pckB.lsbGetQ(4);
                o = pckB.lsbGetD(12);
                pckB.getSkip(16);
                if (p < 0) {
                    p = 0;
                }
                if (o > pckB.dataSize()) {
                    o = pckB.dataSize();
                }
                if (o < 0) {
                    o = 0;
                }
                try {
                    fil.facc.seek(p);
                } catch (Exception e) {
                    pckB.clear();
                    pckP.putStr("error seeking");
                    pckP.opcode = packPlan9.opcRerror;
                    break;
                }
                buf = pckB.getCopy();
                try {
                    fil.facc.write(buf, 0, o);
                } catch (Exception e) {
                    o = 0;
                }
                pckP.opcode = packPlan9.opcRwrite;
                pckB.lsbPutD(0, o);
                pckB.putSkip(4);
                break;
            case packPlan9.opcTcreate:
                fil = new servPlan9file(pckB.lsbGetD(0));
                fil = fids.find(fil);
                if (fil == null) {
                    pckB.clear();
                    pckP.putStr("not exists");
                    pckP.opcode = packPlan9.opcRerror;
                    break;
                }
                if (lower.readOnly) {
                    pckB.clear();
                    pckP.putStr("not allowed");
                    pckP.opcode = packPlan9.opcRerror;
                    break;
                }
                pckB.getSkip(4);
                a = pckP.getStr();
                if (a == null) {
                    return true;
                }
                a = fil.name + "/" + a;
                o = pckB.lsbGetD(0); // perm
                if ((o & 0x80000000) != 0) {
                    userFlash.mkdir(lower.rootFolder + a);
                } else {
                    bits.buf2txt(false, new ArrayList<String>(), lower.rootFolder + a);
                }
                userFlash.setFilePerm(lower.rootFolder + a, (o & 0x100) != 0, (o & 0x80) != 0, (o & 0x40) != 0, (o & 0x4) != 0, (o & 0x2) != 0, (o & 0x1) != 0);
                pckB.clear();
                pckP.opcode = packPlan9.opcRcreate;
                break;
            case packPlan9.opcTremove:
                fil = new servPlan9file(pckB.lsbGetD(0));
                fil = fids.del(fil);
                if (fil == null) {
                    pckB.clear();
                    pckP.putStr("not exists");
                    pckP.opcode = packPlan9.opcRerror;
                    break;
                }
                fil.close();
                if (lower.readOnly) {
                    pckB.clear();
                    pckP.putStr("not allowed");
                    pckP.opcode = packPlan9.opcRerror;
                    break;
                }
                userFlash.delete(lower.rootFolder + fil.name);
                pckB.clear();
                pckP.opcode = packPlan9.opcRremove;
                break;
            default:
                return true;
        }
        if (debugger.servPlan9traf) {
            logger.debug("tx " + pckP.dumpHdr());
        }
        pckP.packSend();
        return false;
    }

    public void run() {
        if (debugger.servPlan9traf) {
            logger.debug("accept connection");
        }
        try {
            for (;;) {
                if (doWork()) {
                    break;
                }
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
        if (debugger.servPlan9traf) {
            logger.debug("close connection");
        }
        conn.setClose();
        for (int i = 0; i < fids.size(); i++) {
            fids.get(i).close();
        }
    }

}
