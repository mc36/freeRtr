package net.freertr.serv;

import java.io.File;
import java.io.RandomAccessFile;
import java.util.List;
import net.freertr.pack.packHolder;
import net.freertr.pack.packTftp;
import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeSide;
import net.freertr.prt.prtGenConn;
import net.freertr.prt.prtServS;
import net.freertr.tab.tabGen;
import net.freertr.user.userFilter;
import net.freertr.user.userFlash;
import net.freertr.user.userHelping;
import net.freertr.util.cmds;
import net.freertr.util.debugger;
import net.freertr.util.logger;
import net.freertr.util.uniResLoc;

/**
 * trivial file transfer protocol (rfc1350) server
 *
 * @author matecsaba
 */
public class servTftp extends servGeneric implements prtServS {

    /**
     * create instance
     */
    public servTftp() {
    }

    /**
     * root folder
     */
    public String rootFolder = "/";

    /**
     * read only server
     */
    public boolean readOnly = true;

    /**
     * defaults text
     */
    public final static String[] defaultL = {
        "server tftp .*! port " + packTftp.port,
        "server tftp .*! protocol " + proto2string(protoAllDgrm),
        "server tftp .*! readonly"
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public tabGen<userFilter> srvDefFlt() {
        return defaultF;
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.setTime(120000);
        new servTftpConn(pipe, this);
        return false;
    }

    public void srvShRun(String beg, List<String> l, int filter) {
        cmds.cfgLine(l, !readOnly, beg, "readonly", "");
        l.add(beg + "path " + rootFolder);
    }

    public boolean srvCfgStr(cmds cmd) {
        String s = cmd.word();
        if (s.equals("path")) {
            rootFolder = "/" + uniResLoc.normalizePath(cmd.word() + "/");
            return false;
        }
        if (s.equals("readonly")) {
            readOnly = true;
            return false;
        }
        if (!s.equals("no")) {
            return true;
        }
        s = cmd.word();
        if (s.equals("path")) {
            rootFolder = "/";
            return false;
        }
        if (s.equals("readonly")) {
            readOnly = false;
            return false;
        }
        return true;
    }

    public void srvHelp(userHelping l) {
        l.add(null, "1 2  path                         set root folder");
        l.add(null, "2 .    <path>                     name of root folder");
        l.add(null, "1 .  readonly                     set write protection");
    }

    public String srvName() {
        return "tftp";
    }

    public int srvPort() {
        return packTftp.port;
    }

    public int srvProto() {
        return protoAllDgrm;
    }

    public boolean srvInit() {
        return genStrmStart(this, new pipeLine(32768, true), 0);
    }

    public boolean srvDeinit() {
        return genericStop(0);

    }

}

class servTftpConn implements Runnable {

    public pipeSide pipe;

    public servTftp lower;

    private RandomAccessFile fil;

    private long siz;

    private long blk;

    public servTftpConn(pipeSide conn, servTftp parent) {
        pipe = conn;
        lower = parent;
        new Thread(this).start();
    }

    public void run() {
        try {
            doer();
        } catch (Exception e) {
            logger.traceback(e);
        }
        try {
            fil.close();
        } catch (Exception e) {
        }
        pipe.setClose();
    }

    private void doer() {
        packHolder pckBin = pipe.readPacket(true);
        if (pckBin == null) {
            logger.info("got no packet");
            return;
        }
        packTftp pckTft = new packTftp();
        if (pckTft.parsePacket(pckBin)) {
            return;
        }
        if (debugger.servTftpTraf) {
            logger.debug("rx " + pckTft.dump());
        }
        String a = lower.rootFolder + uniResLoc.normalizePath("" + pckTft.nam);
        File fh = new File(a);
        boolean red;
        switch (pckTft.typ) {
            case packTftp.msgRead:
                red = true;
                if (!fh.exists()) {
                    sendError(1, "file not exists");
                    return;
                }
                if (!fh.isFile()) {
                    sendError(2, "not a file");
                    return;
                }
                siz = fh.length();
                try {
                    fil = new RandomAccessFile(fh, "r");
                } catch (Exception e) {
                    sendError(2, "error opening file");
                    return;
                }
                break;
            case packTftp.msgWrite:
                red = false;
                userFlash.mkfile(a);
                if (!fh.exists()) {
                    sendError(1, "file not exists");
                    return;
                }
                if (!fh.isFile()) {
                    sendError(2, "not a file");
                    return;
                }
                siz = 0;
                try {
                    fil = new RandomAccessFile(fh, "rw");
                    fil.setLength(0);
                } catch (Exception e) {
                    sendError(2, "error opening file");
                    return;
                }
                break;
            default:
                return;
        }
        blk = 0;
        for (;;) {
            if (red) {
                replyRead(pckTft);
            } else {
                replyWrite(pckTft);
            }
            pckBin = pipe.readPacket(true);
            if (pckBin == null) {
                return;
            }
            pckTft = new packTftp();
            if (pckTft.parsePacket(pckBin)) {
                return;
            }
            if (debugger.servTftpTraf) {
                logger.debug("rx " + pckTft.dump());
            }
        }
    }

    private void sendError(int cod, String str) {
        packTftp pckTft = new packTftp();
        pckTft.blk = cod;
        pckTft.nam = str;
        pckTft.typ = packTftp.msgError;
        if (debugger.servTftpTraf) {
            logger.debug("tx " + pckTft.dump());
        }
        packHolder pckBin = pckTft.createPacket();
        pckBin.merge2beg();
        pckBin.pipeSend(pipe, 0, pckBin.dataSize(), 2);
    }

    private void replyRead(packTftp pckTft) {
        switch (pckTft.typ) {
            case packTftp.msgRead:
                blk = 0;
                break;
            case packTftp.msgAck:
                if (((pckTft.blk - 1) & 0xffff) == (blk & 0xffff)) {
                    blk++;
                    break;
                }
                if ((pckTft.blk & 0xffff) == (blk & 0xffff)) {
                    break;
                }
                return;
            default:
                return;
        }
        long pos = blk * packTftp.size;
        long len = siz - pos;
        if (len > packTftp.size) {
            len = packTftp.size;
        }
        if (len < 0) {
            len = 0;
        }
        pckTft = new packTftp();
        pckTft.dat = new byte[(int) len];
        try {
            fil.seek(pos);
            fil.read(pckTft.dat);
        } catch (Exception e) {
            return;
        }
        pckTft.blk = (int) (blk + 1);
        pckTft.typ = packTftp.msgData;
        if (debugger.servTftpTraf) {
            logger.debug("tx " + pckTft.dump());
        }
        packHolder pckBin = pckTft.createPacket();
        pckBin.merge2beg();
        pckBin.pipeSend(pipe, 0, pckBin.dataSize(), 2);
    }

    private void replyWrite(packTftp pckTft) {
        switch (pckTft.typ) {
            case packTftp.msgWrite:
                blk = 0;
                break;
            case packTftp.msgData:
                if (((pckTft.blk - 1) & 0xffff) == (blk & 0xffff)) {
                    try {
                        fil.write(pckTft.dat);
                    } catch (Exception e) {
                        return;
                    }
                    blk++;
                    break;
                }
                if ((pckTft.blk & 0xffff) == (blk & 0xffff)) {
                    break;
                }
                return;
            default:
                return;
        }
        pckTft = new packTftp();
        pckTft.blk = (int) blk;
        pckTft.typ = packTftp.msgAck;
        if (debugger.servTftpTraf) {
            logger.debug("tx " + pckTft.dump());
        }
        packHolder pckBin = pckTft.createPacket();
        pckBin.merge2beg();
        pckBin.pipeSend(pipe, 0, pckBin.dataSize(), 2);
    }

}
