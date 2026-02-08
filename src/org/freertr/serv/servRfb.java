package org.freertr.serv;

import java.util.List;
import org.freertr.cfg.cfgAll;
import org.freertr.pipe.pipeImage;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGenConn;
import org.freertr.prt.prtServS;
import org.freertr.tab.tabGen;
import org.freertr.user.userFilter;
import org.freertr.pipe.pipeFonts;
import org.freertr.user.userHelp;
import org.freertr.user.userLine;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.debugger;
import org.freertr.util.logger;
import org.freertr.util.notifier;

/**
 * remote frame buffer (rfc6143) server
 *
 * @author matecsaba
 */
public class servRfb extends servGeneric implements prtServS {

    /**
     * create instance
     */
    public servRfb() {
    }

    /**
     * port number
     */
    public final static int port = 5900;

    private userLine lin = new userLine();

    /**
     * defaults text
     */
    public final static userFilter[] defaultF = {
        new userFilter("server rfb .*", cmds.tabulator + "port " + port, null),
        new userFilter("server rfb .*", cmds.tabulator + "protocol " + proto2string(protoAllStrm), null)
    };

    public userFilter[] srvDefFlt() {
        return defaultF;
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.setTime(120000);
        pipe.lineRx = pipeSide.modTyp.modeLF;
        pipe.lineTx = pipeSide.modTyp.modeLF;
        pipeLine pl = new pipeLine(65536, false);
        lin.createHandler(pl.getSide(), "" + id, 0);
        new servRfbConn(pipe, new pipeImage(pl.getSide(), lin.execWidth, lin.execHeight, pipeFonts.font8x16(), pipeFonts.colorData));
        return false;
    }

    public void srvShRun(String beg, List<String> lst, int filter) {
        lin.getShRun(beg, lst, filter);
    }

    public boolean srvCfgStr(cmds cmd) {
        return lin.doCfgStr(cmd);
    }

    public void srvHelp(userHelp l) {
        lin.getHelp(l);
    }

    public String srvName() {
        return "rfb";
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

}

class servRfbConn implements Runnable {

    /**
     * my version
     */
    public static String locVer = "RFB 003.008";

    /**
     * set pixel format
     */
    public final static int typSetFrm = 0;

    /**
     * fix colour map entries
     */
    public final static int typSetPal = 1;

    /**
     * set encoding
     */
    public final static int typSetEnc = 2;

    /**
     * FramebufferUpdateRequest
     */
    public final static int typFBupdate = 3;

    /**
     * key event
     */
    public final static int typKeyEvent = 4;

    /**
     * pointer event
     */
    public final static int typPntrEvnt = 5;

    /**
     * cut text
     */
    public final static int typCutText = 6;

    /**
     * convert type to string
     *
     * @param i type
     * @return string
     */
    public static String type2string(int i) {
        switch (i) {
            case typSetFrm:
                return "setPixForm";
            case typSetPal:
                return "setPalette";
            case typSetEnc:
                return "setEncoding";
            case typFBupdate:
                return "fbUpdate";
            case typKeyEvent:
                return "keyEvent";
            case typPntrEvnt:
                return "pointerEvent";
            case typCutText:
                return "cutText";
            default:
                return "unknown=" + i;
        }
    }

    private final pipeSide pipe;

    private final pipeImage img;

    private int update = 0;

    private int keyShft = 0;

    private notifier notif;

    private int mode = 0; // 0=pal, 1=byte, 2=word, 4=dword, 0x100=msb

    private int redSh1 = 0;

    private int redSh2 = 0;

    private int redSh3 = 0;

    private int grnSh1 = 0;

    private int grnSh2 = 0;

    private int grnSh3 = 0;

    private int bluSh1 = 0;

    private int bluSh2 = 0;

    private int bluSh3 = 0;

    public servRfbConn(pipeSide conn, pipeImage image) {
        pipe = conn;
        img = image;
        notif = new notifier();
        new servRfbTimer(this);
        logger.startThread(this);
    }

    public void run() {
        if (debugger.servRfbTraf) {
            logger.debug("started");
        }
        try {
            if (doStart()) {
                doShutdown();
            }
            doWork();
        } catch (Exception e) {
            logger.traceback(e);
        }
        doShutdown();
        if (debugger.servRfbTraf) {
            logger.debug("stopped");
        }
    }

    private boolean doStart() {
        pipe.linePut(locVer);
        String s = pipe.lineGet(1);
        int ver = s.indexOf(" ");
        if (ver < 0) {
            ver = 0;
        }
        ver = bits.str2num(s.substring(ver, s.length()).replaceAll("\\.", ""));
        if (debugger.servRfbTraf) {
            logger.debug("version=" + ver + " rem=" + s + " loc=" + locVer);
        }
        byte[] buf;
        if (ver >= 3007) {
            buf = new byte[2];
            buf[0] = 1; // protocols
            buf[1] = 1; // none
            pipe.morePut(buf, 0, buf.length);
            buf = new byte[1];
            if (pipe.moreGet(buf, 0, buf.length) != buf.length) {
                return true;
            }
            buf = new byte[4];
            bits.msbPutD(buf, 0, 0); // ok
            pipe.morePut(buf, 0, buf.length);
        } else {
            buf = new byte[4];
            bits.msbPutD(buf, 0, 1); // none
            pipe.morePut(buf, 0, buf.length);
        }
        buf = new byte[1];
        if (pipe.moreGet(buf, 0, buf.length) != buf.length) {
            return true;
        }
        if (debugger.servRfbTraf) {
            logger.debug("shared=" + buf[0]);
        }
        buf = new byte[24];
        bits.msbPutW(buf, 0, img.img1[0].length); // width
        bits.msbPutW(buf, 2, img.img1.length); // height
        buf[4] = 8; // bits in pixel
        buf[5] = 8; // depth
        buf[6] = 1; // msb
        buf[7] = 0; // paletted
        bits.msbPutW(buf, 8, 0); // red max
        bits.msbPutW(buf, 10, 0); // green max
        bits.msbPutW(buf, 12, 0); // blue max
        buf[14] = 0; // red shift
        buf[15] = 0; // red shift
        buf[16] = 0; // red shift
        bits.msbPutD(buf, 17, 0); // padding
        bits.msbPutD(buf, 20, cfgAll.hostName.length());
        pipe.morePut(buf, 0, buf.length);
        pipe.strPut(cfgAll.hostName);
        return false;
    }

    protected void doShutdown() {
        img.pipe.setClose();
        pipe.setClose();
    }

    protected boolean doTimer() {
        if (pipe.isClosed() != 0) {
            return true;
        }
        if (img.pipe.isClosed() != 0) {
            return true;
        }
        notif.misleep(1000);
        for (;;) {
            if (img.scr.doRound(false)) {
                break;
            }
        }
        if (!img.scr.changed) {
            return false;
        }
        if (update < 1) {
            return false;
        }
        img.doImage();
        img.scr.changed = false;
        update--;
        sendUpdate();
        return false;
    }

    private void doWork() {
        for (;;) {
            if (doRound()) {
                break;
            }
        }
    }

    private int getShift(int v) {
        for (int i = 31; i >= 0; i--) {
            if ((v & (1 << i)) != 0) {
                return i + 1;
            }
        }
        return 0;
    }

    private boolean doRound() {
        byte[] buf = new byte[1];
        if (pipe.moreGet(buf, 0, buf.length) != buf.length) {
            return true;
        }
        if (debugger.servRfbTraf) {
            logger.debug("rx=" + type2string(buf[0]));
        }
        switch (buf[0]) {
            case typSetFrm:
                buf = new byte[3];
                if (pipe.moreGet(buf, 0, buf.length) != buf.length) {
                    return true;
                }
                buf = new byte[16];
                if (pipe.moreGet(buf, 0, buf.length) != buf.length) {
                    return true;
                }
                mode = 0;
                if (buf[3] != 0) {
                    switch (buf[0]) {
                        case 8:
                            mode = 1;
                            break;
                        case 16:
                            mode = 2;
                            break;
                        case 32:
                            mode = 4;
                            break;
                    }
                    redSh3 = bits.msbGetW(buf, 4);
                    grnSh3 = bits.msbGetW(buf, 6);
                    bluSh3 = bits.msbGetW(buf, 8);
                    redSh1 = 24 - getShift(redSh3);
                    grnSh1 = 16 - getShift(grnSh3);
                    bluSh1 = 8 - getShift(bluSh3);
                    redSh2 = buf[10];
                    grnSh2 = buf[11];
                    bluSh2 = buf[12];
                }
                if ((buf[2] != 0) && (mode > 1)) {
                    mode |= 0x100;
                }
                if (mode == 0) {
                    sendPalette();
                }
                if (debugger.servRfbTraf) {
                    logger.debug("mode=" + mode + " red=" + redSh1 + "," + redSh2 + "," + redSh3 + " green=" + grnSh1 + "," + grnSh2 + "," + grnSh3 + " blue=" + bluSh1 + "," + bluSh2 + "," + bluSh3);
                }
                break;
            case typSetPal:
                buf = new byte[5];
                if (pipe.moreGet(buf, 0, buf.length) != buf.length) {
                    return true;
                }
                buf = new byte[bits.msbGetW(buf, 3) * 6];
                if (pipe.moreGet(buf, 0, buf.length) != buf.length) {
                    return true;
                }
                break;
            case typSetEnc:
                buf = new byte[3];
                if (pipe.moreGet(buf, 0, buf.length) != buf.length) {
                    return true;
                }
                buf = new byte[bits.msbGetW(buf, 1) * 4];
                if (pipe.moreGet(buf, 0, buf.length) != buf.length) {
                    return true;
                }
                break;
            case typFBupdate:
                buf = new byte[9];
                if (pipe.moreGet(buf, 0, buf.length) != buf.length) {
                    return true;
                }
                gotUpdate(buf[0] != 0, bits.msbGetW(buf, 1), bits.msbGetW(buf, 3), bits.msbGetW(buf, 5), bits.msbGetW(buf, 7));
                break;
            case typKeyEvent:
                buf = new byte[7];
                if (pipe.moreGet(buf, 0, buf.length) != buf.length) {
                    return true;
                }
                gotKey(buf[0] != 0, bits.msbGetD(buf, 3));
                break;
            case typPntrEvnt:
                buf = new byte[5];
                if (pipe.moreGet(buf, 0, buf.length) != buf.length) {
                    return true;
                }
                if (debugger.servRfbTraf) {
                    logger.debug("but=" + buf[0] + " x=" + bits.msbGetW(buf, 1) + " y=" + bits.msbGetW(buf, 3));
                }
                break;
            case typCutText:
                buf = new byte[5];
                if (pipe.moreGet(buf, 0, buf.length) != buf.length) {
                    return true;
                }
                buf = new byte[bits.msbGetD(buf, 1)];
                if (debugger.servRfbTraf) {
                    logger.debug("len=" + buf.length);
                }
                if (pipe.moreGet(buf, 0, buf.length) != buf.length) {
                    return true;
                }
                break;
            default:
                return true;
        }
        return false;
    }

    private void sendPalette() {
        if (debugger.servRfbTraf) {
            logger.debug("tx palette");
        }
        byte[] buf = new byte[6];
        buf[0] = 1; // setColorMap
        buf[1] = 0; // pad
        bits.msbPutW(buf, 2, 0); // first
        bits.msbPutW(buf, 4, img.pal.length); // first
        pipe.morePut(buf, 0, buf.length);
        buf = new byte[img.pal.length * 6];
        bits.byteFill(buf, 0, buf.length, 0);
        int pos = 0;
        for (int i = 0; i < img.pal.length; i++) {
            final int col = img.pal[i];
            buf[pos + 0] = (byte) ((col >>> 16) & 0xff);
            buf[pos + 2] = (byte) ((col >>> 8) & 0xff);
            buf[pos + 4] = (byte) (col & 0xff);
            pos += 6;
        }
        pipe.morePut(buf, 0, buf.length);
    }

    private void sendUpdate() {
        final int sizX = img.img1[0].length;
        final int sizY = img.img1.length;
        int sizP = mode & 0xff;
        if (sizP < 1) {
            sizP = 1;
        }
        if (debugger.servRfbTraf) {
            logger.debug("tx update " + sizX + "x" + sizY + "x" + sizP);
        }
        byte[] buf = new byte[20];
        buf[0] = 0; // FBupdate
        buf[1] = 0; // pad
        bits.msbPutW(buf, 2, 1); // # of rectangles
        bits.msbPutW(buf, 4, 0); // begin x
        bits.msbPutW(buf, 6, 0); // begin y
        bits.msbPutW(buf, 8, sizX); // size x
        bits.msbPutW(buf, 10, sizY); // size y
        bits.msbPutD(buf, 12, 0); // raw encoding
        bits.msbPutD(buf, 16, sizX * sizY * sizP); // byte size
        pipe.morePut(buf, 0, buf.length);
        for (int y = 0; y < sizY; y++) {
            buf = new byte[sizX * sizP];
            for (int x = 0; x < sizX; x++) {
                int i = img.img2[y][x];
                i = (((i >>> redSh1) & redSh3) << redSh2) | (((i >>> grnSh1) & grnSh3) << grnSh2) | (((i >>> bluSh1) & bluSh3) << bluSh2);
                switch (mode) {
                    case 0x000:
                    case 0x100:
                        buf[x] = (byte) img.img1[y][x];
                        break;
                    case 0x001:
                    case 0x101:
                        buf[x] = (byte) i;
                        break;
                    case 0x002:
                        bits.lsbPutW(buf, x * 2, i);
                        break;
                    case 0x102:
                        bits.msbPutW(buf, x * 2, i);
                        break;
                    case 0x004:
                        bits.lsbPutD(buf, x * 4, i);
                        break;
                    case 0x104:
                        bits.msbPutD(buf, x * 4, i);
                        break;
                }
            }
            pipe.morePut(buf, 0, buf.length);
        }
    }

    private void gotUpdate(boolean inc, int begX, int begY, int sizX, int sizY) {
        if (debugger.servRfbTraf) {
            logger.debug("incr=" + inc + " bx=" + begX + " by=" + begY + " sx=" + sizX + " sy=" + sizY);
        }
        img.scr.changed |= !inc;
        update++;
        notif.wakeup();
    }

    private void gotKey(boolean down, int key) {
        if (debugger.servRfbTraf) {
            logger.debug("down=" + down + " key=" + key);
        }
        if (!down) {
            key |= 0x40000000;
        }
        String p = "";
        if ((keyShft & 0xc0) != 0) {
            p = "\033";
        }
        byte[] buf = new byte[1];
        buf[0] = (byte) (key & 0xff);
        if (buf[0] == key) {
            if ((keyShft & 0xc) != 0) {
                buf[0] &= 0x1f;
            }
            img.pipe.strPut(p + new String(buf));
            return;
        }
        switch (key) {
            case 0xff08: // backspace
                img.pipe.strPut(p + "\010");
                return;
            case 0xff09: // tabulator
                img.pipe.strPut(p + "\011");
                return;
            case 0xff0d: // enter
                img.pipe.strPut(p + "\015");
                return;
            case 0xff1b: // escape
                img.pipe.strPut(p + "\033");
                return;
            case 0xff63: // insert
                img.pipe.strPut(p + "\033[2~");
                return;
            case 0xffff: // delete
                img.pipe.strPut(p + "\033[3~");
                return;
            case 0xff50: // home
                img.pipe.strPut(p + "\033[1~");
                return;
            case 0xff57: // end
                img.pipe.strPut(p + "\033[4~");
                return;
            case 0xff55: // pgup
                img.pipe.strPut(p + "\033[5~");
                return;
            case 0xff56: // pgdn
                img.pipe.strPut(p + "\033[6~");
                return;
            case 0xff52: // up
                img.pipe.strPut(p + "\033[A");
                return;
            case 0xff54: // down
                img.pipe.strPut(p + "\033[B");
                return;
            case 0xff51: // left
                img.pipe.strPut(p + "\033[D");
                return;
            case 0xff53: // right
                img.pipe.strPut(p + "\033[C");
                return;
            case 0xffbe: // f1
                img.pipe.strPut(p + "\033[11~");
                return;
            case 0xffbf: // f2
                img.pipe.strPut(p + "\033[12~");
                return;
            case 0xffc0: // f3
                img.pipe.strPut(p + "\033[13~");
                return;
            case 0xffc1: // f4
                img.pipe.strPut(p + "\033[14~");
                return;
            case 0xffc2: // f5
                img.pipe.strPut(p + "\033[15~");
                return;
            case 0xffc3: // f6
                img.pipe.strPut(p + "\033[17~");
                return;
            case 0xffc4: // f7
                img.pipe.strPut(p + "\033[18~");
                return;
            case 0xffc5: // f8
                img.pipe.strPut(p + "\033[19~");
                return;
            case 0xffc6: // f9
                img.pipe.strPut(p + "\033[20~");
                return;
            case 0xffc7: // f10
                img.pipe.strPut(p + "\033[21~");
                return;
            case 0xffc8: // f11
                img.pipe.strPut(p + "\033[23~");
                return;
            case 0xffc9: // f12
                img.pipe.strPut(p + "\033[24~");
                return;
            case 0xffe1: // left shift
                keyShft |= 0x1;
                return;
            case 0xffe2: // right shift
                keyShft |= 0x2;
                return;
            case 0xffe3: // left ctrl
                keyShft |= 0x4;
                return;
            case 0xffe4: // right ctrl
                keyShft |= 0x8;
                return;
            case 0xffe7: // left meta
                keyShft |= 0x10;
                return;
            case 0xffe8: // right meta
                keyShft |= 0x20;
                return;
            case 0xffe9: // left alt
                keyShft |= 0x40;
                return;
            case 0xffea: // right alt
                keyShft |= 0x80;
                return;
            case 0x4000ffe1: // left shift
                keyShft &= ~0x1;
                return;
            case 0x4000ffe2: // right shift
                keyShft &= ~0x2;
                return;
            case 0x4000ffe3: // left ctrl
                keyShft &= ~0x4;
                return;
            case 0x4000ffe4: // right ctrl
                keyShft &= ~0x8;
                return;
            case 0x4000ffe7: // left meta
                keyShft &= ~0x10;
                return;
            case 0x4000ffe8: // right meta
                keyShft &= ~0x20;
                return;
            case 0x4000ffe9: // left alt
                keyShft &= ~0x40;
                return;
            case 0x4000ffea: // right alt
                keyShft &= ~0x80;
                return;
        }
    }

}

class servRfbTimer implements Runnable {

    private final servRfbConn parent;

    public servRfbTimer(servRfbConn lower) {
        parent = lower;
        logger.startThread(this);
    }

    public void run() {
        try {
            for (;;) {
                if (parent.doTimer()) {
                    break;
                }
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
        parent.doShutdown();
    }

}
