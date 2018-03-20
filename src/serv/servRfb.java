package serv;

import cfg.cfgAll;
import java.util.List;
import pipe.pipeImage;
import pipe.pipeLine;
import pipe.pipeSide;
import prt.prtGenConn;
import prt.prtServS;
import tab.tabGen;
import user.userFilter;
import user.userHelping;
import user.userLine;
import user.userFonts1;
import util.bits;
import util.cmds;
import util.debugger;
import util.logger;
import util.notifier;

/**
 * remote frame buffer (rfc6143) server
 *
 * @author matecsaba
 */
public class servRfb extends servGeneric implements prtServS {

    /**
     * port number
     */
    public static final int port = 5900;

    private userLine lin = new userLine();

    /**
     * defaults text
     */
    public final static String defaultL[] = {
        "server rfb .*! port " + port,
        "server rfb .*! protocol " + proto2string(protoAllStrm)
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public tabGen<userFilter> srvDefFlt() {
        return defaultF;
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.timeout = 120000;
        pipe.lineRx = pipeSide.modTyp.modeLF;
        pipe.lineTx = pipeSide.modTyp.modeLF;
        pipeLine pl = new pipeLine(65536, false);
        lin.createHandler(pl.getSide(), "" + id, false);
        new servRfbConn(pipe, new pipeImage(pl.getSide(), 80, 25, userFonts1.font8x16data, userFonts1.colorData));
        return false;
    }

    public void srvShRun(String beg, List<String> lst) {
        lin.getShRun(beg, lst);
    }

    public boolean srvCfgStr(cmds cmd) {
        return lin.doCfgStr(cmd);
    }

    public void srvHelp(userHelping l) {
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
    public static final int typSetFrm = 0;

    /**
     * fix colour map entries
     */
    public static final int typSetPal = 1;

    /**
     * set encoding
     */
    public static final int typSetEnc = 2;

    /**
     * FramebufferUpdateRequest
     */
    public static final int typFBupdate = 3;

    /**
     * key event
     */
    public static final int typKeyEvent = 4;

    /**
     * pointer event
     */
    public static final int typPntrEvnt = 5;

    /**
     * cut text
     */
    public static final int typCutText = 6;

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

    public servRfbConn(pipeSide conn, pipeImage image) {
        pipe = conn;
        img = image;
        notif = new notifier();
        new servRfbTimer(this);
        new Thread(this).start();
    }

    public void run() {
        if (debugger.servRfbTraf) {
            logger.debug("started");
        }
        try {
            if (doStart()) {
                pipe.setClose();
            }
            sendPalette();
            doWork();
        } catch (Exception e) {
            logger.traceback(e);
        }
        img.setClose();
        pipe.setClose();
        if (debugger.servRfbTraf) {
            logger.debug("stopped");
        }
    }

    private boolean doStart() {
        pipe.linePut(locVer);
        String s = pipe.lineGet(1);
        int ver = bits.str2num(s.substring(s.indexOf(" "), s.length()).replaceAll("\\.", ""));
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

    protected boolean doTimer() {
        if (pipe.isClosed() != 0) {
            return true;
        }
        notif.misleep(1000);
        for (;;) {
            if (img.doRound(false)) {
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
                if (debugger.servRfbTraf) {
                    logger.debug("bits=" + buf[0] + " deep=" + buf[1]);
                }
                sendPalette();
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
        if (debugger.servRfbTraf) {
            logger.debug("tx update " + sizX + "x" + sizY);
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
        bits.msbPutD(buf, 16, sizX * sizY); // byte size
        pipe.morePut(buf, 0, buf.length);
        for (int y = 0; y < sizY; y++) {
            buf = new byte[sizX];
            for (int x = 0; x < sizX; x++) {
                buf[x] = (byte) img.img1[y][x];
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
            img.doKey(p + new String(buf));
            return;
        }
        switch (key) {
            case 0xff08: // backspace
                img.doKey(p + "\010");
                return;
            case 0xff09: // tabulator
                img.doKey(p + "\011");
                return;
            case 0xff0d: // enter
                img.doKey(p + "\015");
                return;
            case 0xff1b: // escape
                img.doKey(p + "\033");
                return;
            case 0xff63: // insert
                img.doKey(p + "\033[2~");
                return;
            case 0xffff: // delete
                img.doKey(p + "\033[3~");
                return;
            case 0xff50: // home
                img.doKey(p + "\033[1~");
                return;
            case 0xff57: // end
                img.doKey(p + "\033[4~");
                return;
            case 0xff55: // pgup
                img.doKey(p + "\033[5~");
                return;
            case 0xff56: // pgdn
                img.doKey(p + "\033[6~");
                return;
            case 0xff52: // up
                img.doKey(p + "\033[A");
                return;
            case 0xff54: // down
                img.doKey(p + "\033[B");
                return;
            case 0xff51: // left
                img.doKey(p + "\033[D");
                return;
            case 0xff53: // right
                img.doKey(p + "\033[C");
                return;
            case 0xffbe: // f1
                img.doKey(p + "\033[11~");
                return;
            case 0xffbf: // f2
                img.doKey(p + "\033[12~");
                return;
            case 0xffc0: // f3
                img.doKey(p + "\033[13~");
                return;
            case 0xffc1: // f4
                img.doKey(p + "\033[14~");
                return;
            case 0xffc2: // f5
                img.doKey(p + "\033[15~");
                return;
            case 0xffc3: // f6
                img.doKey(p + "\033[17~");
                return;
            case 0xffc4: // f7
                img.doKey(p + "\033[18~");
                return;
            case 0xffc5: // f8
                img.doKey(p + "\033[19~");
                return;
            case 0xffc6: // f9
                img.doKey(p + "\033[20~");
                return;
            case 0xffc7: // f10
                img.doKey(p + "\033[21~");
                return;
            case 0xffc8: // f11
                img.doKey(p + "\033[23~");
                return;
            case 0xffc9: // f12
                img.doKey(p + "\033[24~");
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
        new Thread(this).start();
    }

    public void run() {
        try {
            for (;;) {
                if (parent.doTimer()) {
                    return;
                }
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

}
