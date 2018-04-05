package snd;

import cfg.cfgAll;
import cfg.cfgDial;
import java.io.RandomAccessFile;
import pack.packRtp;
import pipe.pipeModem;
import pipe.pipeSide;
import util.bits;
import util.cmds;
import util.logger;

/**
 * sound script handler
 *
 * @author matecsaba
 */
public class sndScript implements Runnable {

    /**
     * need prompt
     */
    public boolean prompt = false;

    private final sndCodec codr;

    private final String calSrc;

    private final String calTrg;

    private pipeSide user;

    private packRtp strm;

    private cfgDial per;

    private String rcd;

    private packRtp fwd;

    private sndWave play;

    private sndWave rec;

    private String recF;

    private sndWave dtmf;

    /**
     * make script handler
     *
     * @param script pipe to communicate to
     * @param codec codec to use
     * @param rtp voice connection
     * @param calling called number
     * @param called calling number
     */
    public sndScript(pipeSide script, sndCodec codec, packRtp rtp, String calling, String called) {
        user = script;
        codr = codec;
        strm = rtp;
        calSrc = calling;
        calTrg = called;
        new Thread(this).start();
    }

    public void run() {
        try {
            doer();
        } catch (Exception e) {
            logger.traceback(e);
        }
        try {
            per.stopCall(rcd);
        } catch (Exception e) {
        }
        try {
            user.setClose();
        } catch (Exception e) {
        }
        try {
            strm.setClose();
        } catch (Exception e) {
        }
        try {
            fwd.setClose();
        } catch (Exception e) {
        }
    }

    private void doer() {
        for (;;) {
            if (play != null) {
                if (play.isStopped()) {
                    user.linePut("play-stopped");
                    play = null;
                }
            }
            if (rec != null) {
                String s = rec.getDtmf();
                if (s.length() > 0) {
                    user.linePut("dtmf-code " + s);
                }
                if (rec.isStopped()) {
                    if (bits.byteSave(true, rec.buf, recF)) {
                        user.linePut("error errorsaving");
                    }
                    user.linePut("record-stopped");
                    rec = null;
                    recF = null;
                }
            }
            if (dtmf != null) {
                String s = dtmf.getDtmf();
                if (s.length() > 0) {
                    user.linePut("dtmf-code " + s);
                }
                if (dtmf.isStopped()) {
                    user.linePut("dtmf-stopped");
                    dtmf = null;
                }
            }
            if (fwd != null) {
                if (fwd.isClosed() != 0) {
                    user.linePut("forward-stopped");
                    if (per != null) {
                        per.stopCall(rcd);
                    }
                    fwd.setClose();
                    fwd = null;
                    per = null;
                }
            }
            if (strm.isClosed() != 0) {
                user.linePut("hangup");
                break;
            }
            if (user.isClosed() != 0) {
                break;
            }
            if (user.ready2rx() < 1) {
                bits.sleep(100);
                continue;
            }
            String a;
            if (prompt) {
                user.strPut("voice>");
                a = user.lineGet(0x32);
            } else {
                a = user.lineGet(1);
            }
            cmds cmd = new cmds("voice", a);
            a = cmd.word().toLowerCase();
            if (a.length() < 1) {
                continue;
            }
            if (a.equals("help")) {
                user.linePut("help commands:");
                user.linePut("help   sleep <num>");
                user.linePut("help   echo <str>");
                user.linePut("help   play-start <vaw>");
                user.linePut("help   play-stop");
                user.linePut("help   play-running");
                user.linePut("help   play-wait");
                user.linePut("help   record-start <vaw>");
                user.linePut("help   record-stop");
                user.linePut("help   record-running");
                user.linePut("help   record-wait");
                user.linePut("help   dtmf-start");
                user.linePut("help   dtmf-stop");
                user.linePut("help   dtmf-running");
                user.linePut("help   dtmf-wait");
                user.linePut("help   calling");
                user.linePut("help   called");
                user.linePut("help   forward-start <srcaddr> <trgaddr>");
                user.linePut("help   forward-stop");
                user.linePut("help   forward-running");
                user.linePut("help   forward-wait");
                user.linePut("help   modem-answer");
                user.linePut("help   modem-originate");
                user.linePut("help   hangup");
                user.linePut("help responses:");
                user.linePut("help   hangup");
                user.linePut("help   forwarded");
                user.linePut("help   calling <addr>");
                user.linePut("help   called <addr>");
                user.linePut("help   forward-stopped");
                user.linePut("help   forward-running <true/false>");
                user.linePut("help   play-stopped");
                user.linePut("help   play-running <true/false>");
                user.linePut("help   record-stopped");
                user.linePut("help   record-running <true/false>");
                user.linePut("help   dtmf-stopped");
                user.linePut("help   dtmf-code <char>");
                user.linePut("help   dtmf-running <true/false>");
                user.linePut("help   error <reason>");
                user.linePut("help end");
                continue;
            }
            if (a.equals("echo")) {
                user.linePut(cmd.getRemaining());
                continue;
            }
            if (a.equals("sleep")) {
                int i = bits.str2num(cmd.word());
                if (i < 1) {
                    continue;
                }
                bits.sleep(i);
                continue;
            }
            if (a.equals("calling")) {
                user.linePut("calling " + calSrc);
                continue;
            }
            if (a.equals("called")) {
                user.linePut("called " + calTrg);
                continue;
            }
            if (a.equals("hangup")) {
                strm.setClose();
                continue;
            }
            if (a.equals("play-stop")) {
                if (play == null) {
                    user.linePut("error not-playing");
                    continue;
                }
                play.stopWork();
                continue;
            }
            if (a.equals("record-stop")) {
                if (rec == null) {
                    user.linePut("error not-recording");
                    continue;
                }
                rec.stopWork();
                continue;
            }
            if (a.equals("dtmf-stop")) {
                if (dtmf == null) {
                    user.linePut("error not-detecting");
                    continue;
                }
                dtmf.stopWork();
                continue;
            }
            if (a.equals("forward-stop")) {
                if (fwd == null) {
                    user.linePut("error not-forwarding");
                    continue;
                }
                if (per != null) {
                    per.stopCall(rcd);
                }
                fwd.setClose();
                continue;
            }
            if (a.equals("play-wait")) {
                if (play == null) {
                    continue;
                }
                play.wait4stop();
                continue;
            }
            if (a.equals("record-wait")) {
                if (rec == null) {
                    continue;
                }
                rec.wait4stop();
                continue;
            }
            if (a.equals("dtmf-wait")) {
                if (dtmf == null) {
                    continue;
                }
                dtmf.wait4stop();
                continue;
            }
            if (a.equals("forward-wait")) {
                if (fwd == null) {
                    continue;
                }
                for (;;) {
                    if (fwd.isClosed() != 0) {
                        break;
                    }
                    bits.sleep(100);
                }
                continue;
            }
            if (a.equals("play-running")) {
                user.linePut("play-running " + (play != null));
                continue;
            }
            if (a.equals("record-running")) {
                user.linePut("record-running " + (rec != null));
                continue;
            }
            if (a.equals("dtmf-running")) {
                user.linePut("dtmf-running " + (dtmf != null));
                continue;
            }
            if (a.equals("forward-running")) {
                user.linePut("forward-running " + (fwd != null));
                continue;
            }
            if (a.equals("forward-start")) {
                if (fwd != null) {
                    user.linePut("error already-forwarding");
                    continue;
                }
                if (rec != null) {
                    user.linePut("error already-recording");
                    continue;
                }
                if (dtmf != null) {
                    user.linePut("error already-detecting");
                    continue;
                }
                if (play != null) {
                    user.linePut("error already-playing");
                    continue;
                }
                a = cmd.word();
                String s = cmd.getRemaining();
                per = cfgAll.dialFind(a, s, null);
                if (per == null) {
                    user.linePut("error bad-number");
                    continue;
                }
                rcd = per.makeCall(a, s);
                if (rcd == null) {
                    user.linePut("error failed-call");
                    continue;
                }
                fwd = per.getCall(rcd);
                new sndConnect(strm, fwd, codr, per.getCodec());
                user.linePut("forwarded");
                continue;
            }
            if (a.equals("play-start")) {
                if (fwd != null) {
                    user.linePut("error already-forwarding");
                    continue;
                }
                if (play != null) {
                    user.linePut("error already-playing");
                    continue;
                }
                byte[] buf = null;
                try {
                    RandomAccessFile fr = new RandomAccessFile(cmd.getRemaining(), "r");
                    buf = new byte[(int) fr.length()];
                    fr.read(buf, 0, buf.length);
                    fr.close();
                } catch (Exception e) {
                    user.linePut("error no-file");
                    continue;
                }
                play = new sndWave(codr, strm);
                play.buf = buf;
                play.startPlay();
                continue;
            }
            if (a.equals("record-start")) {
                if (fwd != null) {
                    user.linePut("error already-forwarding");
                    continue;
                }
                if (rec != null) {
                    user.linePut("error already-recording");
                    continue;
                }
                if (dtmf != null) {
                    user.linePut("error already-detecting");
                    continue;
                }
                recF = cmd.getRemaining();
                rec = new sndWave(codr, strm);
                rec.startRecord();
                continue;
            }
            if (a.equals("dtmf-start")) {
                if (fwd != null) {
                    user.linePut("error already-forwarding");
                    continue;
                }
                if (rec != null) {
                    user.linePut("error already-recording");
                    continue;
                }
                if (dtmf != null) {
                    user.linePut("error already-detecting");
                    continue;
                }
                dtmf = new sndWave(codr, strm);
                dtmf.startDtmf();
                continue;
            }
            if (a.equals("modem-answer")) {
                if (fwd != null) {
                    user.linePut("error already-forwarding");
                    continue;
                }
                if (rec != null) {
                    user.linePut("error already-recording");
                    continue;
                }
                if (dtmf != null) {
                    user.linePut("error already-detecting");
                    continue;
                }
                if (play != null) {
                    user.linePut("error already-playing");
                    continue;
                }
                pipeModem.answer(user, codr, strm);
                strm = null;
                user = null;
                return;
            }
            if (a.equals("modem-originate")) {
                if (fwd != null) {
                    user.linePut("error already-forwarding");
                    continue;
                }
                if (rec != null) {
                    user.linePut("error already-recording");
                    continue;
                }
                if (dtmf != null) {
                    user.linePut("error already-detecting");
                    continue;
                }
                if (play != null) {
                    user.linePut("error already-playing");
                    continue;
                }
                pipeModem.originate(user, codr, strm);
                strm = null;
                user = null;
                return;
            }
            user.linePut("error bad-command");
        }
        if (rec != null) {
            rec.stopWork();;
            rec.wait4stop();
            bits.byteSave(true, rec.buf, recF);
        }
        if (play != null) {
            play.stopWork();
            play.wait4stop();
        }
        if (dtmf != null) {
            dtmf.stopWork();
            dtmf.wait4stop();
        }
        if (fwd != null) {
            if (per != null) {
                per.stopCall(rcd);
            }
            fwd.setClose();
        }
    }

}
