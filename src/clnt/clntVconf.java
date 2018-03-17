package clnt;

import cfg.cfgAll;
import cfg.cfgDial;
import java.util.Comparator;
import java.util.Timer;
import java.util.TimerTask;
import pack.packHolder;
import pack.packRtp;
import pipe.pipeLine;
import pipe.pipeSide;
import snd.sndCodec;
import tab.tabGen;
import util.bits;
import util.cmds;
import util.logger;

/**
 * voice conference client
 *
 * @author matecsaba
 */
public class clntVconf implements Runnable {

    /**
     * time when started
     */
    public long started;

    /**
     * need prompt
     */
    public boolean prompt = false;

    /**
     * calling number
     */
    public String calling;

    /**
     * get events
     */
    public boolean events;

    /**
     * control pipe
     */
    protected final pipeSide user;

    /**
     * participants
     */
    protected final tabGen<clntVconfPeer> peers = new tabGen<clntVconfPeer>();

    private pipeSide reported;

    /**
     * create instance
     */
    public clntVconf() {
        pipeLine pip = new pipeLine(32768, false);
        reported = pip.getSide();
        user = pip.getSide();
        user.timeout = 120000;
        user.lineTx = pipeSide.modTyp.modeCRLF;
        user.lineRx = pipeSide.modTyp.modeCRtryLF;
        started = bits.getTime();
    }

    /**
     * add one participant
     *
     * @param called called number
     * @return false on success, true on error
     */
    public boolean addPeer(String called) {
        clntVconfPeer ntry = new clntVconfPeer(this, called);
        clntVconfPeer old = peers.add(ntry);
        if (old != null) {
            return true;
        }
        ntry.startWork();
        return false;
    }

    /**
     * get control pipe
     *
     * @return pipe
     */
    public pipeSide getPipe() {
        return reported;
    }

    /**
     * start work
     */
    public void startWork() {
        new Thread(this).start();
        new clntVconfWork(this);
    }

    public void run() {
        try {
            doer();
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

    private void sendStatus() {
        user.linePut("status peers=" + peers.size() + " time=" + bits.timePast(started));
    }

    private void doer() {
        for (;;) {
            if (user.isClosed() != 0) {
                return;
            }
            String a;
            if (prompt) {
                user.strPut("conference>");
                a = user.lineGet(0x32);
            } else {
                a = user.lineGet(1);
            }
            cmds cmd = new cmds("conf", a);
            a = cmd.word().toLowerCase();
            if (a.length() < 1) {
                continue;
            }
            if (a.equals("help")) {
                user.linePut("help commands:");
                user.linePut("help   exit");
                user.linePut("help   sleep <num>");
                user.linePut("help   echo <str>");
                user.linePut("help   events <0/1>");
                user.linePut("help   calling <addr>");
                user.linePut("help   list");
                user.linePut("help   status");
                user.linePut("help   display");
                user.linePut("help   add <addr>");
                user.linePut("help   del <addr>");
                user.linePut("help   vol-in <vol> <addr>");
                user.linePut("help   vol-out <vol> <addr>");
                user.linePut("help responses:");
                user.linePut("help   fail <addr>");
                user.linePut("help   join <addr>");
                user.linePut("help   leave <addr>");
                user.linePut("help   list <addr>");
                user.linePut("help   status <info> ...");
                user.linePut("help   error <reason>");
                user.linePut("help end");
                continue;
            }
            if (a.equals("exit")) {
                user.setClose();
                return;
            }
            if (a.equals("echo")) {
                user.linePut(cmd.getRemaining());
                continue;
            }
            if (a.equals("status")) {
                sendStatus();
                continue;
            }
            if (a.equals("display")) {
                for (;;) {
                    if (user.isClosed() != 0) {
                        break;
                    }
                    if (user.ready2rx() > 0) {
                        break;
                    }
                    bits.sleep(1000);
                    sendStatus();
                }
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
            if (a.equals("events")) {
                events = cmd.word().equals("1");
                continue;
            }
            if (a.equals("calling")) {
                calling = cmd.getRemaining();
                continue;
            }
            if (a.equals("list")) {
                for (int i = 0; i < peers.size(); i++) {
                    clntVconfPeer ntry = peers.get(i);
                    if (ntry == null) {
                        continue;
                    }
                    user.linePut("list " + ntry.target);
                }
                continue;
            }
            if (a.equals("add")) {
                addPeer(cmd.getRemaining());
                continue;
            }
            if (a.equals("del")) {
                clntVconfPeer ntry = new clntVconfPeer(this, cmd.getRemaining());
                ntry = peers.find(ntry);
                if (ntry == null) {
                    user.linePut("error not-found");
                    continue;
                }
                ntry.need2run = false;
                continue;
            }
            if (a.equals("vol-in")) {
                int i = bits.str2num(cmd.word());
                clntVconfPeer ntry = new clntVconfPeer(this, cmd.getRemaining());
                ntry = peers.find(ntry);
                if (ntry == null) {
                    user.linePut("error not-found");
                    continue;
                }
                ntry.volIn = i;
                continue;
            }
            if (a.equals("vol-out")) {
                int i = bits.str2num(cmd.word());
                clntVconfPeer ntry = new clntVconfPeer(this, cmd.getRemaining());
                ntry = peers.find(ntry);
                if (ntry == null) {
                    user.linePut("error not-found");
                    continue;
                }
                ntry.volOut = i;
                continue;
            }
            user.linePut("error bad-command");
        }
    }

}

class clntVconfWork extends TimerTask {

    public final static int paySiz = 160;

    public final static int payInt = 1000 / (8000 / paySiz);

    public final static int samMin = -32767;

    public final static int samMax = 32767;

    public final clntVconf lower;

    private Timer keepTimer = new Timer();

    private packHolder pck = new packHolder(true, true);

    private byte[] buf = new byte[paySiz];

    private int[] sam = new int[paySiz];

    public clntVconfWork(clntVconf parent) {
        lower = parent;
        keepTimer.scheduleAtFixedRate(this, 10, payInt);
    }

    public void run() {
        try {
            doer();
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

    private static int limitSam(int o) {
        if (o < samMin) {
            o = samMin;
        }
        if (o > samMax) {
            o = samMax;
        }
        return o;
    }

    public void doer() {
        if ((lower.user.isClosed() != 0) && (lower.peers.size() < 1)) {
            keepTimer.cancel();
            return;
        }
        for (int i = 0; i < paySiz; i++) {
            sam[i] = 0;
        }
        for (int p = 0; p < lower.peers.size(); p++) {
            clntVconfPeer ntry = lower.peers.get(p);
            if (ntry == null) {
                continue;
            }
            if (ntry.codec == null) {
                continue;
            }
            int o = ntry.pipeUsr.nonBlockGet(buf, 0, paySiz);
            if (o < 0) {
                ntry.sam = new int[paySiz];
                continue;
            }
            bits.byteFill(buf, o, paySiz - o, 0);
            ntry.sam = ntry.codec.decodeBuf(buf, 0, paySiz);
            for (int i = 0; i < paySiz; i++) {
                o = (ntry.sam[i] * ntry.volIn) / 100;
                ntry.sam[i] = o;
                sam[i] += o;
            }
        }
        for (int p = 0; p < lower.peers.size(); p++) {
            clntVconfPeer ntry = lower.peers.get(p);
            if (ntry == null) {
                continue;
            }
            if (ntry.codec == null) {
                continue;
            }
            pck.clear();
            for (int i = 0; i < paySiz; i++) {
                int o = sam[i] - ntry.sam[i];
                o = (o * ntry.volOut) / 100;
                o = limitSam(o);
                o = ntry.codec.encodeInt(o);
                pck.putByte(i, o);
            }
            pck.putSkip(paySiz);
            pck.merge2end();
            ntry.sendPack(pck);
        }
    }

}

class clntVconfPeer implements Runnable, Comparator<clntVconfPeer> {

    public final clntVconf lower;

    public final String target;

    public final pipeSide pipeUsr;

    public boolean need2run;

    private final pipeSide pipeOwn;

    public int[] sam = new int[clntVconfWork.paySiz];

    public sndCodec codec;

    public int volIn = 100;

    public int volOut = 100;

    private cfgDial peer;

    private String callId;

    private packRtp rtp;

    private int syncSrc;

    public clntVconfPeer(clntVconf parent, String called) {
        lower = parent;
        target = called;
        pipeLine pip = new pipeLine(1024, false);
        pipeUsr = pip.getSide();
        pipeOwn = pip.getSide();
    }

    public int compare(clntVconfPeer o1, clntVconfPeer o2) {
        return o1.target.compareTo(o2.target);
    }

    public void startWork() {
        need2run = true;
        new Thread(this).start();
    }

    public void run() {
        try {
            doer();
        } catch (Exception e) {
            logger.traceback(e);
        }
        lower.peers.del(this);
        pipeUsr.setClose();
        pipeOwn.setClose();
        if (rtp != null) {
            rtp.setClose();
        }
        if (peer != null) {
            peer.stopCall(callId);
        }
    }

    public void sendPack(packHolder pck) {
        if (rtp == null) {
            return;
        }
        pck.RTPtyp = codec.getRTPtype();
        pck.RTPsrc = syncSrc;
        rtp.sendPack(pck);
    }

    public void doer() {
        syncSrc = bits.randomD();
        peer = cfgAll.dialFind(lower.calling, target, null);
        if (peer == null) {
            if (lower.events) {
                lower.user.linePut("failed " + target);
            }
            return;
        }
        callId = peer.makeCall(lower.calling, target);
        if (callId == null) {
            if (lower.events) {
                lower.user.linePut("failed " + target);
            }
            return;
        }
        rtp = peer.getCall(callId);
        codec = peer.getCodec();
        packHolder pck = new packHolder(true, true);
        if (lower.events) {
            lower.user.linePut("join " + target);
        }
        for (;;) {
            if (!need2run) {
                break;
            }
            if (rtp.isClosed() != 0) {
                break;
            }
            if (rtp.recvPack(pck, true) < 1) {
                break;
            }
            if (pck.RTPtyp != codec.getRTPtype()) {
                continue;
            }
            byte[] buf = pck.getCopy();
            pipeOwn.nonBlockPut(buf, 0, buf.length);
        }
        if (lower.events) {
            lower.user.linePut("leave " + target);
        }
    }

}
