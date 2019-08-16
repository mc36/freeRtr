package clnt;

import cfg.cfgAll;
import cry.cryBase64;
import java.io.File;
import java.io.RandomAccessFile;
import java.util.ArrayList;
import java.util.List;
import pack.packDnsRec;
import pack.packText;
import pipe.pipeDiscard;
import pipe.pipeProgress;
import pipe.pipeSide;
import serv.servGeneric;
import serv.servSmtp;
import user.userTerminal;
import util.bits;
import util.debugger;
import util.logger;
import util.uniResLoc;
import util.version;

/**
 * simple mail transfer protocol (rfc821) client
 *
 * @author matecsaba
 */
public class clntSmtp implements Runnable {

    private pipeProgress cons;

    private pipeSide pipe;

    private String boundary;

    /**
     * sender
     */
    public String from = "";

    private String lastS;

    private String lastR;

    private String lastT;

    /**
     * target
     */
    public final List<String> rcpt = new ArrayList<String>();

    private final List<String> body = new ArrayList<String>();

    /**
     * create new client
     *
     * @param console console to use
     */
    public clntSmtp(pipeSide console) {
        cons = new pipeProgress(pipeDiscard.needAny(console));
        boundary = "boundary" + bits.randomD() + "crossing" + bits.randomD() + "line";
    }

    /**
     * clean up state
     */
    public void cleanUp() {
        try {
            pipe.setClose();
        } catch (Exception e) {
        }
    }

    private void sendLine(String s) {
        if (debugger.clntSmtpTraf) {
            logger.debug("tx:" + s);
        }
        cons.debugTx(s);
        pipe.linePut(s);
        lastT = s;
    }

    private String getLine() {
        packText pck = new packText(pipe);
        String s = pck.codedRecv();
        if (s == null) {
            s = "";
        }
        if (debugger.clntSmtpTraf) {
            logger.debug("rx:" + s);
        }
        cons.debugRx(s);
        lastR = s;
        return s;
    }

    private int getRes(int div) {
        String s = getLine();
        s = s.trim();
        int i = s.indexOf(" ");
        if (i >= 0) {
            s = s.substring(0, i);
        }
        i = bits.str2num(s);
        if (debugger.clntSmtpTraf) {
            logger.debug("rx:" + i);
        }
        return i / div;
    }

    /**
     * write header
     *
     * @param from source
     * @param rcpt target
     * @param subj subject
     */
    public void putHead(String from, String rcpt, String subj) {
        long tim = bits.getTime() + cfgAll.timeServerOffset;
        body.add("From: " + from);
        body.add("To: " + rcpt);
        body.add("Subject: " + subj);
        body.add("Date: " + bits.time2str(cfgAll.timeZoneName, tim, 4));
        body.add("MIME-Version: 1.0");
        body.add("Content-Type: multipart/mixed; boundary=" + boundary);
        body.add("X-Mailer: " + version.usrAgnt);
        body.add("Message-ID: <" + tim + "@" + cfgAll.hostName + ">");
        body.add("");
        body.add("this is a message in mime format!");
        body.add("");
    }

    /**
     * write whole body
     *
     * @param txt
     */
    public void putBody(List<String> txt) {
        body.clear();
        body.addAll(txt);
    }

    /**
     * write text
     *
     * @param txt lines to write
     */
    public void putText(List<String> txt) {
        body.add("--" + boundary);
        body.add("Content-Type: text/plain; charset=us-ascii");
        body.add("Content-Transfer-Encoding: 7bit");
        body.add("");
        body.addAll(txt);
    }

    /**
     * write message
     *
     * @param txt lines of message
     */
    public void putMsg(List<String> txt) {
        body.add("--" + boundary);
        body.add("Content-Type: message/rfc822");
        body.add("Content-Transfer-Encoding: 7bit");
        body.add("");
        body.addAll(txt);
    }

    /**
     * put file as attachment
     *
     * @param buf bytes to put
     * @param src file name
     */
    public void putFile(byte[] buf, String src) {
        src = new File(src).getName();
        body.add("--" + boundary);
        body.add("Content-Type: application/octet-stream; name=" + src);
        body.add("Content-Transfer-Encoding: base64");
        body.add("Content-Disposition: attachment; filename=" + src);
        body.add("");
        int pos = 0;
        for (;;) {
            int i = buf.length - pos;
            if (i < 1) {
                break;
            }
            if (i > cryBase64.maxIn) {
                i = cryBase64.maxIn;
            }
            body.add(cryBase64.encodeBytes(buf, pos, i));
            pos += i;
        }
    }

    /**
     * put file as attachment
     *
     * @param src file name
     */
    public void putFile(String src) {
        byte[] buf = null;
        try {
            RandomAccessFile fr = new RandomAccessFile(src, "r");
            long siz = fr.length();
            buf = new byte[(int) siz];
            fr.read(buf, 0, buf.length);
            fr.close();
        } catch (Exception e) {
            return;
        }
        putFile(buf, src);
    }

    /**
     * add last boundary
     */
    public void putFinish() {
        body.add("--" + boundary + "--");
    }

    /**
     * create error message
     *
     * @return false on success, true on error
     */
    public boolean conv2err() {
        String of = from;
        from = "";
        rcpt.clear();
        rcpt.add(of);
        List<String> ob = new ArrayList<String>();
        ob.addAll(body);
        body.clear();
        putHead("error@" + cfgAll.hostName, of, "failure notice");
        List<String> l = new ArrayList<String>();
        l.add("hi " + of + "!");
        l.add("");
        l.add("this message was automatically generated at");
        l.add(cfgAll.hostName + " because the attached mail was not");
        l.add("delivered to the recipients. sorry for it!");
        l.add("");
        l.add("this is the protocol state:");
        l.add("last stage: " + lastS);
        l.add("last transmitted: " + lastT);
        l.add("last received: " + lastR);
        l.add("");
        l.add("this is the original header:");
        for (int i = 0; i < ob.size(); i++) {
            String s = ob.get(i);
            if (s.length() < 1) {
                break;
            }
            l.add(s);
        }
        l.add("");
        l.add("have a nice day!");
        putText(l);
        putMsg(ob);
        putFinish();
        return of.length() < 1;
    }

    /**
     * do sending work
     *
     * @return false on success, true on error
     */
    public boolean doSend() {
        lastS = "no recipients configured";
        if (rcpt.size() < 1) {
            return false;
        }
        lastS = "no suitable server found";
        String serv = cfgAll.mailServerName;
        if (serv == null) {
            uniResLoc url = new uniResLoc();
            url.fromString("smtp://" + rcpt.get(0));
            clntDns clnt = new clntDns();
            clnt.doResolvList(cfgAll.nameServerAddr, url.server, packDnsRec.typeMX);
            serv = clnt.getMX();
        }
        if (serv == null) {
            return true;
        }
        lastS = "failed to open connection to " + serv;
        pipe = new userTerminal(cons).resolvAndConn(servGeneric.protoTcp, serv, new servSmtp().srvPort(), "smtp");
        if (pipe == null) {
            return true;
        }
        pipe.lineRx = pipeSide.modTyp.modeCRtryLF;
        pipe.lineTx = pipeSide.modTyp.modeCRLF;
        cons.debugStat("logging in");
        lastS = "failed to receive greeting message";
        if (getRes(100) != 2) {
            return true;
        }
        lastS = "failed to exchange hostname";
        sendLine("helo " + cfgAll.hostName);
        if (getRes(100) != 2) {
            return true;
        }
        if (cfgAll.mailServerUser != null) {
            lastS = "failed to start authentication";
            byte[] zero = new byte[1];
            zero[0] = 0;
            byte[] buf = bits.byteConcat(zero, cfgAll.mailServerUser.getBytes());
            buf = bits.byteConcat(buf, zero);
            buf = bits.byteConcat(buf, cfgAll.mailServerPass.getBytes());
            sendLine("auth plain");
            if (getRes(100) != 3) {
                return true;
            }
            lastS = "failed to finish authentication";
            sendLine(cryBase64.encodeBytes(buf));
            if (getRes(100) != 2) {
                return true;
            }
        }
        lastS = "failed to start authentication";
        sendLine("mail from:<" + from + ">");
        if (getRes(100) != 2) {
            return true;
        }
        lastS = "failed to set recipients";
        cons.debugStat("sending recipients");
        boolean any = false;
        for (int i = 0; i < rcpt.size(); i++) {
            sendLine("rcpt to:<" + rcpt.get(i) + ">");
            any |= getRes(100) == 2;
        }
        if (!any) {
            cons.debugRes("no recipients accepted");
            return true;
        }
        sendLine("data");
        if (getRes(100) != 3) {
            return true;
        }
        lastS = "failed to start lines";
        cons.setMax(body.size());
        cons.debugStat("sending " + cons.getMax() + " lines");
        for (int i = 0; i < body.size(); i++) {
            packText t = new packText(pipe);
            t.dottedSend(body.get(i));
            cons.setCurr(i);
        }
        cons.debugStat(body.size() + " lines done");
        lastS = "failed to finish lines";
        sendLine(".");
        if (getRes(100) != 2) {
            return true;
        }
        lastS = "failed to log out";
        cons.debugRes("remote accepted");
        sendLine("QUIT");
        getLine();
        pipe.setClose();
        return false;
    }

    /**
     * do sending work
     *
     * @param retry number of tries
     * @return false on success, true on error
     */
    public boolean doSend(int retry) {
        for (; retry > 0; retry--) {
            if (!doSend()) {
                return false;
            }
            logger.warn("error sending email from " + from + ", result=" + lastS);
            bits.sleep(bits.random(3, 10) * 1000);
        }
        return true;
    }

    /**
     * do background sending work
     */
    public void startSend() {
        new Thread(this).start();
    }

    public void run() {
        if (debugger.clntSmtpTraf) {
            logger.debug("sending from " + from);
        }
        try {
            boolean b = doSend(5);
            cleanUp();
            if (!b) {
                return;
            }
            logger.error("giving up email from " + from);
            if (conv2err()) {
                return;
            }
            b = doSend(5);
            cleanUp();
            if (!b) {
                return;
            }
            logger.error("giving up error email");
        } catch (Exception e) {
            logger.traceback(e);
        }
        cleanUp();
    }

    /**
     * upload one file
     *
     * @param trg source
     * @param src target
     * @return result code
     */
    public boolean upload(uniResLoc trg, File src) {
        cons.debugStat("encoding " + src + " to body");
        rcpt.add(trg.toEmail());
        putHead(cfgAll.hostName, trg.toEmail(), "" + src);
        putText(bits.str2lst("this is your file!"));
        putFile("" + src);
        putFinish();
        return doSend();
    }

}
