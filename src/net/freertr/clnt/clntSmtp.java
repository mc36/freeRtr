package net.freertr.clnt;

import java.io.File;
import java.io.RandomAccessFile;
import java.util.ArrayList;
import java.util.List;
import net.freertr.cfg.cfgAll;
import net.freertr.cry.cryBase64;
import net.freertr.pack.packDnsRec;
import net.freertr.pack.packText;
import net.freertr.pipe.pipeDiscard;
import net.freertr.pipe.pipeProgress;
import net.freertr.pipe.pipeSide;
import net.freertr.serv.servGeneric;
import net.freertr.serv.servSmtp;
import net.freertr.user.userTerminal;
import net.freertr.util.bits;
import net.freertr.util.debugger;
import net.freertr.util.logger;
import net.freertr.util.uniResLoc;
import net.freertr.util.version;

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

    /**
     * target
     */
    public String rcpt = "";

    /**
     * envelope id
     */
    public String envid = "";

    /**
     * delivery notification
     */
    public boolean notify = false;

    private String serv;

    private String lastR;

    private String lastT;

    private List<String> errors = new ArrayList<String>();

    private List<String> body = new ArrayList<String>();

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
     * prepend header
     *
     * @param msg message
     * @param hdr headers
     */
    public static void prependHead(List<String> msg, List<String> hdr) {
        for (int i = 0; i < hdr.size(); i++) {
            msg.add(i, hdr.get(i));
        }
    }

    /**
     * prepend header
     *
     * @param msg message
     * @param hdr headers
     */
    public static void deleteHead(List<String> msg, List<String> hdr) {
        for (int o = 0; o < msg.size(); o++) {
            String a = msg.get(o);
            if (a.length() < 1) {
                break;
            }
            int i = a.indexOf(":");
            if (i < 0) {
                i = a.length();
            }
            a = a.substring(0, i).toLowerCase();
            if (hdr.indexOf(a) < 0) {
                continue;
            }
            msg.remove(o);
            o--;
        }
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
        body.add("Message-ID: <" + tim + "@" + cfgAll.getFqdn() + ">");
        body.add("");
        body.add("this is a message in mime format!");
        body.add("");
    }

    /**
     * write whole body
     *
     * @param txt text
     */
    public void putBody(List<String> txt) {
        body.clear();
        body = txt;
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
     * write text
     *
     * @param txt lines to write
     */
    public void putRep(List<String> txt) {
        body.add("--" + boundary);
        body.add("Content-Type: message/delivery-status; charset=us-ascii");
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
    public boolean conv2rep() {
        String of = from;
        String ot = rcpt;
        String oe = envid;
        from = "";
        rcpt = of;
        envid = "";
        notify = false;
        List<String> ob = body;
        body = new ArrayList<String>();
        putHead("report@" + cfgAll.getFqdn(), of, "delivery notification");
        List<String> l = new ArrayList<String>();
        l.add("hi " + of + "!");
        l.add("");
        l.add("this message was automatically generated at");
        l.add(cfgAll.getFqdn() + " because your message was delivered");
        l.add("to " + ot + ".");
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
        l = new ArrayList<String>();
        l.add("Reporting-MTA: dns; " + cfgAll.getFqdn());
        l.add("Original-Envelope-ID: " + oe);
        l.add("");
        l.add("Action: delivered");
        l.add("Status: 2.0.0");
        putRep(l);
        putFinish();
        return of.length() < 1;
    }

    /**
     * create error message
     *
     * @return false on success, true on error
     */
    public boolean conv2err() {
        String of = from;
        String ot = rcpt;
        from = "";
        rcpt = of;
        envid = "";
        notify = false;
        List<String> ob = body;
        body = new ArrayList<String>();
        putHead("error@" + cfgAll.getFqdn(), of, "failure notice");
        List<String> l = new ArrayList<String>();
        l.add("hi " + of + "!");
        l.add("");
        l.add("this message was automatically generated at");
        l.add(cfgAll.getFqdn() + " because the attached mail was not");
        l.add("delivered to " + ot + ".");
        l.add("");
        l.add("this is what happened:");
        for (int i = 0; i < errors.size(); i++) {
            l.add(errors.get(i));
        }
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
     * @return null on success, error string otherwise
     */
    public String doSend() {
        serv = "";
        lastR = "";
        lastT = "";
        if (rcpt.length() < 1) {
            return "no recipients configured";
        }
        serv = cfgAll.mailServerName;
        if (serv == null) {
            uniResLoc url = new uniResLoc();
            url.fromString("smtp://" + rcpt);
            clntDns clnt = new clntDns();
            clnt.doResolvList(cfgAll.nameServerAddr, url.server, true, packDnsRec.typeMX);
            serv = clnt.getMX();
        }
        if (serv == null) {
            serv = "";
            return "no suitable server found";
        }
        pipe = new userTerminal(cons).resolvAndConn(servGeneric.protoTcp, serv, new servSmtp().srvPort(), "smtp");
        if (pipe == null) {
            return "failed to open connection";
        }
        pipe.lineRx = pipeSide.modTyp.modeCRtryLF;
        pipe.lineTx = pipeSide.modTyp.modeCRLF;
        cons.debugStat("logging in");
        if (getRes(100) != 2) {
            return "failed to receive greeting message";
        }
        sendLine("HELO " + cfgAll.getFqdn());
        if (getRes(100) != 2) {
            return "failed to exchange hostname";
        }
        if (cfgAll.mailServerUser != null) {
            byte[] zero = new byte[1];
            zero[0] = 0;
            byte[] buf = bits.byteConcat(zero, cfgAll.mailServerUser.getBytes());
            buf = bits.byteConcat(buf, zero);
            buf = bits.byteConcat(buf, cfgAll.mailServerPass.getBytes());
            sendLine("AUTH PLAIN");
            if (getRes(100) != 3) {
                return "failed to start authentication";
            }
            sendLine(cryBase64.encodeBytes(buf));
            if (getRes(100) != 2) {
                return "failed to finish authentication";
            }
        }
        String a = "";
        if (notify) {
            a += " RET=HDRS ENVID=" + envid;
        }
        sendLine("MAIL FROM:<" + from + ">" + a);
        if (getRes(100) != 2) {
            return "failed to set sender";
        }
        cons.debugStat("sending recipients");
        a = "";
        if (notify) {
            a += " NOTIFY=SUCCESS,FAILURE,DELAY";
        }
        sendLine("RCPT TO:<" + rcpt + ">" + a);
        if (getRes(100) != 2) {
            return "failed to set recipients";
        }
        sendLine("DATA");
        if (getRes(100) != 3) {
            return "failed to start transfer";
        }
        cons.setMax(body.size());
        cons.debugStat("sending " + cons.getMax() + " lines");
        for (int i = 0; i < body.size(); i++) {
            packText t = new packText(pipe);
            t.dottedSend(body.get(i));
            cons.setCurr(i);
        }
        cons.debugStat(body.size() + " lines done");
        sendLine(".");
        if (getRes(100) != 2) {
            return "failed to finish transfer";
        }
        sendLine("QUIT");
        getLine();
        pipe.setClose();
        return null;
    }

    /**
     * do sending work
     *
     * @param retry number of tries
     * @return false on success, true on error
     */
    public boolean doSend(int retry) {
        for (; retry > 0; retry--) {
            String a = doSend();
            if (a == null) {
                return false;
            }
            errors.add(bits.time2str(cfgAll.timeZoneName, bits.getTime() + cfgAll.timeServerOffset, 3) + " remote=" + serv + " issue=" + a + " sent=" + lastT + " received=" + lastR);
            logger.warn("error sending email from " + from + " to " + rcpt);
            bits.sleep(bits.random(60 * 1000, 600 * 1000));
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
            logger.debug("sending from " + from + " to " + rcpt);
        }
        try {
            boolean b = doSend(10);
            cleanUp();
            if (!b) {
                if (!notify) {
                    return;
                }
                if (conv2rep()) {
                    return;
                }
                b = doSend(5);
                cleanUp();
                if (!b) {
                    return;
                }
                logger.error("giving up report email from " + from + " to " + rcpt);
                return;
            }
            logger.error("giving up email from " + from + " to " + rcpt);
            if (conv2err()) {
                return;
            }
            b = doSend(5);
            cleanUp();
            if (!b) {
                return;
            }
            logger.error("giving up error email from " + from + " to " + rcpt);
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
    public String upload(uniResLoc trg, File src) {
        cons.debugStat("encoding " + src + " to body");
        rcpt = trg.toEmail();
        putHead("file@" + cfgAll.getFqdn(), trg.toEmail(), "" + src);
        putText(bits.str2lst("this is your file!"));
        putFile("" + src);
        putFinish();
        return doSend();
    }

}
