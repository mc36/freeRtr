package org.freertr.clnt;

import java.io.File;
import java.io.RandomAccessFile;
import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.cfg.cfgAll;
import org.freertr.enc.encBase64;
import org.freertr.pack.packDnsRec;
import org.freertr.pack.packText;
import org.freertr.pipe.pipeDiscard;
import org.freertr.pipe.pipeProgress;
import org.freertr.pipe.pipeSide;
import org.freertr.serv.servGeneric;
import org.freertr.serv.servSmtp;
import org.freertr.enc.encUrl;
import org.freertr.util.bits;
import org.freertr.util.debugger;
import org.freertr.util.logger;
import org.freertr.util.syncInt;

/**
 * simple mail transfer protocol (rfc821) client
 *
 * @author matecsaba
 */
public class clntSmtp implements Runnable {

    /**
     * startup counter
     */
    public final static syncInt cntrStart = new syncInt(0);

    /**
     * error counter
     */
    public final static syncInt cntrError = new syncInt(0);

    /**
     * stop counter
     */
    public final static syncInt cntrStop = new syncInt(0);

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
            int p = -1;
            for (i = 0; i < hdr.size(); i++) {
                if (!a.equals(hdr.get(i))) {
                    continue;
                }
                p = i;
                break;
            }
            if (p < 0) {
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
        body.add("X-Mailer: " + clntHttp.getAgentLine());
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
            if (i > encBase64.maxIn) {
                i = encBase64.maxIn;
            }
            body.add(encBase64.encodeBytes(buf, pos, i));
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
            encUrl url = new encUrl();
            url.fromString("smtp://" + rcpt);
            clntDns clnt = new clntDns();
            clnt.doResolvList(cfgAll.nameServerAddr, url.server, true, packDnsRec.typeMX);
            serv = clnt.getMX();
        }
        if (serv == null) {
            serv = "";
            return "no suitable server found";
        }
        clntProxy prx = cfgAll.getClntPrx(cfgAll.mailProxy);
        if (prx == null) {
            return "no proxy configured";
        }
        addrIP trg = clntDns.justResolv(serv, 0);
        if (trg == null) {
            return "no address found for server";
        }
        pipe = prx.doConnect(servGeneric.protoTcp, trg, new servSmtp().srvPort(), "smtp");
        if (pipe == null) {
            return "failed to open connection";
        }
        pipe.setTime(120000);
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
            sendLine(encBase64.encodeBytes(buf));
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
        for (;;) {
            String a = doSend();
            if (a == null) {
                return false;
            }
            errors.add(logger.getTimestamp() + " remote=" + serv + " issue=" + a + " sent=" + lastT + " received=" + lastR);
            logger.warn("error sending email from " + from + " to " + rcpt);
            retry--;
            if (retry < 1) {
                break;
            }
            bits.sleep(bits.random(60 * 1000, 600 * 1000));
        }
        return true;
    }

    /**
     * do background sending work
     */
    public void startSend() {
        logger.startThread(this);
    }

    public void run() {
        if (debugger.clntSmtpTraf) {
            logger.debug("sending from " + from + " to " + rcpt);
        }
        cntrStart.add(1);
        try {
            boolean b = doSend(10);
            cleanUp();
            if (!b) {
                if (!notify) {
                    cntrStop.add(1);
                    return;
                }
                if (conv2rep()) {
                    cntrStop.add(1);
                    return;
                }
                b = doSend(5);
                cleanUp();
                if (!b) {
                    cntrStop.add(1);
                    return;
                }
                logger.error("giving up report email from " + from + " to " + rcpt);
                cntrError.add(1);
                return;
            }
            cntrError.add(1);
            logger.error("giving up email from " + from + " to " + rcpt);
            if (conv2err()) {
                return;
            }
            cntrStart.add(1);
            b = doSend(5);
            cleanUp();
            if (!b) {
                cntrStop.add(1);
                return;
            }
            logger.error("giving up error email from " + from + " to " + rcpt);
            cntrError.add(1);
        } catch (Exception e) {
            logger.traceback(e);
            cntrError.add(1);
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
    public String upload(encUrl trg, File src) {
        cons.debugStat("encoding " + src + " to body");
        rcpt = trg.toEmail();
        putHead("file@" + cfgAll.getFqdn(), trg.toEmail(), "" + src);
        putText(bits.str2lst("this is your file!"));
        putFile("" + src);
        putFinish();
        return doSend();
    }

}
