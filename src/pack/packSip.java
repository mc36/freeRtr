package pack;

import java.util.ArrayList;
import java.util.List;
import pipe.pipeSide;
import util.bits;
import util.cmds;
import util.logger;
import util.version;
import addr.addrIP;
import cfg.cfgAll;
import clnt.clntHttp;
import snd.sndCodec;
import util.uniResLoc;

/**
 * session initiation protocol (rfc3261) packet
 *
 * @author matecsaba
 */
public class packSip {

    /**
     * port number
     */
    public final static int port = 5060;

    /**
     * update tag
     *
     * @param s (un)tagged
     * @return tagged
     */
    public static String updateTag(String s) {
        if (s.indexOf(";tag=") >= 0) {
            return "" + s;
        } else {
            return s + ";tag=" + bits.randomD() + "-" + cfgAll.hostName;
        }
    }

    /**
     * remove tag
     *
     * @param s (un)tagged
     * @return tagged
     */
    public static String removeTag(String s) {
        int i = s.indexOf(";tag=");
        if (i < 0) {
            return s;
        } else {
            return s.substring(0, i).trim();
        }
    }

    /**
     * update via header
     *
     * @param s header
     * @param n new address
     * @return updated header
     */
    public static String updateVia(String s, String n) {
        int i = s.indexOf(";");
        if (i < 0) {
            return s;
        } else {
            return n + ";" + s.substring(i + 1, s.length()).trim();
        }
    }

    private final pipeSide pipe;

    /**
     * received command
     */
    public String command;

    /**
     * header lines
     */
    public List<String> header;

    /**
     * content lines
     */
    public List<String> content;

    /**
     * create packet handler
     *
     * @param lower lower layer to use
     */
    public packSip(pipeSide lower) {
        pipe = lower;
        clear();
    }

    /**
     * copy packet bytes
     *
     * @param conn new pipe, null if copy
     * @return copy of this packet
     */
    public packSip byteCopy(pipeSide conn) {
        packSip trg;
        if (conn != null) {
            trg = new packSip(conn);
        } else {
            trg = new packSip(pipe);
        }
        trg.command = "" + command;
        trg.header = new ArrayList<String>();
        for (int i = 0; i < header.size(); i++) {
            trg.header.add(header.get(i));
        }
        trg.content = new ArrayList<String>();
        for (int i = 0; i < content.size(); i++) {
            trg.content.add(content.get(i));
        }
        return trg;
    }

    /**
     * clear packet data
     */
    public void clear() {
        command = "";
        header = new ArrayList<String>();
        content = new ArrayList<String>();
    }

    /**
     * bytes ready to receive
     *
     * @return bytes
     */
    public int ready2rx() {
        return pipe.ready2rx();
    }

    /**
     * check if closed
     *
     * @return status
     */
    public int isClosed() {
        return pipe.isClosed();
    }

    /**
     * read up one packet
     *
     * @return false on success, true on error
     */
    public boolean readUp() {
        clear();
        for (;;) {
            if (pipe.isClosed() != 0) {
                return true;
            }
            String a = pipe.lineGet(1);
            if (a.length() < 1) {
                break;
            }
            header.add(a);
        }
        if (header.size() > 0) {
            command = header.remove(0);
        }
        headerUncompact("a", "Accept-Contact");
        headerUncompact("u", "Allow-Events");
        headerUncompact("i", "Call-ID");
        headerUncompact("m", "Contact");
        headerUncompact("e", "Content-Encoding");
        headerUncompact("l", "Content-Length");
        headerUncompact("c", "Content-Type");
        headerUncompact("o", "Event");
        headerUncompact("f", "From");
        headerUncompact("y", "Identity");
        headerUncompact("r", "Refer-To");
        headerUncompact("b", "Referred-By");
        headerUncompact("j", "Reject-Contact");
        headerUncompact("d", "Request-Disposition");
        headerUncompact("x", "Session-Expires");
        headerUncompact("s", "Subject");
        headerUncompact("k", "Supported");
        headerUncompact("t", "To");
        headerUncompact("v", "Via");
        int i = bits.str2num(headerGet("Content-Length", 1));
        byte[] buf = new byte[i];
        pipe.moreGet(buf, 0, buf.length);
        String a = "";
        for (i = 0; i < buf.length; i++) {
            int o = buf[i] & 0xff;
            switch (pipeSide.getType(o, pipeSide.modTyp.modeNone)) {
                case modeCR:
                case modeLF:
                    a = a.trim();
                    if (a.length() > 0) {
                        content.add(a);
                    }
                    a = "";
                    break;
                default:
                    byte[] chr = new byte[1];
                    chr[0] = (byte) o;
                    a += new String(chr);
                    break;
            }
        }
        a = a.trim();
        if (a.length() > 0) {
            content.add(a);
        }
        return false;
    }

    /**
     * write current packet
     */
    public void writeDown() {
        byte[] buf2 = pipeSide.getEnding(pipeSide.modTyp.modeCRLF);
        for (int i = 0; i < content.size(); i++) {
            buf2 = bits.byteConcat(buf2, content.get(i).getBytes());
            buf2 = bits.byteConcat(buf2, pipeSide.getEnding(pipeSide.modTyp.modeCRLF));
        }
        for (;;) {
            int i = headerFind("Content-Length", 1);
            if (i < 0) {
                break;
            }
            header.remove(i);
        }
        header.add("Content-Length: " + (buf2.length - 2));
        byte[] buf1 = bits.byteConcat(command.getBytes(), pipeSide.getEnding(pipeSide.modTyp.modeCRLF));
        for (int i = 0; i < header.size(); i++) {
            buf1 = bits.byteConcat(buf1, header.get(i).getBytes());
            buf1 = bits.byteConcat(buf1, pipeSide.getEnding(pipeSide.modTyp.modeCRLF));
        }
        buf1 = bits.byteConcat(buf1, buf2);
        int o = pipe.ready2tx();
        pipe.morePut(buf1, 0, buf1.length);
        for (int i = 0; i < 10; i++) {
            if (o == pipe.ready2tx()) {
                break;
            }
            bits.sleep(100);
        }
    }

    /**
     * write keepalive packet
     */
    public void writeKeep() {
        byte[] buf = pipeSide.getEnding(pipeSide.modTyp.modeCRLF);
        pipe.morePut(buf, 0, buf.length);
    }

    private void headerUncompact(String src, String trg) {
        for (int i = 0; i < header.size(); i++) {
            String a = header.get(i);
            String s = "";
            int o = a.indexOf(":");
            if (o >= 0) {
                s = a.substring(o + 1, a.length());
                a = a.substring(0, o);
            }
            if (!a.trim().toLowerCase().equals(src)) {
                continue;
            }
            header.set(i, trg + ": " + s.trim());
        }
    }

    /**
     * find header value
     *
     * @param ned needed header
     * @param num number of occurance
     * @return number of line, -1 if not found
     */
    public int headerFind(String ned, int num) {
        ned = ned.trim().toLowerCase();
        for (int i = 0; i < header.size(); i++) {
            String a = header.get(i);
            int o = a.indexOf(":");
            if (o >= 0) {
                a = a.substring(0, o);
            }
            a = a.trim().toLowerCase();
            if (!a.equals(ned)) {
                continue;
            }
            num -= 1;
            if (num > 0) {
                continue;
            }
            return i;
        }
        return -1;
    }

    /**
     * find content value
     *
     * @param ned needed header
     * @param num number of occurance
     * @return number of line, -1 if not found
     */
    public int contentFind(String ned, int num) {
        ned = ned.trim().toLowerCase();
        for (int i = 0; i < content.size(); i++) {
            String a = content.get(i);
            if (!a.startsWith(ned)) {
                continue;
            }
            num -= 1;
            if (num > 0) {
                continue;
            }
            return i;
        }
        return -1;
    }

    /**
     * get header value
     *
     * @param ned header type
     * @param num number of occurance
     * @return value of header, "" if not found
     */
    public String headerGet(String ned, int num) {
        num = headerFind(ned, num);
        if (num < 0) {
            return "";
        }
        ned = header.get(num);
        num = ned.indexOf(":");
        if (num < 0) {
            return "";
        }
        return ned.substring(num + 1, ned.length()).trim();
    }

    /**
     * set header value
     *
     * @param ned header type
     * @param num number of occurance
     * @param val value to set
     * @return true on error, false on success
     */
    public boolean headerSet(String ned, int num, String val) {
        num = headerFind(ned, num);
        if (num < 0) {
            return true;
        }
        ned = header.get(num);
        int idx = ned.indexOf(":");
        if (idx < 0) {
            return true;
        }
        header.set(num, ned.substring(0, idx) + ": " + val);
        return false;
    }

    /**
     * DEL header value
     *
     * @param ned header type
     * @param num number of occurance
     * @return true on error, false on success
     */
    public boolean headerDel(String ned, int num) {
        num = headerFind(ned, num);
        if (num < 0) {
            return true;
        }
        header.remove(num);
        return false;
    }

    /**
     * get content value
     *
     * @param ned header type
     * @param num number of occurance
     * @return value of header, "" if not found
     */
    public String contentGet(String ned, int num) {
        num = contentFind(ned, num);
        if (num < 0) {
            return "";
        }
        String a = content.get(num);
        return a.substring(ned.length(), a.length()).trim();
    }

    /**
     * dump the message
     *
     * @param dir direction
     */
    public void dump(String dir) {
        logger.debug(dir + ": " + command);
        for (int i = 0; i < header.size(); i++) {
            logger.debug(header.get(i));
        }
        for (int i = 0; i < content.size(); i++) {
            logger.debug(content.get(i));
        }
    }

    /**
     * copy header field
     *
     * @param src source packet
     * @param hdr header field
     */
    public void copyHeader(packSip src, String hdr) {
        for (int i = 1;; i++) {
            String a = src.headerGet(hdr, i);
            if (a.length() < 1) {
                break;
            }
            header.add(hdr + ": " + a);
        }
    }

    /**
     * create sdp payload
     *
     * @param addr local address
     * @param port local port
     * @param alaw true=aLaw, false=uLaw
     */
    public void makeSdp(addrIP addr, int port, sndCodec alaw) {
        header.add("Content-Type: application/sdp");
        content.add("v=0");
        int i;
        if (addr.isIPv4()) {
            i = 4;
        } else {
            i = 6;
        }
        content.add("o=user 0 0 IN IP" + i + " " + addr);
        content.add("s=call");
        content.add("c=IN IP" + i + " " + addr);
        content.add("t=0 0");
        content.add("m=audio " + port + " RTP/AVP " + alaw.getRTPtype());
        content.add("a=rtpmap:" + alaw.getRTPtype() + " " + alaw.getRTPname() + "/8000");
    }

    /**
     * get sdp media endpoint
     *
     * @param addr address to set
     * @return port number, -1 if error
     */
    public int sdpGetMediaEP(addrIP addr) {
        String s = contentGet("c=", 1);
        if (s.length() < 1) {
            return -1;
        }
        cmds c = new cmds("sdp", s);
        c.word();
        c.word();
        if (addr.fromString(c.word())) {
            return -1;
        }
        s = contentGet("m=", 1);
        if (s.length() < 1) {
            return -1;
        }
        c = new cmds("sdp", s);
        c.word();
        return bits.str2num(c.word());
    }

    /**
     * make trying packet
     *
     * @param cmd command to report
     * @param src source to reply to
     * @param cntc contact, null=nothing
     */
    public void makeNumeric(String cmd, packSip src, String cntc) {
        clear();
        command = "SIP/2.0 " + cmd;
        copyHeader(src, "Via");
        copyHeader(src, "From");
        copyHeader(src, "To");
        copyHeader(src, "Call-ID");
        copyHeader(src, "CSeq");
        if (cntc != null) {
            header.add("Contact: " + cntc);
        }
        header.add("Max-Forwards: 70");
        header.add("Server: " + version.usrAgnt);
        header.add("Allow: INVITE, MESSAGE, NOTIFY, ACK, BYE, CANCEL");
    }

    /**
     * make ok packet
     *
     * @param src source to reply to
     * @param cntc contact, null=nothing
     * @param expr expires, 0=nothing
     */
    public void makeOk(packSip src, String cntc, int expr) {
        clear();
        command = "SIP/2.0 200 ok";
        copyHeader(src, "Via");
        copyHeader(src, "From");
        header.add("To: " + updateTag(src.headerGet("To", 1)));
        copyHeader(src, "Call-ID");
        copyHeader(src, "CSeq");
        if (cntc != null) {
            header.add("Contact: " + cntc);
        }
        if (expr > 0) {
            header.add("Expires: " + expr);
        }
        header.add("Max-Forwards: 70");
        header.add("Server: " + version.usrAgnt);
        header.add("Allow: INVITE, MESSAGE, NOTIFY, ACK, BYE, CANCEL");
    }

    /**
     * make bad extension packet
     *
     * @param src source to reply to
     * @param cntc contact, null=nothing
     * @param err error text
     */
    public void makeErr(packSip src, String cntc, String err) {
        clear();
        command = "SIP/2.0 420 " + err;
        copyHeader(src, "Via");
        copyHeader(src, "From");
        copyHeader(src, "To");
        copyHeader(src, "Call-ID");
        copyHeader(src, "CSeq");
        if (cntc != null) {
            header.add("Contact: " + cntc);
        }
        header.add("Max-Forwards: 70");
        header.add("Server: " + version.usrAgnt);
        header.add("Allow: INVITE, MESSAGE, NOTIFY, ACK, BYE, CANCEL");
    }

    /**
     * make request
     *
     * @param cmd command to do
     * @param url url, null=derive from trg
     * @param src source
     * @param trg target
     * @param cntc contact, null=nothing
     * @param via via, null=nothing
     * @param cid call id, null=nothing
     * @param seq sequence
     * @param expr expires, 0=nothing
     */
    public void makeReq(String cmd, String url, String src, String trg, String cntc, String via, String cid, int seq, int expr) {
        clear();
        if (url == null) {
            url = uniResLoc.fromEmail(trg);
        }
        command = cmd + " " + url + " SIP/2.0";
        header.add("Max-Forwards: 70");
        header.add("User-Agent: " + version.usrAgnt);
        header.add("To: " + trg);
        header.add("From: " + src);
        if (cid != null) {
            header.add("Call-ID: " + cid);
        }
        header.add("CSeq: " + bits.num2str(seq) + " " + cmd);
        header.add("Allow: INVITE, MESSAGE, NOTIFY, ACK, BYE, CANCEL");
        if (cntc != null) {
            header.add("Contact: " + cntc);
        }
        if (via != null) {
            header.add("Via: " + via);
        }
        if (expr > 0) {
            header.add("Expires: " + expr);
        }
    }

    /**
     * add author line
     *
     * @param pre prefix of reply
     * @param got got callenge, null if nothing
     * @param usr username
     * @param pwd password
     */
    public void addAuthor(String pre, String got, String usr, String pwd) {
        if (got == null) {
            return;
        }
        got = got.trim();
        int i = got.indexOf(" ");
        if (i < 0) {
            return;
        }
        got = got.substring(i + 1, got.length()).trim();
        i = command.indexOf(" ");
        int o = command.lastIndexOf(" ");
        if (i < 0) {
            return;
        }
        if (o <= i) {
            return;
        }
        got = clntHttp.getAuthor(got, command.substring(0, i).trim(), command.substring(i + 1, o).trim(), usr, pwd);
        if (got == null) {
            return;
        }
        header.add(pre + got);
    }

}
