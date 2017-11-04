package pack;

import java.util.ArrayList;
import java.util.List;
import pipe.pipeSide;
import util.bits;
import util.cmds;
import util.logger;
import util.version;
import addr.addrIP;
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

    private pipeSide pipe;

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
     * @param src source packet
     */
    public void byteCopy(packSip src) {
        command = "" + src.command;
        header = new ArrayList<String>();
        for (int i = 0; i < src.header.size(); i++) {
            header.add(src.header.get(i));
        }
        content = new ArrayList<String>();
        for (int i = 0; i < src.content.size(); i++) {
            content.add(src.content.get(i));
        }
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
     * read up one packet
     *
     * @return false on success, true on error
     */
    public boolean readUp() {
        clear();
        command = pipe.lineGet(1);
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
        int i = bits.str2num(headerGet("content-length", 1));
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
        int i = pipe.ready2tx();
        pipe.morePut(buf1, 0, buf1.length);
        for (; i != pipe.ready2tx();) {
            bits.sleep(100);
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
    public void makeSdp(addrIP addr, int port, boolean alaw) {
        header.add("content-type: application/sdp");
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
        String a;
        if (alaw) {
            i = 8;
            a = "A";
        } else {
            i = 0;
            a = "U";
        }
        content.add("m=audio " + port + " RTP/AVP " + i);
        content.add("a=rtpmap:" + i + " PCM" + a + "/8000");
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
        copyHeader(src, "via");
        copyHeader(src, "from");
        copyHeader(src, "to");
        copyHeader(src, "call-id");
        copyHeader(src, "cseq");
        if (cntc != null) {
            header.add("contact: " + cntc);
        }
        header.add("max-forwards: 70");
        header.add("server: " + version.usrAgnt);
        header.add("allow: invite, ack, bye, cancel");
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
        copyHeader(src, "via");
        copyHeader(src, "from");
        header.add("to: " + src.headerGet("to", 1) + ";tag=" + bits.randomD());
        copyHeader(src, "call-id");
        copyHeader(src, "cseq");
        if (cntc != null) {
            header.add("contact: " + cntc);
        }
        if (expr > 0) {
            header.add("expires: " + expr);
        }
        header.add("max-forwards: 70");
        header.add("server: " + version.usrAgnt);
        header.add("allow: invite, ack, bye, cancel");
    }

    /**
     * make ringing packet
     *
     * @param src source to reply to
     * @param cntc contact, null=nothing
     */
    public void makeNoExt(packSip src, String cntc) {
        clear();
        command = "SIP/2.0 420 bad extension";
        copyHeader(src, "via");
        copyHeader(src, "from");
        copyHeader(src, "to");
        copyHeader(src, "call-id");
        copyHeader(src, "cseq");
        if (cntc != null) {
            header.add("contact: " + cntc);
        }
        header.add("max-forwards: 70");
        header.add("server: " + version.usrAgnt);
        header.add("allow: invite, ack, bye, cancel");
    }

    /**
     * make request
     *
     * @param cmd command to do
     * @param src source
     * @param trg target
     * @param cntc contact, null=nothing
     * @param cid call id
     * @param seq sequence
     */
    public void makeReq(String cmd, String src, String trg, String cntc, String cid, int seq) {
        clear();
        command = cmd + " " + uniResLoc.fromEmail(trg) + " SIP/2.0";
        header.add("max-forwards: 70");
        header.add("user-agent: " + version.usrAgnt);
        header.add("to: " + trg);
        header.add("from: " + src);
        header.add("call-id: " + cid);
        header.add("cseq: " + seq + " " + cmd);
        if (cntc != null) {
            header.add("contact: " + cntc);
        }
    }

}
