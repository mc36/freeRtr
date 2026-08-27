
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.MulticastSocket;
import java.nio.ByteBuffer;
import java.nio.channels.DatagramChannel;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

/**
 * packet helpers
 *
 * @author matecsaba
 */
public class packer {

    private final ByteBuffer buffer = ByteBuffer.allocate(4096);

    /**
     * the codec
     */
    public final codec coder = codec.getCodec();

    private DatagramChannel target;

    private DatagramChannel source;

    private int src;

    private int seq;

    private int clk;

    /**
     * create instance
     */
    protected packer() {
    }

    /**
     * create sender
     *
     * @param grp group
     * @param prt port
     * @return instance
     * @throws Exception on error
     */
    public static packer sender(String grp, String prt) throws Exception {
        packer r = new packer();
        InetAddress group = InetAddress.getByName(grp);
        int port = Integer.parseInt(prt);
        r.target = DatagramChannel.open();
        DatagramSocket scket = r.target.socket();
        scket.bind(new InetSocketAddress(port));
        MulticastSocket mcast = (MulticastSocket) scket;
        mcast.connect(group, port);
        mcast.setTimeToLive(255);
        mcast.setTrafficClass(46 << 2);
        r.src = new Random().nextInt();
        r.seq = 0;
        r.clk = 0;
        return r;
    }

    /**
     * create receiver
     *
     * @param grp group
     * @param src source
     * @param prt port
     * @return instance
     * @throws Exception on error
     */
    public static packer receiver(String grp, String src, String prt) throws Exception {
        packer r = new packer();
        InetAddress group = InetAddress.getByName(grp);
        InetAddress source = InetAddress.getByName(src);
        int port = Integer.parseInt(prt);
        r.source = DatagramChannel.open();
        DatagramSocket scket = r.source.socket();
        MulticastSocket mcast = (MulticastSocket) scket;
        r.source.socket().bind(new InetSocketAddress(port));
        r.source.join(group, mcast.getNetworkInterface(), source);
        return r;
    }

    /**
     * get kind
     *
     * @param a string
     * @return kind
     * @throws Exception on error
     */
    public packet string2kind(String a) throws Exception {
        if (a == null) {
            return new packetRtp(this);
        }
        if (a.equals("rtp")) {
            return new packetRtp(this);
        }
        if (a.equals("scr")) {
            return new packetScr(this);
        }
        if (a.equals("vba")) {
            return new packetVba(this);
        }
        if (a.equals("wfa")) {
            return new packetWfa(this);
        }
        if (a.equals("udpm")) {
            return new packetUdpMsb(this);
        }
        if (a.equals("udpl")) {
            return new packetUdpLsb(this);
        }
        throw new Exception("unknown kind");
    }

    /**
     * generate sdp payload
     *
     * @param grp group
     * @param src source
     * @param prt port
     * @return sdp payload
     */
    public static byte[] generateSdp(String grp, String src, String prt) {
        List<String> res = new ArrayList<String>();
        res.add("v=0");
        res.add("o=Node 0 0 IN IP4 " + src);
        res.add("s=Noname");
        res.add("c=IN IP4 " + grp);
        res.add("t=0 0");
        res.add("m=audio " + prt + " RTP/AVP " + consts.rtpt);
        res.add("a=rtpmap:" + consts.rtpt + " L" + (consts.smpb * 8) + "/" + consts.rate + "/2");
        res.add("a=source-filter: incl IN IP4 " + grp + " " + src);
        res.add("a=recvonly");
        res.add("a=type:broadcast");
        int o = res.size() * 2;
        for (int i = 0; i < res.size(); i++) {
            o += res.get(i).length();
        }
        byte[] buf = new byte[o];
        o = 0;
        for (int i = 0; i < res.size(); i++) {
            byte[] cur = res.get(i).getBytes();
            System.arraycopy(cur, 0, buf, o, cur.length);
            o += cur.length;
            buf[o + 0] = 13;
            buf[o + 1] = 10;
            o += 2;
        }
        return buf;
    }

    /**
     * send sap announcement
     *
     * @param buf sdp payload
     * @param len sdp length
     * @param src source
     * @param id identification
     * @throws Exception on error
     */
    public void announceSap(byte[] buf, int len, String src, String id) throws Exception {
        byte[] mime = {'a', 'p', 'p', 'l', 'i', 'c', 'a', 't', 'i', 'o', 'n', '/', 's', 'd', 'p', 0};
        byte[] source = InetAddress.getByName(src).getAddress();
        buffer.clear();
        putMsb(buffer, 0, 0x20000000 | Integer.parseInt(id));
        buffer.put(4, source, source.length - 4, source.length);
        buffer.put(8, mime, 0, mime.length);
        buffer.put(8 + mime.length, buf, 0, len);
        buffer.position(0);
        buffer.limit(len + mime.length + 8);
        target.write(buffer);
    }

    private static void putMsb(ByteBuffer buf, int ofs, int val) {
        buf.put(ofs + 0, (byte) (val >>> 24));
        buf.put(ofs + 1, (byte) (val >>> 16));
        buf.put(ofs + 2, (byte) (val >>> 8));
        buf.put(ofs + 3, (byte) val);
    }

    private static int getMsb(ByteBuffer buf, int ofs) {
        int val = buf.get(ofs + 3) & 0xff;
        val |= (buf.get(ofs + 2) & 0xff) << 8;
        val |= (buf.get(ofs + 1) & 0xff) << 16;
        val |= (buf.get(ofs + 0) & 0xff) << 24;
        return val;
    }

    /**
     * write udp data
     *
     * @param buf msb bytes
     * @param len length
     * @throws Exception on error
     */
    public void writeUdpMsb(byte[] buf, int len) throws Exception {
        buffer.clear();
        buffer.put(0, buf, 0, len);
        buffer.position(0);
        buffer.limit(len);
        target.write(buffer);
    }

    /**
     * read udp data
     *
     * @param buf msb bytes
     * @return bytes
     * @throws Exception on error
     */
    public int readUdpMsb(byte[] buf) throws Exception {
        buffer.clear();
        source.receive(buffer);
        int len = buffer.position();
        buffer.get(0, buf, 0, len);
        return len;
    }

    /**
     * write udp data
     *
     * @param buf msb bytes
     * @param len length
     * @throws Exception on error
     */
    public void writeUdpLsb(byte[] buf, int len) throws Exception {
        coder.byteSwap(buf, len);
        buffer.clear();
        buffer.put(0, buf, 0, len);
        buffer.position(0);
        buffer.limit(len);
        target.write(buffer);
    }

    /**
     * read udp data
     *
     * @param buf msb bytes
     * @return bytes
     * @throws Exception on error
     */
    public int readUdpLsb(byte[] buf) throws Exception {
        buffer.clear();
        source.receive(buffer);
        int len = buffer.position();
        buffer.get(0, buf, 0, len);
        coder.byteSwap(buf, len);
        return len;
    }

    /**
     * write rtp data
     *
     * @param buf msb bytes
     * @param len length
     * @throws Exception on error
     */
    public void writeRtp(byte[] buf, int len) throws Exception {
        buffer.clear();
        putMsb(buffer, 0, 0x80000000 | (consts.rtpt << 16) | seq);
        putMsb(buffer, 4, clk);
        putMsb(buffer, 8, src);
        buffer.put(consts.rtpl, buf, 0, len);
        buffer.position(0);
        buffer.limit(len + consts.rtpl);
        target.write(buffer);
        seq++;
        seq &= 0xffff;
        clk += len / (2 * consts.smpb);
    }

    /**
     * read rtp data
     *
     * @param buf msb bytes
     * @return bytes
     * @throws Exception on error
     */
    public int readRtp(byte[] buf) throws Exception {
        int len;
        for (;;) {
            buffer.clear();
            source.receive(buffer);
            len = buffer.position() - consts.rtpl;
            if (len < consts.rtpl) {
                break;
            }
            if ((buffer.get(1) & 0xff) == consts.rtpt) {
                break;
            }
        }
        buffer.get(consts.rtpl, buf, 0, len);
        return len;
    }

    /**
     * write scream data
     *
     * @param buf msb bytes
     * @param len length
     * @throws Exception on error
     */
    public void writeScr(byte[] buf, int len) throws Exception {
        coder.byteSwap(buf, len);
        buffer.clear();
        buffer.put(0, (byte) consts.scrb);
        buffer.put(1, (byte) (consts.smpb * 8));
        buffer.put(2, (byte) 2);
        buffer.put(3, (byte) consts.scrt);
        buffer.put(4, (byte) 0);
        buffer.put(consts.scrl, buf, 0, len);
        buffer.position(0);
        buffer.limit(len + consts.scrl);
        target.write(buffer);
    }

    /**
     * read scream data
     *
     * @param buf msb bytes
     * @return bytes
     * @throws Exception on error
     */
    public int readScr(byte[] buf) throws Exception {
        int len;
        for (;;) {
            buffer.clear();
            source.receive(buffer);
            len = buffer.position() - consts.scrl;
            if (len < consts.scrl) {
                break;
            }
            if ((buffer.get(0) & 0xff) != consts.scrb) {
                continue;
            }
            if ((buffer.get(1) & 0xff) != (consts.smpb * 8)) {
                continue;
            }
            if ((buffer.get(3) & 0xff) == consts.scrt) {
                break;
            }
        }
        buffer.get(consts.scrl, buf, 0, len);
        coder.byteSwap(buf, len);
        return len;
    }

    /**
     * write vban data
     *
     * @param buf msb bytes
     * @param len length
     * @throws Exception on error
     */
    public void writeVba(byte[] buf, int len) throws Exception {
        coder.byteSwap(buf, len);
        buffer.clear();
        putMsb(buffer, 0, consts.vbam);
        buffer.put(4, (byte) consts.vbab());
        buffer.put(5, (byte) ((len / (2 * consts.smpb)) - 1));
        buffer.put(6, (byte) 1);
        buffer.put(7, (byte) (consts.smpb - 1));
        putMsb(buffer, 8, 0x6e6f6e65);
        putMsb(buffer, 12, 0);
        putMsb(buffer, 16, 0);
        putMsb(buffer, 20, 0);
        putMsb(buffer, 24, seq);
        buffer.put(consts.vbal, buf, 0, len);
        buffer.position(0);
        buffer.limit(len + consts.vbal);
        target.write(buffer);
        seq++;
    }

    /**
     * read vban data
     *
     * @param buf msb bytes
     * @return bytes
     * @throws Exception on error
     */
    public int readVba(byte[] buf) throws Exception {
        int len;
        for (;;) {
            buffer.clear();
            source.receive(buffer);
            len = buffer.position() - consts.vbal;
            if (len < consts.vbal) {
                break;
            }
            if (getMsb(buffer, 0) != consts.vbam) {
                continue;
            }
            if ((buffer.get(4) & 0xff) != consts.vbab()) {
                continue;
            }
            if ((buffer.get(7) & 0xff) == (consts.smpb - 1)) {
                break;
            }
        }
        buffer.get(consts.vbal, buf, 0, len);
        coder.byteSwap(buf, len);
        return len;
    }

    /**
     * write wfas data
     *
     * @param buf msb bytes
     * @param len length
     * @throws Exception on error
     */
    public void writeWfa(byte[] buf, int len) throws Exception {
        coder.byteSwap(buf, len);
        buffer.clear();
        putMsb(buffer, 0, consts.wfam);
        putMsb(buffer, 2, (consts.wfam << 16) | seq);
        putMsb(buffer, 6, clk);
        buffer.put(consts.wfal, buf, 0, len);
        buffer.position(0);
        buffer.limit(len + consts.wfal);
        target.write(buffer);
        seq++;
        seq &= 0xffff;
        clk += len / (2 * consts.smpb);
    }

    /**
     * read wfas data
     *
     * @param buf msb bytes
     * @return bytes
     * @throws Exception on error
     */
    public int readWfa(byte[] buf) throws Exception {
        int len;
        for (;;) {
            buffer.clear();
            source.receive(buffer);
            len = buffer.position() - consts.wfal;
            if (len < consts.wfal) {
                break;
            }
            if (getMsb(buffer, 0) == consts.wfam) {
                break;
            }
        }
        buffer.get(consts.wfal, buf, 0, len);
        coder.byteSwap(buf, len);
        return len;
    }

}

class packetRtp extends packet {

    public packetRtp(packer p) {
        super(p);
    }

    public int readKind(byte[] buf) throws Exception {
        return pck.readRtp(buf);
    }

    public void writeKind(byte[] buf, int len) throws Exception {
        pck.writeRtp(buf, len);
    }

}

class packetScr extends packet {

    public packetScr(packer p) {
        super(p);
    }

    public int readKind(byte[] buf) throws Exception {
        return pck.readScr(buf);
    }

    public void writeKind(byte[] buf, int len) throws Exception {
        pck.writeScr(buf, len);
    }

}

class packetVba extends packet {

    public packetVba(packer p) {
        super(p);
    }

    public int readKind(byte[] buf) throws Exception {
        return pck.readVba(buf);
    }

    public void writeKind(byte[] buf, int len) throws Exception {
        pck.writeVba(buf, len);
    }

}

class packetWfa extends packet {

    public packetWfa(packer p) {
        super(p);
    }

    public int readKind(byte[] buf) throws Exception {
        return pck.readWfa(buf);
    }

    public void writeKind(byte[] buf, int len) throws Exception {
        pck.writeWfa(buf, len);
    }

}

class packetUdpMsb extends packet {

    public packetUdpMsb(packer p) {
        super(p);
    }

    public int readKind(byte[] buf) throws Exception {
        return pck.readUdpMsb(buf);
    }

    public void writeKind(byte[] buf, int len) throws Exception {
        pck.writeUdpMsb(buf, len);
    }

}

class packetUdpLsb extends packet {

    public packetUdpLsb(packer p) {
        super(p);
    }

    public int readKind(byte[] buf) throws Exception {
        return pck.readUdpLsb(buf);
    }

    public void writeKind(byte[] buf, int len) throws Exception {
        pck.writeUdpLsb(buf, len);
    }

}
