
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

    private packer() {
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
        res.add("m=audio " + prt + " RTP/AVP " + devicer.rtpt);
        res.add("a=rtpmap:" + devicer.rtpt + " L" + (devicer.smpb * 8) + "/" + devicer.rate + "/2");
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

    /**
     * write rtp data
     *
     * @param buf msb bytes
     * @param len length
     * @throws Exception on error
     */
    public void writeRtp(byte[] buf, int len) throws Exception {
        buffer.clear();
        putMsb(buffer, 0, 0x80000000 | (devicer.rtpt << 16) | seq);
        putMsb(buffer, 4, clk);
        putMsb(buffer, 8, src);
        buffer.put(devicer.rtpl, buf, 0, len);
        buffer.position(0);
        buffer.limit(len + devicer.rtpl);
        target.write(buffer);
        seq++;
        seq &= 0xffff;
        clk += len / (2 * devicer.smpb);
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
            len = buffer.position() - devicer.rtpl;
            if (len < devicer.rtpl) {
                break;
            }
            if ((buffer.get(1) & 0xff) == devicer.rtpt) {
                break;
            }
        }
        buffer.get(devicer.rtpl, buf, 0, len);
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
        buffer.put(0, (byte) devicer.scrb);
        buffer.put(1, (byte) (devicer.smpb * 8));
        buffer.put(2, (byte) 2);
        buffer.put(3, (byte) devicer.scrt);
        buffer.put(4, (byte) 0);
        buffer.put(devicer.scrl, buf, 0, len);
        buffer.position(0);
        buffer.limit(len + devicer.scrl);
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
            len = buffer.position() - devicer.scrl;
            if (len < devicer.scrl) {
                break;
            }
            if ((buffer.get(0) & 0xff) != devicer.scrb) {
                continue;
            }
            if ((buffer.get(1) & 0xff) != (devicer.smpb * 8)) {
                continue;
            }
            if ((buffer.get(3) & 0xff) == devicer.scrt) {
                break;
            }
        }
        buffer.get(devicer.scrl, buf, 0, len);
        coder.byteSwap(buf, len);
        return len;
    }

}
