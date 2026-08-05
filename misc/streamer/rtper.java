
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
 * rtp helpers
 *
 * @author matecsaba
 */
public class rtper {

    private final ByteBuffer buffer = ByteBuffer.allocate(4096);

    private DatagramChannel target;

    private DatagramChannel source;

    private int src;

    private int seq;

    private int clk;

    private rtper() {
    }

    public static rtper sender(String grp, String prt) throws Exception {
        rtper r = new rtper();
        InetAddress group = InetAddress.getByName(grp);
        int port = Integer.parseInt(prt);
        r.target = DatagramChannel.open();
        DatagramSocket scket = r.target.socket();
        scket.bind(new InetSocketAddress(port));
        MulticastSocket mcast = (MulticastSocket) scket;
        mcast.connect(group, port);
        mcast.setTimeToLive(255);
        r.src = new Random().nextInt();
        r.seq = 0;
        r.clk = 0;
        return r;
    }

    public void write(byte[] buf, int len) throws Exception {
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

    private static void putMsb(ByteBuffer buf, int ofs, int val) {
        buf.put(ofs + 0, (byte) (val >>> 24));
        buf.put(ofs + 1, (byte) (val >>> 16));
        buf.put(ofs + 2, (byte) (val >>> 8));
        buf.put(ofs + 3, (byte) val);
    }

    public int read(byte[] buf) throws Exception {
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

    public static rtper receive(String src, String prt) throws Exception {
        rtper r = new rtper();
        InetAddress addr = InetAddress.getByName(src);
        int port = Integer.parseInt(prt);
        r.source = DatagramChannel.open();
        r.source.socket().bind(new InetSocketAddress(addr, port));
        return r;
    }

    public static byte[] genSdp(String grp, String src, String prt) {
        List<String> res = new ArrayList<String>();
        res.add("v=0");
        res.add("o=Node 0 0 IN IP4 " + src);
        res.add("s=None");
        res.add("c=IN IP4 " + grp);
        res.add("t=0 0");
        res.add("m=audio " + prt + " RTP/AVP " + devicer.rtpt);
        res.add("a=rtpmap:" + devicer.rtpt + " L" + (devicer.smpb * 8) + "/" + devicer.rate + "/2");
        res.add("a=source-filter: incl IN IP4 " + grp + " " + src);
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

    public void announce(byte[] buf, int len, String src, String id) throws Exception {
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

    public static rtper receive(String grp, String src, String prt) throws Exception {
        rtper r = new rtper();
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

}
