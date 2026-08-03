
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

    private ByteBuffer buffer;

    private DatagramChannel target;

    private int src;

    private int seq;

    private int clk;

    public rtper(String grp, String prt) throws Exception {
        InetAddress group = InetAddress.getByName(grp);
        int port = Integer.parseInt(prt);
        target = DatagramChannel.open();
        DatagramSocket scket = target.socket();
        scket.bind(new InetSocketAddress(port));
        MulticastSocket mcast = (MulticastSocket) scket;
        mcast.connect(group, port);
        mcast.setTimeToLive(255);
        buffer = ByteBuffer.allocate(4096);
        src = new Random().nextInt();
        seq = 0;
        clk = 0;
    }

    public void write(byte[] buf, int len) throws Exception {
        buffer.clear();
        putMsb(buffer, 0, 0x800a0000 | seq);
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

    private static void putMsb(ByteBuffer buf, int ofs, int val) {
        buf.put(ofs + 0, (byte) (val >>> 24));
        buf.put(ofs + 1, (byte) (val >>> 16));
        buf.put(ofs + 2, (byte) (val >>> 8));
        buf.put(ofs + 3, (byte) val);
    }

    public static int decode(ByteBuffer buf, byte[] res) {
        int len = buf.position() - consts.rtpl;
        buf.get(consts.rtpl, res, 0, len);
        return len;
    }

    public static DatagramChannel receive(String src, String prt) throws Exception {
        InetAddress addr = InetAddress.getByName(src);
        int port = Integer.parseInt(prt);
        DatagramChannel channel = DatagramChannel.open();
        channel.socket().bind(new InetSocketAddress(addr, port));
        return channel;
    }

    public static List<String> genSdp(String grp, String src, String prt) {
        List<String> res = new ArrayList<String>();
        res.add("v=0");
        res.add("o=Node 0 0 IN IP4 " + src);
        res.add("s=None");
        res.add("c=IN IP4 " + grp);
        res.add("t=0 0");
        res.add("m=audio " + prt + " RTP/AVP 10");
        res.add("a=rtpmap:10 L" + (consts.smpb * 8) + "/" + consts.rate + "/2");
        res.add("a=source-filter: incl IN IP4 " + grp + " " + src);
        return res;
    }

    public static void sdp2cli(List<String> res) {
        res.add(0, "echo \"");
        res.add("\" | ffplay -protocol_whitelist file,fd,udp,rtp -");
    }

    public static DatagramChannel receive(String grp, String src, String prt) throws Exception {
        InetAddress group = InetAddress.getByName(grp);
        InetAddress source = InetAddress.getByName(src);
        int port = Integer.parseInt(prt);
        DatagramChannel channel = DatagramChannel.open();
        DatagramSocket scket = channel.socket();
        MulticastSocket mcast = (MulticastSocket) scket;
        channel.socket().bind(new InetSocketAddress(port));
        channel.join(group, mcast.getNetworkInterface(), source);
        return channel;
    }

}
