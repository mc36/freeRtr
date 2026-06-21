
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.MulticastSocket;
import java.nio.ByteBuffer;
import java.nio.channels.DatagramChannel;
import java.util.Random;

public class rtper {

    public final static int size = 12;

    public final static int payload = 1280;

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
        buffer.put(size, buf, 0, len);
        buffer.position(0);
        buffer.limit(len + size);
        target.write(buffer);
        seq++;
        seq &= 0xffff;
        clk += len >>> 2;
    }

    private static void putMsb(ByteBuffer buf, int ofs, int val) {
        buf.put(ofs + 0, (byte) (val >>> 24));
        buf.put(ofs + 1, (byte) (val >>> 16));
        buf.put(ofs + 2, (byte) (val >>> 8));
        buf.put(ofs + 3, (byte) val);
    }

    public static byte[] decode(ByteBuffer buf) {
        int len = buf.position() - size;
        byte[] res = new byte[len];
        buf.get(size, res, 0, res.length);
        return res;
    }

    public static DatagramChannel receive(String src, String prt) throws Exception {
        InetAddress addr = InetAddress.getByName(src);
        int port = Integer.parseInt(prt);
        DatagramChannel channel = DatagramChannel.open();
        channel.socket().bind(new InetSocketAddress(addr, port));
        return channel;
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
