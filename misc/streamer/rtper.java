
import java.net.InetAddress;
import java.net.MulticastSocket;
import java.nio.ByteBuffer;
import java.nio.channels.DatagramChannel;

public class rtper {

    private ByteBuffer buffer;

    private DatagramChannel target;

    private int src;

    private int seq;

    private int clk;

    public rtper(InetAddress group, int port) throws Exception {
        target = DatagramChannel.open();
        target.socket().connect(group, port);
        ((MulticastSocket) target.socket()).setTimeToLive(255);
        buffer = ByteBuffer.allocate(4096);
        src = (int) ProcessHandle.current().pid();
        seq = 0;
        clk = 0;
    }

    public void write(byte[] buf, int len) throws Exception {
        buffer.clear();
        putMsb(buffer, 0, 0x800a0000 | seq);
        putMsb(buffer, 4, clk);
        putMsb(buffer, 8, src);
        buffer.put(12, buf, 0, len);
        buffer.position(0);
        buffer.limit(len + 12);
        target.write(buffer);
        seq++;
        seq &= 0xffff;
        clk += len / 4;
    }

    private static void putMsb(ByteBuffer buf, int ofs, int val) {
        buf.put(ofs + 0, (byte) (val >>> 24));
        buf.put(ofs + 1, (byte) (val >>> 16));
        buf.put(ofs + 2, (byte) (val >>> 8));
        buf.put(ofs + 3, (byte) val);
    }

}
