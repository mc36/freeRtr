
import java.io.InputStream;
import java.net.InetAddress;
import java.nio.ByteBuffer;
import java.nio.channels.DatagramChannel;

public class sender {

    public static void main(String[] args) throws Exception {
        InetAddress group = InetAddress.getByName(args[2]);
        int port = Integer.parseInt(args[3]);
        DatagramChannel channel = DatagramChannel.open();
        channel.socket().connect(group, port);
        ByteBuffer buffer = ByteBuffer.allocate(4096);

        String[] cmd = {
            "ffmpeg",
            "-ss", args[1],
            "-re",
            "-i", args[0],
            "-vn", "-sn",
            "-ar", "44100",
            "-ac", "2",
            "-c:a", "pcm_s16be",
            "-f", "s16be",
            "-"};
        Process process = Runtime.getRuntime().exec(cmd);
        InputStream stream = process.getInputStream();

        int seq = 0;
        int clk = 0;
        int src = (int) process.pid();
        for (;;) {
            byte[] buf = new byte[1024];
            int i = stream.read(buf, 0, buf.length);
            if (i < 0) {
                break;
            }
            if (i < 1) {
                if (!process.isAlive()) {
                    break;
                }
                continue;
            }
            buffer.clear();
            putMsb(buffer, 0, 0x800a0000 | seq);
            putMsb(buffer, 4, clk);
            putMsb(buffer, 8, src);
            buffer.put(12, buf, 0, i);
            buffer.position(0);
            buffer.limit(i + 12);
            channel.write(buffer);
            seq++;
            seq &= 0xffff;
            clk += i / 4;
        }
    }

    private static void putMsb(ByteBuffer buf, int ofs, int val) {
        buf.put(ofs + 0, (byte) (val >>> 24));
        buf.put(ofs + 1, (byte) (val >>> 16));
        buf.put(ofs + 2, (byte) (val >>> 8));
        buf.put(ofs + 3, (byte) val);
    }

}
