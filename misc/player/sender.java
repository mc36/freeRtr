
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
            buffer.put(0, (byte) 0x80);
            buffer.put(1, (byte) 0xa);
            buffer.put(2, (byte) (seq >>> 8));
            buffer.put(3, (byte) seq);
            buffer.put(4, (byte) (clk >>> 24));
            buffer.put(5, (byte) (clk >>> 16));
            buffer.put(6, (byte) (clk >>> 8));
            buffer.put(7, (byte) clk);
            buffer.put(12, buf, 0, i);
            buffer.position(0);
            buffer.limit(i + 12);
            channel.write(buffer);
            seq++;
            clk += i / 4;
        }
    }

}
