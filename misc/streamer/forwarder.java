
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.MulticastSocket;
import java.nio.ByteBuffer;
import java.nio.channels.DatagramChannel;

public class forwarder {

    public static void main(String[] args) throws Exception {
        InetAddress addr = InetAddress.getByName(args[0]);
        int port = Integer.parseInt(args[1]);
        DatagramChannel source = DatagramChannel.open();
        source.socket().bind(new InetSocketAddress(addr, port));

        addr = InetAddress.getByName(args[2]);
        port = Integer.parseInt(args[3]);
        DatagramChannel target = DatagramChannel.open();
        target.socket().connect(addr, port);
        ((MulticastSocket) target.socket()).setTimeToLive(255);
        ByteBuffer buffer = ByteBuffer.allocate(4096);
        for (;;) {
            buffer.clear();
            source.receive(buffer);
            int i = buffer.position();
            if (i < 1) {
                if (!source.isConnected()) {
                    break;
                }
                continue;
            }
            buffer.limit(i);
            buffer.position(0);
            target.write(buffer);
        }
    }

}
