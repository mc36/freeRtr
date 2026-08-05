
/**
 * forward stream
 *
 * @author matecsaba
 */
public class forwarder {

    public static void main(String[] args) throws Exception {
        rtper source = rtper.receive(args[0], args[1]);
        rtper rtp = rtper.sender(args[2], args[3]);
        byte[] buf = new byte[devicer.payl];
        for (;;) {
            int i = source.read(buf);
            if (i < 1) {
                break;
            }
            rtp.write(buf, i);
        }
    }

}
