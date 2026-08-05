
/**
 * stream from file
 *
 * @author matecsaba
 */
public class sender {

    public static void main(String[] args) throws Exception {
        decoder dec = new decoder(args[0], args[1]);
        rtper rtp = rtper.sender(args[2], args[3]);
        byte[] buf = new byte[devicer.payl];
        for (;;) {
            int i = dec.read(buf);
            if (i < 1) {
                break;
            }
            rtp.write(buf, i);
        }
    }

}
