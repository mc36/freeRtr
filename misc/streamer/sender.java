
public class sender {

    public static void main(String[] args) throws Exception {
        rtper rtp = new rtper(args[2], args[3]);
        decoder dec = new decoder(args[0], args[1]);

        for (;;) {
            byte[] buf = new byte[1024];
            int i = dec.read(buf);
            if (i < 0) {
                break;
            }
            rtp.write(buf, i);
        }
    }

}
