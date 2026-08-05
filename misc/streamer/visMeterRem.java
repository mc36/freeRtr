
/**
 * measure remote level
 *
 * @author matecsaba
 */
public class visMeterRem {

    public static void main(String[] args) throws Exception {
        rtper channel = rtper.receive(args[0], args[1], args[2]);
        byte[] buf = new byte[devicer.payl];
        visDoer vu = new visDoer();
        for (;;) {
            int i = channel.read(buf);
            if (i < 1) {
                break;
            }
            vu.doer(buf, i);
        }
    }

}
