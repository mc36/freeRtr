
/**
 * packet size printer
 *
 * @author matecsaba
 */
public class packetSizes {

    public static void main(String[] args) throws Exception {
        for (int i = 100; i < 2000; i++) {
            double a = i;
            double b = devicer.rate * 2.0 * devicer.smpb / a;
            double c = a / (2 * devicer.smpb);
            if (b != (int) b) {
                continue;
            }
            if (c != (int) c) {
                continue;
            }
            System.out.println(i + " " + b + " " + c);
        }
    }

}
