
/**
 * mono conversion
 *
 * @author matecsaba
 */
public class monoDoer {

    /**
     * duplicate channel
     *
     * @param cur samples
     * @param src source
     * @param trg target
     * @param vol volume
     */
    public static void duplicate(int cur[], int src, int trg, int vol) {
        for (int i = 0; i < cur.length; i += 2) {
            long res = cur[i + src];
            res *= 100;
            res /= vol;
            cur[i + trg] = (int) res;
        }
    }

    /**
     * mix channels
     *
     * @param cur samples
     * @param vol volume
     */
    public static void mixer(int cur[], int vol) {
        for (int i = 0; i < cur.length; i += 2) {
            long res = cur[i];
            res += cur[i + 1];
            res *= 50;
            res /= vol;
            cur[i + 0] = (int) res;
            cur[i + 1] = (int) res;
        }
    }

}
