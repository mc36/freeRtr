
import java.util.List;

/**
 * generate sdp of stream
 *
 * @author matecsaba
 */
public class generateSdp {

    public static void main(String[] args) throws Exception {
        List<String> res = rtper.genSdp(args[0], args[1], args[2]);
        print(res);
        System.out.println("");
        rtper.sdp2cli(res);
        print(res);
    }

    public static void print(List<String> res) {
        for (int i = 0; i < res.size(); i++) {
            System.out.println(res.get(i));
        }
    }

}
