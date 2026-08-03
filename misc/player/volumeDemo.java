
import java.util.Random;

/**
 * volume demo
 *
 * @author matecsaba
 */
public class volumeDemo {

    public int min;

    public int max;

    public int mov;

    public int tim;

    public volumeOne[] dev;

    public Random rnd = new Random();

    public static void main(String[] args) throws Exception {
        volumeDemo a = new volumeDemo();
        a.init(args);
        a.doer();
    }

    private void init(String[] args) throws Exception {
        min = Integer.parseInt(args[0]);
        max = Integer.parseInt(args[1]);
        mov = Integer.parseInt(args[2]);
        tim = Integer.parseInt(args[3]);
        dev = new volumeOne[args.length - 4];
        System.out.println("will use " + dev.length + " players in range " + min + "-" + max + " with " + mov + " steps in " + tim + " ms");
        for (int i = 0; i < dev.length; i++) {
            volumeOne ntry = new volumeOne(this, args[4 + i]);
            dev[i] = ntry;
            System.out.println((i + 1) + " is " + ntry.url);
            ntry.read();
        }
    }

    public void round() {
        for (;;) {
            playerUtil.sleep(tim);
            System.out.print("now:");
            for (int i = 0; i < dev.length; i++) {
                System.out.print(dev[i]);
            }
            System.out.println("");
            for (int i = 0; i < dev.length; i++) {
                dev[i].step();
            }
            boolean need = false;
            for (int i = 0; i < dev.length; i++) {
                need |= dev[i].need();
            }
            if (!need) {
                break;
            }
        }
    }

    public void doer() {
        for (;;) {
            int d = rnd.nextInt(dev.length);
            System.out.println("round for " + (d + 1));
            dev[d].need = max;
            round();
            for (int i = 0; i < dev.length; i++) {
                dev[i].need = min;
            }
            dev[d].need = max;
            round();
        }
    }

}

class volumeOne {

    public final String url;

    public final volumeDemo prnt;

    public int curr;

    public int need;

    public volumeOne(volumeDemo p, String a) {
        url = a;
        prnt = p;
    }

    public String toString() {
        String a = "" + curr;
        for (; a.length() < 5;) {
            a = " " + a;
        }
        return a;
    }

    public void read() {
        playerLyric res = playerUtil.download(url + "/player.class?cmd=vol");
        String a = res.get(res.findRegex(".*currently.*"));
        a = a.substring(a.indexOf("currently") + 9, a.length()).trim();
        a = a.substring(0, a.indexOf(" "));
        curr = Integer.parseInt(a);
        curr -= (curr % prnt.mov);
        need = curr;
    }

    public void write(int val) {
        if (val == curr) {
            return;
        }
        playerUtil.download(url + "/player.class?cmd=vol&song=" + need);
        curr = val;
    }

    public void step() {
        if (need < curr) {
            write(curr - prnt.mov);
        }
        if (need > curr) {
            write(curr + prnt.mov);
        }
    }

    public boolean need() {
        return curr != need;
    }

}
