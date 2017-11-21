
import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

/**
 * find duplicates
 *
 * @author matecsaba
 */
public class duplicates implements Comparator<String> {

    /**
     * the main
     *
     * @param args arguments
     */
    public static void main(String[] args) {
        String s = args[0];
        playerUtil.put("reading " + s + "...");
        String[] l1 = new File(s).list();
        List<String> lst = new ArrayList<String>();
        for (int i = 0; i < l1.length; i++) {
            lst.add(l1[i]);
        }
        Collections.sort(lst, new duplicates());
        s = "<null>";
        for (int i = 0; i < lst.size(); i++) {
            String a = lst.get(i).toLowerCase();
            if (a.startsWith(s)) {
                playerUtil.put(s);
            }
            s = a;
        }
    }

    public int compare(String o1, String o2) {
        return o1.compareTo(o2);
    }

}
