package user;

import util.cmds;

/**
 * process hw externalization
 *
 * @author matecsaba
 */
public class userHwext {

    private String path = "./";

    /**
     * do the work
     *
     * @param cmd command to do
     */
    public void doer(cmds cmd) {
        cmd.error("externalizing hardware");
        for (;;) {
            String s = cmd.word();
            if (s.length() < 1) {
                break;
            }
            s = s.toLowerCase();
            if (s.equals("path")) {
                path = cmd.word();
                continue;
            }
        }

    }

}
