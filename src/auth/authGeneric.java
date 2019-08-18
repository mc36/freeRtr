package auth;

import java.util.Comparator;
import java.util.List;
import user.userHelping;
import util.cmds;

/**
 * authenticate one user
 *
 * @author matecsaba
 */
public abstract class authGeneric implements Comparator<authGeneric> {

    /**
     * name of list
     */
    public String autName = "";

    /**
     * log on failure
     */
    public boolean logFail = false;

    /**
     * log on error
     */
    public boolean logErr = false;

    /**
     * log on success
     */
    public boolean logOk = false;

    /**
     * authenticate user by username/password
     *
     * @param user username
     * @param pass password
     * @return authentication value
     */
    public abstract authResult authUserPass(String user, String pass);

    /**
     * authorize user command
     *
     * @param user username
     * @param cmd command
     * @return authentication value
     */
    public abstract authResult authUserCommand(String user, String cmd);

    /**
     * authenticate user by username/chap
     *
     * @param user username
     * @param id id received
     * @param chal challenge requested
     * @param resp response received
     * @return authentication value
     */
    public abstract authResult authUserChap(String user, int id, byte[] chal, byte[] resp);

    /**
     * authenticate user by username/apop
     *
     * @param cookie cookie
     * @param user username
     * @param resp response received
     * @return authentication value
     */
    public abstract authResult authUserApop(String cookie, String user, String resp);

    /**
     * get running configuration
     *
     * @param beg beginning string
     * @return string list
     */
    public abstract List<String> getShRun(String beg);

    /**
     * get help text
     *
     * @param l where to store
     */
    public abstract void getHelp(userHelping l);

    /**
     * get config text
     *
     * @return text
     */
    public abstract String getCfgName();

    /**
     * parse commands
     *
     * @param cmd commands
     * @return true if error happened
     */
    public abstract boolean fromString(cmds cmd);

    /**
     * convert to string
     *
     * @return string
     */
    public String toString() {
        return autName;
    }

    /**
     * compare two instances
     *
     * @param o1 first
     * @param o2 second
     * @return as usual
     */
    public int compare(authGeneric o1, authGeneric o2) {
        return o1.autName.toLowerCase().compareTo(o2.autName.toLowerCase());
    }

}
