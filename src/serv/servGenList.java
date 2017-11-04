package serv;

import java.util.List;

import tab.tabGen;
import cfg.cfgGeneric;

/**
 * list of generic server
 *
 * @param <T> type of
 * @author matecsaba
 */
public class servGenList<T extends servGeneric> {

    /**
     * list of servers
     */
    public final tabGen<T> lst;

    /**
     * create new list
     */
    public servGenList() {
        lst = new tabGen<T>();
    }

    private boolean updateName(T ntry) {
        ntry.srvName = ntry.srvName.trim();
        return (ntry.srvName.length() < 1);
    }

    /**
     * find one descriptor
     *
     * @param ntry descriptor to add when not found
     * @param create add this on this name if not found
     * @return descriptor, null if not found
     */
    public T find(T ntry, boolean create) {
        if (updateName(ntry)) {
            return null;
        }
        if (!create) {
            return lst.find(ntry);
        }
        T old = lst.add(ntry);
        if (old != null) {
            return old;
        }
        ntry.srvInitialize();
        return ntry;
    }

    /**
     * delete one descriptor
     *
     * @param ntry descriptor to delete
     * @return descriptor, null if not found
     */
    public T del(T ntry) {
        if (updateName(ntry)) {
            return null;
        }
        ntry = lst.del(ntry);
        if (ntry == null) {
            return null;
        }
        ntry.srvDeinit();
        return ntry;
    }

    /**
     * clear whole list
     */
    public void clear() {
        lst.clear();
    }

    /**
     * get number of list entries
     *
     * @return number of descriptors
     */
    public int size() {
        return lst.size();
    }

    /**
     * read one descriptor
     *
     * @param i number of descriptor to read
     * @return descriptor readed
     */
    public T get(int i) {
        return lst.get(i);
    }

    /**
     * get running config of this list
     *
     * @param cfg list to append
     * @param filter filter defaults
     */
    public void getShRun(List<String> cfg, boolean filter) {
        for (int i = 0; i < lst.size(); i++) {
            T ntry = lst.get(i);
            cfg.addAll(ntry.getShRun("", filter));
        }
    }

    /**
     * get running config of list
     *
     * @param <T> type of server
     * @param cfg list to append
     * @param lst list of elements
     * @param flt true to filter defaults, false to not
     */
    @SuppressWarnings("unchecked")
    public static <T extends cfgGeneric> void listGetRun(List<String> cfg, tabGen<?> lst, boolean flt) {
        for (int i = 0; i < lst.size(); i++) {
            T ace = (T) lst.get(i);
            if (ace == null) {
                continue;
            }
            cfg.addAll(ace.getShRun(flt));
        }
    }

}
