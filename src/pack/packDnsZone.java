package pack;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import tab.tabGen;
import user.userFormat;
import util.bits;
import util.cmds;

/**
 * domain name protocol (rfc1035) zone
 *
 * @author matecsaba
 */
public class packDnsZone implements Comparator<packDnsZone> {

    /**
     * name of zone
     */
    public final String name;

    /**
     * default ttl
     */
    public int defttl = 0;

    /**
     * time last updated
     */
    public long updated;

    /**
     * list of entries
     */
    protected final tabGen<packDnsRec> recs = new tabGen<packDnsRec>();

    /**
     * create empty zone
     *
     * @param nam name of zone
     */
    public packDnsZone(String nam) {
        name = nam;
        defttl = 24 * 60 * 60;
    }

    /**
     * clear all records
     */
    public void clear() {
        recs.clear();
    }

    /**
     * add entries from zone
     *
     * @param zon zone to import
     * @return records added
     */
    public int addList(List<packDnsRec> zon) {
        if (zon == null) {
            return 0;
        }
        long tim = bits.getTime();
        int add = 0;
        for (int i = 0; i < zon.size(); i++) {
            packDnsRec ntry = zon.get(i);
            if (ntry == null) {
                continue;
            }
            ntry.added = tim;
            if (recs.add(ntry) == null) {
                add++;
            }
        }
        return add;
    }

    /**
     * add entries from zone
     *
     * @param zon zone to import
     * @return records added
     */
    public int addZone(packDnsZone zon) {
        if (zon == null) {
            return 0;
        }
        long tim = bits.getTime();
        int add = 0;
        for (int i = 0; i < zon.recs.size(); i++) {
            packDnsRec ntry = zon.recs.get(i);
            if (ntry == null) {
                continue;
            }
            ntry.added = tim;
            if (recs.add(ntry) == null) {
                add++;
            }
        }
        return add;
    }

    /**
     * add one entry
     *
     * @param ntry record to add
     * @return false if added, true if updated
     */
    public boolean addBin(packDnsRec ntry) {
        if (ntry.ttl < 0) {
            ntry.ttl = defttl;
        }
        ntry.added = bits.getTime();
        return recs.put(ntry) != null;
    }

    /**
     * find one entry
     *
     * @param ntry record to delete
     * @return record, null if not found
     */
    public packDnsRec findBin(packDnsRec ntry) {
        return recs.find(ntry);
    }

    /**
     * delete one entry
     *
     * @param ntry record to delete
     * @return false if deleteed, true if not found
     */
    public boolean delBin(packDnsRec ntry) {
        return recs.del(ntry) == null;
    }

    /**
     * get size of list
     *
     * @return size of list
     */
    public int size() {
        return recs.size();
    }

    /**
     * get value
     *
     * @param i sequence number
     * @return read value, null if not found
     */
    public packDnsRec get(int i) {
        return recs.get(i);
    }

    /**
     * add one entry
     *
     * @param s user string
     * @return false if added/updated, true if error happened
     */
    public boolean addUser(String s) {
        s = s.trim();
        for (;;) {
            int i = s.indexOf("@");
            if (i < 0) {
                break;
            }
            s = s.substring(0, i) + name + s.substring(i + 1, s.length());
        }
        if (s.length() < 1) {
            return false;
        }
        packDnsRec ntry = new packDnsRec();
        cmds cmd = new cmds("dns", s);
        s = cmd.word();
        if (s.equals("defttl")) {
            defttl = bits.str2num(cmd.word());
            return false;
        }
        if (s.equals("no")) {
            s = cmd.word();
            if (!s.equals("rr")) {
                return true;
            }
            if (ntry.fromUserStr(cmd)) {
                return true;
            }
            return delBin(ntry);
        }
        if (!s.equals("rr")) {
            return true;
        }
        if (ntry.fromUserStr(cmd)) {
            return true;
        }
        ntry.ttl = defttl;
        addBin(ntry);
        return false;
    }

    /**
     * find one entry
     *
     * @param nam name of entry
     * @param typ type of entry
     * @return record, null if not found
     */
    public packDnsRec findUser(String nam, int typ) {
        packDnsRec ntry = new packDnsRec();
        ntry.name = nam;
        ntry.typ = typ;
        return findBin(ntry);
    }

    private String unComment(String lin) {
        if (lin == null) {
            return "";
        }
        int i = lin.indexOf(cmds.comment);
        if (i < 0) {
            return lin.trim();
        }
        return lin.substring(0, i).trim();
    }

    /**
     * read up zone file
     *
     * @param txt text to read
     * @return 0=successful, line number of bad line on error
     */
    public int loadZone(List<String> txt) {
        updated = bits.getTime();
        for (int i = 0; i < txt.size(); i++) {
            String s = unComment(txt.get(i));
            if (addUser(s)) {
                return i;
            }
        }
        return 0;
    }

    /**
     * save zone file
     *
     * @param beg beginning text
     * @return zone file
     */
    public List<String> saveZone(String beg) {
        List<String> txt = new ArrayList<String>();
        txt.add(beg + " defttl " + defttl);
        beg += " rr ";
        for (int i = 0; i < recs.size(); i++) {
            packDnsRec ntry = recs.get(i);
            if (ntry == null) {
                continue;
            }
            txt.add(beg + ntry.toUserStr(" "));
        }
        return txt;
    }

    /**
     * conver to user string
     *
     * @return user string
     */
    public userFormat toUserStr() {
        userFormat res = new userFormat("|", "name|type|data");
        for (int i = 0; i < recs.size(); i++) {
            res.add(recs.get(i).toUserStr("|"));
        }
        return res;
    }

    /**
     * generate reverse zone
     *
     * @return reversed zone
     */
    public packDnsZone reverseZone() {
        packDnsZone rev = new packDnsZone("reverse." + name);
        packDnsRec ntry = findUser(name, packDnsRec.typeSOA);
        if (ntry != null) {
            ntry.name = rev.name;
            rev.addBin(ntry);
        }
        for (int i = 0; i < recs.size(); i++) {
            ntry = recs.get(i);
            switch (ntry.typ) {
                case packDnsRec.typeA:
                case packDnsRec.typeAAAA:
                case packDnsRec.typeA6:
                    packDnsRec ntry2 = new packDnsRec();
                    ntry2.target = ntry.name;
                    ntry2.name = packDnsRec.generateReverse(ntry.addr);
                    ntry2.clss = packDnsRec.classIN;
                    ntry2.typ = packDnsRec.typePTR;
                    ntry2.ttl = ntry.ttl;
                    rev.addBin(ntry2);
                    break;
                default:
                    break;
            }
        }
        return rev;
    }

    public int compare(packDnsZone o1, packDnsZone o2) {
        return o1.name.toLowerCase().compareTo(o2.name.toLowerCase());
    }

}
