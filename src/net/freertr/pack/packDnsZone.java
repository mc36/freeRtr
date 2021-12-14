package net.freertr.pack;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import net.freertr.tab.tabGen;
import net.freertr.user.userFormat;
import net.freertr.util.bits;
import net.freertr.util.cmds;

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
     * zone transfer
     */
    public boolean axfr = false;

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
        name = nam.toLowerCase();
        defttl = 24 * 60 * 60;
        axfr = true;
    }

    /**
     * list subdomains
     *
     * @return list
     */
    public List<String> subDomains() {
        List<String> lst = new ArrayList<String>();
        for (int i = 0; i < recs.size(); i++) {
            packDnsRec ntry = recs.get(i);
            String a = ntry.name;
            if (lst.contains(a)) {
                continue;
            }
            lst.add(a);
        }
        return lst;
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
        int add = 0;
        for (int i = 0; i < zon.size(); i++) {
            packDnsRec ntry = zon.get(i);
            if (ntry == null) {
                continue;
            }
            if (!addBin(ntry)) {
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
        int add = 0;
        for (int i = 0; i < zon.recs.size(); i++) {
            packDnsRec ntry = zon.recs.get(i);
            if (ntry == null) {
                continue;
            }
            if (!addBin(ntry)) {
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
        ntry.name = ntry.name.toLowerCase();
        if (ntry.ttl < 0) {
            ntry.ttl = defttl;
        }
        ntry.added = bits.getTime();
        ntry.asked = 1;
        packDnsRec old = recs.put(ntry);
        if (old == null) {
            return false;
        }
        for (int i = 0; i < old.res.size(); i++) {
            ntry.res.add(old.res.get(i));
        }
        return true;
    }

    /**
     * find one entry
     *
     * @param ntry record to find
     * @return record, null if not found
     */
    public packDnsRec findBin(packDnsRec ntry) {
        ntry.name = ntry.name.toLowerCase();
        ntry = recs.find(ntry);
        if (ntry == null) {
            return null;
        }
        ntry.asked++;
        return ntry;
    }

    /**
     * delete one entry
     *
     * @param ntry record to delete
     * @return false if deleteed, true if not found
     */
    public boolean delBin(packDnsRec ntry) {
        ntry.name = ntry.name.toLowerCase();
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
        if (s.equals("axfr")) {
            axfr = cmd.word().equals("enable");
            return false;
        }
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

    /**
     * find one wildcard
     *
     * @param nam name of entry
     * @param org original name
     * @param typ type of entry
     * @return record, null if not found
     */
    public packDnsRec findWild(String nam, String org, int typ) {
        packDnsRec ntry = new packDnsRec();
        ntry.name = nam;
        ntry.typ = typ;
        ntry = findBin(ntry);
        if (ntry == null) {
            return null;
        }
        ntry = ntry.copyBytes();
        ntry.name = org;
        return ntry;
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
        String a;
        if (axfr) {
            a = "enable";
        } else {
            a = "disable";
        }
        txt.add(beg + " axfr " + a);
        beg += " rr ";
        for (int i = 0; i < recs.size(); i++) {
            packDnsRec ntry = recs.get(i);
            if (ntry == null) {
                continue;
            }
            txt.addAll(ntry.toUserStr(" ", beg, false));
        }
        return txt;
    }

    /**
     * convert to user string
     *
     * @param stat statistics
     * @return user string
     */
    public userFormat toUserStr(boolean stat) {
        String a = "";
        if (stat) {
            a = "|hit|ttl|since";
        }
        userFormat res = new userFormat("|", "name|type|data" + a);
        for (int i = 0; i < recs.size(); i++) {
            List<String> lst = recs.get(i).toUserStr("|", "", stat);
            for (int o = 0; o < lst.size(); o++) {
                res.add(lst.get(o));
            }
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
                    packDnsRes res = new packDnsRes();
                    ntry2.res.add(res);
                    res.target = ntry.name;
                    ntry2.name = packDnsRec.generateReverse(ntry.res.get(0).addr);
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
        return o1.name.compareTo(o2.name);
    }

}
