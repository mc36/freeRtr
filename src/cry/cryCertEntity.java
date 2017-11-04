package cry;

import pack.packHolder;

/**
 * certificate (rfc5280) entity
 *
 * @author matecsaba
 */
public class cryCertEntity {

    /**
     * value
     */
    public String commonName;

    /**
     * value
     */
    public String surName;

    /**
     * value
     */
    public String serialNum;

    /**
     * value
     */
    public String country;

    /**
     * value
     */
    public String locality;

    /**
     * value
     */
    public String province;

    /**
     * value
     */
    public String street;

    /**
     * value
     */
    public String organizationName;

    /**
     * value
     */
    public String organizationUnit;

    /**
     * value
     */
    public String title;

    /**
     * value
     */
    public String description;

    /**
     * value
     */
    public String searchGuide;

    /**
     * value
     */
    public String businessCat;

    /**
     * value
     */
    public String postalAddr;

    /**
     * value
     */
    public String postalCode;

    /**
     * value
     */
    public String postalBox;

    /**
     * value
     */
    public String physicalDeliver;

    /**
     * value
     */
    public String telephoneNum;

    /**
     * value
     */
    public String telexNum;

    /**
     * value
     */
    public String teletexNum;

    /**
     * value
     */
    public String faxNum;

    /**
     * value
     */
    public String x121addr;

    /**
     * value
     */
    public String intSDNnum;

    /**
     * value
     */
    public String regAddr;

    /**
     * value
     */
    public String destInd;

    /**
     * value
     */
    public String preferDeliver;

    /**
     * value
     */
    public String presentAddr;

    /**
     * value
     */
    public String supportApp;

    /**
     * value
     */
    public String member;

    /**
     * value
     */
    public String owner;

    /**
     * value
     */
    public String occupant;

    /**
     * value
     */
    public String seeAlso;

    /**
     * value
     */
    public String password;

    /**
     * value
     */
    public String certificate;

    /**
     * value
     */
    public String caCert;

    /**
     * value
     */
    public String caCrl;

    /**
     * value
     */
    public String crl;

    /**
     * value
     */
    public String crossCert;

    /**
     * value
     */
    public String name;

    /**
     * value
     */
    public String givenName;

    /**
     * value
     */
    public String initials;

    /**
     * value
     */
    public String genQual;

    /**
     * value
     */
    public String x500id;

    /**
     * value
     */
    public String dnQual;

    /**
     * value
     */
    public String enhancSearch;

    /**
     * value
     */
    public String protoInfo;

    /**
     * value
     */
    public String distingName;

    /**
     * value
     */
    public String uniqueMember;

    /**
     * value
     */
    public String houseId;

    /**
     * value
     */
    public String supportAlgs;

    /**
     * value
     */
    public String deltaCrl;

    /**
     * value
     */
    public String dmdName;

    /**
     * value
     */
    public String pseudonym;

    /**
     * value
     */
    public String role;

    public String toString() {
        return commonName;
    }

    private void addValue(packHolder pck, int typ, String val) {
        if (val == null) {
            return;
        }
        packHolder p1 = new packHolder(true, true);
        packHolder p2 = new packHolder(true, true);
        int[] oid = new int[3];
        oid[0] = 0x55;
        oid[1] = 0x04;
        oid[2] = (byte) typ;
        cryAsn1.writeObjectId(p1, oid);
        cryAsn1 a = new cryAsn1();
        a.buf = val.getBytes();
        a.tag = cryAsn1.tagPrintableString;
        a.tagWrite(p1);
        p1.merge2end();
        cryAsn1.writeSequence(p2, p1);
        cryAsn1.writeSet(pck, p2);
    }

    /**
     * write entity to asn1 format
     *
     * @param pck packet to write to
     */
    public void asn1writer(packHolder pck) {
        addValue(pck, 0x03, commonName);
        addValue(pck, 0x04, surName);
        addValue(pck, 0x05, serialNum);
        addValue(pck, 0x06, country);
        addValue(pck, 0x07, locality);
        addValue(pck, 0x08, province);
        addValue(pck, 0x09, street);
        addValue(pck, 0x0a, organizationName);
        addValue(pck, 0x0b, organizationUnit);
        addValue(pck, 0x0c, title);
        addValue(pck, 0x0d, description);
        addValue(pck, 0x0e, searchGuide);
        addValue(pck, 0x0f, businessCat);
        addValue(pck, 0x10, postalAddr);
        addValue(pck, 0x11, postalCode);
        addValue(pck, 0x12, postalBox);
        addValue(pck, 0x13, physicalDeliver);
        addValue(pck, 0x14, telephoneNum);
        addValue(pck, 0x15, telexNum);
        addValue(pck, 0x16, teletexNum);
        addValue(pck, 0x17, faxNum);
        addValue(pck, 0x18, x121addr);
        addValue(pck, 0x19, intSDNnum);
        addValue(pck, 0x1a, regAddr);
        addValue(pck, 0x1b, destInd);
        addValue(pck, 0x1c, preferDeliver);
        addValue(pck, 0x1d, presentAddr);
        addValue(pck, 0x1e, supportApp);
        addValue(pck, 0x1f, member);
        addValue(pck, 0x20, owner);
        addValue(pck, 0x21, occupant);
        addValue(pck, 0x22, seeAlso);
        addValue(pck, 0x23, password);
        addValue(pck, 0x24, certificate);
        addValue(pck, 0x25, caCert);
        addValue(pck, 0x26, caCrl);
        addValue(pck, 0x27, crl);
        addValue(pck, 0x28, crossCert);
        addValue(pck, 0x29, name);
        addValue(pck, 0x2a, givenName);
        addValue(pck, 0x2b, initials);
        addValue(pck, 0x2c, genQual);
        addValue(pck, 0x2d, x500id);
        addValue(pck, 0x2e, dnQual);
        addValue(pck, 0x2f, enhancSearch);
        addValue(pck, 0x30, protoInfo);
        addValue(pck, 0x31, distingName);
        addValue(pck, 0x32, uniqueMember);
        addValue(pck, 0x33, houseId);
        addValue(pck, 0x34, supportAlgs);
        addValue(pck, 0x35, deltaCrl);
        addValue(pck, 0x36, dmdName);
        addValue(pck, 0x41, pseudonym);
        addValue(pck, 0x48, role);
    }

    /**
     * read entity from asn1 format
     *
     * @param pck packet to read from
     * @return false on success, true on error
     */
    public boolean asn1reader(packHolder pck) {
        for (;;) {
            if (pck.dataSize() < 1) {
                break;
            }
            cryAsn1 a = new cryAsn1();
            if (a.tagRead(pck)) {
                return true;
            }
            if ((!a.cnst) || (a.tag != cryAsn1.tagSet)) {
                return true;
            }
            packHolder p = a.getPack();
            if (a.tagRead(p)) {
                return true;
            }
            if ((!a.cnst) || (a.tag != cryAsn1.tagSequence)) {
                return true;
            }
            p = a.getPack();
            if (a.tagRead(p)) {
                return true;
            }
            if ((a.cnst) || (a.tag != cryAsn1.tagObjectID)) {
                return true;
            }
            if (a.buf.length != 3) {
                continue;
            }
            if (a.buf[0] != 0x55) {
                continue;
            }
            if (a.buf[1] != 0x04) {
                continue;
            }
            int t = a.buf[2];
            if (a.tagRead(p)) {
                return true;
            }
            if (a.cnst) {
                return true;
            }
            switch (a.tag) {
                case cryAsn1.tagPrintableString:
                case cryAsn1.tagUTF8string:
                case cryAsn1.tagGeneralString:
                    break;
                default:
                    return true;
            }
            String v = new String(a.buf);
            switch (t) {
                case 0x03:
                    commonName = v;
                    break;
                case 0x04:
                    surName = v;
                    break;
                case 0x05:
                    serialNum = v;
                    break;
                case 0x06:
                    country = v;
                    break;
                case 0x07:
                    locality = v;
                    break;
                case 0x08:
                    province = v;
                    break;
                case 0x09:
                    street = v;
                    break;
                case 0x0a:
                    organizationName = v;
                    break;
                case 0x0b:
                    organizationUnit = v;
                    break;
                case 0x0c:
                    title = v;
                    break;
                case 0x0d:
                    description = v;
                    break;
                case 0x0e:
                    searchGuide = v;
                    break;
                case 0x0f:
                    businessCat = v;
                    break;
                case 0x10:
                    postalAddr = v;
                    break;
                case 0x11:
                    postalCode = v;
                    break;
                case 0x12:
                    postalBox = v;
                    break;
                case 0x13:
                    physicalDeliver = v;
                    break;
                case 0x14:
                    telephoneNum = v;
                    break;
                case 0x15:
                    telexNum = v;
                    break;
                case 0x16:
                    teletexNum = v;
                    break;
                case 0x17:
                    faxNum = v;
                    break;
                case 0x18:
                    x121addr = v;
                    break;
                case 0x19:
                    intSDNnum = v;
                    break;
                case 0x1a:
                    regAddr = v;
                    break;
                case 0x1b:
                    destInd = v;
                    break;
                case 0x1c:
                    preferDeliver = v;
                    break;
                case 0x1d:
                    presentAddr = v;
                    break;
                case 0x1e:
                    supportApp = v;
                    break;
                case 0x1f:
                    member = v;
                    break;
                case 0x20:
                    owner = v;
                    break;
                case 0x21:
                    occupant = v;
                    break;
                case 0x22:
                    seeAlso = v;
                    break;
                case 0x23:
                    password = v;
                    break;
                case 0x24:
                    certificate = v;
                    break;
                case 0x25:
                    caCert = v;
                    break;
                case 0x26:
                    caCrl = v;
                    break;
                case 0x27:
                    crl = v;
                    break;
                case 0x28:
                    crossCert = v;
                    break;
                case 0x29:
                    name = v;
                    break;
                case 0x2a:
                    givenName = v;
                    break;
                case 0x2b:
                    initials = v;
                    break;
                case 0x2c:
                    genQual = v;
                    break;
                case 0x2d:
                    x500id = v;
                    break;
                case 0x2e:
                    dnQual = v;
                    break;
                case 0x2f:
                    enhancSearch = v;
                    break;
                case 0x30:
                    protoInfo = v;
                    break;
                case 0x31:
                    distingName = v;
                    break;
                case 0x32:
                    uniqueMember = v;
                    break;
                case 0x33:
                    houseId = v;
                    break;
                case 0x34:
                    supportAlgs = v;
                    break;
                case 0x35:
                    deltaCrl = v;
                    break;
                case 0x36:
                    dmdName = v;
                    break;
                case 0x41:
                    pseudonym = v;
                    break;
                case 0x48:
                    role = v;
                    break;
            }
        }
        return false;
    }

}
