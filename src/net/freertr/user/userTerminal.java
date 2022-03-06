package net.freertr.user;

import net.freertr.addr.addrIP;
import net.freertr.cfg.cfgAll;
import net.freertr.clnt.clntDns;
import net.freertr.pipe.pipeProgress;
import net.freertr.pipe.pipeSide;
import net.freertr.sec.secClient;
import net.freertr.serv.servGeneric;
import net.freertr.util.bits;

/**
 * terminal emulator
 *
 * @author matecsaba
 */
public class userTerminal {

    private final pipeSide console;

    /**
     * start user terminal
     *
     * @param con console to use
     */
    public userTerminal(pipeSide con) {
        console = con;
    }

    /**
     * do resolver work
     *
     * @param host server name
     * @param prt protocol to prefer, 0=default, 4=ip4, 6=ip6
     * @return address of remove, null on error
     */
    public static addrIP justResolv(String host, int prt) {
        clntDns clnt = new clntDns();
        if (clnt.doResolvAddr(cfgAll.nameServerAddr, host, prt)) {
            return null;
        }
        return clnt.getAddr(prt);
    }

    /**
     * resolve one host to address
     *
     * @param host hostname
     * @param prt protocol to prefer, 0=default
     * @return ip address, null if not found
     */
    public addrIP resolveAddr(String host, int prt) {
        addrIP addr = new addrIP();
        if (!addr.fromString(host)) {
            return addr;
        }
        console.strPut("resolving " + host + " for proto " + prt);
        addrIP adr = justResolv(host, prt);
        if (adr == null) {
            console.linePut(" failed");
            return null;
        }
        console.linePut(" ok!");
        return adr;
    }

    /**
     * ask user
     *
     * @param que question
     * @param hide true to hide input
     * @return entered string
     */
    public String userInput(String que, boolean hide) {
        console.strPut(que);
        int red = 0x32;
        if (hide) {
            if (cfgAll.passwdStars) {
                red = 0x33;
            } else {
                red = 0x31;
            }
        }
        String res = console.lineGet(red);
        return res;
    }

    /**
     * start ssh session
     *
     * @param stream connection to use
     * @param proto protocol to use
     * @param user username
     * @param pass password
     * @return pipeline of this connection, null=error
     */
    public pipeSide startSecurity(pipeSide stream, int proto, String user, String pass) {
        proto &= servGeneric.protoSec;
        if (stream == null) {
            return null;
        }
        if (proto == 0) {
            return stream;
        }
        if (proto == servGeneric.protoSsh) {
            pipeProgress prg = new pipeProgress(console);
            if (user == null) {
                user = userInput("username: ", false);
            }
            if (pass == null) {
                pass = userInput("password: ", true);
            }
        }
        console.strPut("securing connection");
        stream = secClient.openSec(stream, proto, user, pass);
        if (stream == null) {
            console.linePut(" failed");
            return null;
        }
        console.linePut(" ok!");
        return stream;
    }

}
