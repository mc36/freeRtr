package net.freertr.user;

import net.freertr.addr.addrIP;
import net.freertr.cfg.cfgAll;
import net.freertr.clnt.clntDns;
import net.freertr.pipe.pipeProgress;
import net.freertr.pipe.pipeSide;
import net.freertr.sec.secClient;
import net.freertr.serv.servGeneric;

/**
 * terminal emulator
 *
 * @author matecsaba
 */
public class userTerminal {

    private final pipeProgress console;

    /**
     * start user terminal
     *
     * @param con console to use
     */
    public userTerminal(pipeProgress con) {
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
        console.debugStat("resolving " + host + " for proto " + prt);
        addrIP adr = justResolv(host, prt);
        if (adr == null) {
            console.debugStat("not found");
            return null;
        }
        return adr;
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
            if (user == null) {
                user = console.userInput("username: ", false);
            }
            if (pass == null) {
                pass = console.userInput("password: ", true);
            }
        }
        console.debugStat("securing connection");
        stream = secClient.openSec(stream, proto, user, pass);
        if (stream == null) {
            console.debugStat("failed");
            return null;
        }
        return stream;
    }

}
