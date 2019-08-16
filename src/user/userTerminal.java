package user;

import addr.addrIP;
import cfg.cfgAll;
import clnt.clntDns;
import clnt.clntProxy;
import java.util.ArrayList;
import java.util.List;
import pipe.pipeProgress;
import pipe.pipeSide;
import sec.secClient;
import serv.servGeneric;

/**
 * terminal emulator
 *
 * @author matecsaba
 */
public class userTerminal {

    private pipeProgress console;

    private pipeSide stream;

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
     * start tcp session
     *
     * @param prx proxy to use
     * @param proto protocol to use
     * @param addr address of remote
     * @param port port to connect to
     * @param name client name
     * @return pipeline of this connection, null=error
     */
    public pipeSide startConn(clntProxy prx, int proto, addrIP addr, int port, String name) {
        console.debugStat("connecting to " + addr + " " + port);
        if (prx == null) {
            return null;
        }
        stream = prx.doConnect(proto, addr, port, name);
        return stream;
    }

    /**
     * start ssh session
     *
     * @param proto protocol to use
     * @param user username
     * @param pass password
     * @return pipeline of this connection, null=error
     */
    public pipeSide startSecurity(int proto, String user, String pass) {
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
        return stream;
    }

    /**
     * resolve and connect
     *
     * @param proto protocol to use
     * @param server server name
     * @param port port number
     * @param name client name
     * @return pipeside
     */
    public pipeSide resolvAndConn(int proto, String server, int port, String name) {
        int prf;
        switch (proto & servGeneric.protoNets) {
            case servGeneric.protoIp4:
                prf = 4;
                break;
            case servGeneric.protoIp6:
                prf = 6;
                break;
            default:
                prf = 0;
                break;
        }
        addrIP adr = resolveAddr(server, prf);
        if (adr == null) {
            return null;
        }
        pipeSide pipe = startConn(cfgAll.getClntPrx(), proto, adr, port, name);
        if (pipe == null) {
            return null;
        }
        pipe.wait4ready(0);
        pipe.lineRx = pipeSide.modTyp.modeCRtryLF;
        pipe.lineTx = pipeSide.modTyp.modeCRLF;
        pipe.timeout = 60000;
        return pipe;
    }

}
