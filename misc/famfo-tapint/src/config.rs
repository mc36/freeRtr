use std::env::args;
use std::error::Error;
use std::net::{IpAddr, SocketAddr, ToSocketAddrs};

#[cfg(not(any(feature = "crypto", test)))]
const CONF_HELP: &'static str = concat!(
    "Usage:\n",
    " --iface_name <str>      Name of the interface\n",
    " --laddr <socket addr>   Listen address\n",
    " --raddr <socket addr>   Remote address\n",
    " --addr <ip addr>        Address to add to the interface (in CIDR format)\n",
    " --daemonize             Run as a daemon\n\n",
    " --addr can be used multiple times to add multiple addresses to the interface\n",
);

#[cfg(any(feature = "crypto", test))]
const CONF_HELP: &'static str = concat!(
    "Usage:\n",
    " --iface_name <str>      Name of the interface\n",
    " --laddr <socket addr>   Listen address\n",
    " --raddr <socket addr>   Remote address\n",
    " --addr <ip addr>        Address to add to the interface (in CIDR format)\n",
    " --key <str>             Encryption key for the tunnel\n",
    " --daemonize             Run as a daemon\n\n",
    " --addr can be used multiple times to add multiple addresses to the interface\n",
);

#[derive(Debug)]
pub struct Config {
    pub iface_name: String,
    pub laddr: SocketAddr,
    pub raddr: SocketAddr,
    pub addr: Option<Vec<(IpAddr, u8)>>,
    pub daemonize: bool,
    #[cfg(any(feature = "crypto", test))]
    pub key: Option<String>,
}

impl Config {
    pub fn load() -> Result<Self, Box<dyn Error>> {
        let mut iface_name = None;
        let mut laddr = None;
        let mut raddr = None;
        let mut addr = Vec::new();
        let mut daemonize = false;

        #[cfg(any(feature = "crypto", test))]
        let mut key = None;

        let mut count = 0;
        let mut args = args();
        let _ = args.next();
        let mut last = String::new();

        for arg in args {
            count += 1;
            if arg.as_str() == "--daemonize" {
                daemonize = true;
            } else if last.is_empty() {
                last = arg;
            } else {
                if arg.starts_with("--") {
                    return Err(format!("Invalid argument: {last} (pos: {})", count - 1).into());
                }
                match last.as_str() {
                    "--iface_name" => iface_name = Some(arg),
                    "--laddr" => laddr = Some(arg),
                    "--raddr" => raddr = Some(arg),
                    "--addr" => addr.push(arg),
                    #[cfg(any(feature = "crypto", test))]
                    "--key" => key = Some(arg),
                    _ => {
                        return Err(format!("Invalid argument: {last} {arg} (pos: {count})").into())
                    }
                }
                last.clear();
            }
        }

        let mut err_str = String::new();
        if iface_name.is_none() {
            err_str.push_str(" - iface_name\n")
        }
        if laddr.is_none() {
            err_str.push_str(" - laddr\n")
        }
        if raddr.is_none() {
            err_str.push_str(" - raddr\n")
        }

        if !err_str.is_empty() {
            return Err(format!("Missing the following argument(s):\n{err_str}").into());
        }

        let iface_name = iface_name.unwrap();
        let laddr = laddr.unwrap().to_socket_addrs()?.next();
        if laddr.is_none() {
            return Err(format!("Couldn't resolve listen address").into());
        }
        let raddr = raddr.unwrap().to_socket_addrs()?.next();
        if raddr.is_none() {
            return Err(format!("Couldn't resolve remote address").into());
        }

        let out_addr = if addr.len() == 0 {
            None
        } else {
            let mut out_addr = Vec::new();
            for a in addr {
                if let Some((addr, len)) = a.split_once('/') {
                    out_addr.push((addr.parse()?, len.parse()?));
                } else {
                    return Err(format!("Couldn't parse address: {a}").into());
                }
            }
            Some(out_addr)
        };

        if iface_name.len() > 13 {
            return Err("Interface name can't be longer than 13 characters".into());
        }

        #[cfg(any(feature = "crypto", test))]
        if let Some(key) = &key {
            if key.as_bytes().len() != 32 {
                return Err("The key has to be exactly 32 bytes long".into());
            }
        }

        #[cfg(not(any(feature = "crypto", test)))]
        return Ok(Self {
            iface_name,
            laddr: laddr.unwrap(),
            raddr: raddr.unwrap(),
            addr: out_addr,
            daemonize,
        });

        #[cfg(any(feature = "crypto", test))]
        return Ok(Self {
            iface_name,
            laddr: laddr.unwrap(),
            raddr: raddr.unwrap(),
            addr: out_addr,
            daemonize,
            key,
        });
    }
}

pub fn usage() -> &'static str {
    CONF_HELP
}
