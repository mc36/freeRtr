use std::env::args;
use std::error::Error;
use std::net::{SocketAddr, ToSocketAddrs};

#[cfg(not(any(feature = "crypto", test)))]
const CONF_HELP: &'static str = concat!(
    "Options:\n",
    " --list_interfaces\n",
    " --version\n\n",
    "Usage:\n",
    " --iface_name <str>      Name of the interface to capture on\n",
    " --laddr <socket addr>   Listen address\n",
    " --raddr <socket addr>   Remote address\n",
    " --daemonize             Run as a daemon\n\n",
);

#[cfg(any(feature = "crypto", test))]
const CONF_HELP: &'static str = concat!(
    "Options:\n",
    " --list_interfaces\n",
    " --version\n\n",
    "Usage:\n",
    " --iface_name <str>      Name of the interface to capture on\n",
    " --laddr <socket addr>   Listen address\n",
    " --raddr <socket addr>   Remote address\n",
    " --key <str>             Encryption key for the tunnel\n",
    " --daemonize             Run as a daemon\n",
);

#[derive(Debug)]
pub struct Config {
    pub iface_name: String,
    pub laddr: SocketAddr,
    pub raddr: SocketAddr,
    pub daemonize: bool,
    #[cfg(any(feature = "crypto", test))]
    pub key: Option<String>,
}

impl Config {
    pub fn load() -> Result<Option<Self>, Box<dyn Error>> {
        let mut iface_name = None;
        let mut laddr = None;
        let mut raddr = None;
        let mut daemonize = false;

        #[cfg(any(feature = "crypto", test))]
        let mut key = None;

        let mut count = 0;
        let mut args = args();
        let _ = args.next();
        let mut last = String::new();

        for arg in args {
            count += 1;
            match arg.as_str() {
                "--daemonize" => daemonize = true,
                "--version" => {
                    println!("{}: {}", env!("CARGO_PKG_NAME"), env!("CARGO_PKG_VERSION"));
                    return Ok(None);
                },
                "--list_interfaces" => {
                    if let Ok(interfaces) = pcap::Device::list() {
                        for iface in interfaces {
                            println!("- {}", iface.name);
                            if let Some(descr) = iface.desc {
                                println!("{descr}\n");
                            }
                        }
                        return Ok(None);
                    } else {
                        eprintln!("Failed to lookup interfaces");
                        return Ok(None);
                    }
                },
                _ => {
                    if last.is_empty() {
                        last = arg;
                    } else {
                        if arg.starts_with("--") {
                            return Err(format!("Invalid argument: {last} (pos: {})", count - 1).into());
                        }
                        match last.as_str() {
                            "--iface_name" => iface_name = Some(arg),
                            "--laddr" => laddr = Some(arg),
                            "--raddr" => raddr = Some(arg),
                            #[cfg(any(feature = "crypto", test))]
                            "--key" => key = Some(arg),
                            _ => {
                                return Err(format!("Invalid argument: {last} {arg} (pos: {count})").into())
                            }
                        }
                        last.clear();
                    }
                }
            }
        }

        let mut err_str = String::new();
        let mut errs = 0;
        if iface_name.is_none() {
            err_str.push_str(" - iface_name\n");
            errs += 1;
        }
        if laddr.is_none() {
            err_str.push_str(" - laddr\n");
            errs += 1;
        }
        if raddr.is_none() {
            err_str.push_str(" - raddr\n");
            errs += 1;
        }

        if errs == 3 {
            print!("{}", usage());
            return Ok(None);
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
        return Ok(Some(Self {
            iface_name,
            laddr: laddr.unwrap(),
            raddr: raddr.unwrap(),
            daemonize,
        }));

        #[cfg(any(feature = "crypto", test))]
        return Ok(Some(Self {
            iface_name,
            laddr: laddr.unwrap(),
            raddr: raddr.unwrap(),
            daemonize,
            key,
        }));
    }
}

pub fn usage() -> &'static str {
    CONF_HELP
}
