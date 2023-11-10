use crate::common::{Stats, BUF_SIZE};
use crate::config::Config;
use std::error::Error;
use std::fs;
use std::io::Read;
use std::net::{Ipv4Addr, UdpSocket};
use std::os::unix::io::AsRawFd;
use std::sync::{atomic::Ordering, Arc};

#[cfg(any(feature = "crypto", test))]
use orion::aead;

// IOCTL constants
const TUN_MAGIC: u8 = b'T';
const TUNSETIFF: u8 = 202;

pub struct Tap {
    fd: std::fs::File,
    #[cfg(any(feature = "crypto", test))]
    key: Option<aead::SecretKey>,
}

impl Tap {
    pub fn open(cfg: &Config) -> Result<Self, Box<dyn Error>> {
        let tap_fd = fs::OpenOptions::new()
            .read(true)
            .write(true)
            .open("/dev/net/tun");

        let net_fd = unsafe { libc::socket(libc::AF_INET, libc::SOCK_DGRAM, 0) };
        let net_fd6 = unsafe { libc::socket(libc::AF_INET6, libc::SOCK_DGRAM, libc::IPPROTO_IP) };

        if tap_fd.is_err() {
            return Err(format!("Could not open tap interface: {}", tap_fd.unwrap_err()).into());
        };
        if net_fd < 0 {
            return Err(format!(
                "Could not open connecton to netlink: {}",
                nix::Error::last()
            )
            .into());
        };
        if net_fd6 < 0 {
            return Err(format!(
                "Could not open connecton to netlink: {}",
                nix::Error::last()
            )
            .into());
        };

        let tap_fd = tap_fd.unwrap();
        let ifr_name = get_ifrname(&cfg.iface_name);

        let ifr_flags = libc::IFF_TAP | libc::IFF_NO_PI;
        let mut ifreq = unsafe { std::mem::zeroed::<libc::ifreq>() };
        ifreq.ifr_name = ifr_name;
        ifreq.ifr_ifru.ifru_flags = ifr_flags as i16;

        nix::ioctl_write_int!(create_tap, TUN_MAGIC, TUNSETIFF);
        if let Err(e) =
            unsafe { create_tap(tap_fd.as_raw_fd(), std::ptr::addr_of_mut!(ifreq) as u64) }
        {
            return Err(format!("Could not cerate tap interface: {}", e).into());
        }

        let mut count = 0;
        if let Some(addrs) = &cfg.addr {
            for (addr, len) in addrs {
                match addr {
                    std::net::IpAddr::V4(inet) => {
                        let mut o = inet.octets();
                        o.reverse();
                        let inet = Ipv4Addr::new(o[0], o[1], o[2], o[3]);

                        let mut bits = Vec::new();
                        for _ in (32 - len)..32 {
                            bits.push(1);
                        }
                        for _ in 0..(32 - len) {
                            bits.push(0);
                        }

                        let (one, res) = bits.split_at(8);
                        let (two, res) = res.split_at(8);
                        let (three, four) = res.split_at(8);

                        let one = parse_bits(one);
                        let two = parse_bits(two);
                        let three = parse_bits(three);
                        let four = parse_bits(four);

                        let netaddr = Ipv4Addr::from([four, three, two, one]);

                        let mut ipaddr = unsafe { std::mem::zeroed::<libc::sockaddr_in>() };
                        ipaddr.sin_family = libc::AF_INET as u16;
                        ipaddr.sin_addr.s_addr = inet.into();

                        let mut netmask = unsafe { std::mem::zeroed::<libc::sockaddr_in>() };
                        netmask.sin_family = libc::AF_INET as u16;
                        netmask.sin_addr.s_addr = netaddr.into();

                        let mut ifreq = unsafe { std::mem::zeroed::<libc::ifreq>() };
                        ifreq.ifr_name = get_ifrname(&format!("{}:{}", cfg.iface_name, count));
                        ifreq.ifr_ifru.ifru_addr =
                            unsafe { *(std::ptr::addr_of_mut!(ipaddr) as *mut libc::sockaddr) };

                        #[cfg(not(target_env = "musl"))]
                        let req = unsafe { libc::ioctl(net_fd, libc::SIOCSIFADDR, &ifreq) };
                        #[cfg(target_env = "musl")]
                        let req = unsafe { libc::ioctl(net_fd, libc::SIOCSIFADDR as i32, &ifreq) };

                        if req < 0 {
                            return Err(format!(
                                "Couldn't add address {} to interface {}: {}",
                                addr,
                                cfg.iface_name,
                                nix::Error::last()
                            )
                            .into());
                        }

                        ifreq.ifr_ifru.ifru_netmask =
                            unsafe { *(std::ptr::addr_of_mut!(netmask) as *mut libc::sockaddr) };

                        #[cfg(not(target_env = "musl"))]
                        let req = unsafe { libc::ioctl(net_fd, libc::SIOCSIFNETMASK, &ifreq) };
                        #[cfg(target_env = "musl")]
                        let req =
                            unsafe { libc::ioctl(net_fd, libc::SIOCSIFNETMASK as i32, &ifreq) };

                        if req > 0 {
                            return Err(format!(
                                "Couldn't set netmask {} to interface {}: {}",
                                netaddr,
                                cfg.iface_name,
                                nix::Error::last()
                            )
                            .into());
                        }

                        count += 1;
                    }
                    std::net::IpAddr::V6(inet6) => {
                        let mut ifreq = unsafe { std::mem::zeroed::<libc::ifreq>() };
                        ifreq.ifr_name = ifr_name;

                        #[cfg(not(target_env = "musl"))]
                        let req = unsafe { libc::ioctl(net_fd6, libc::SIOCGIFINDEX, &ifreq) };
                        #[cfg(target_env = "musl")]
                        let req =
                            unsafe { libc::ioctl(net_fd6, libc::SIOCGIFINDEX as i32, &ifreq) };

                        if req > 0 {
                            return Err(format!(
                                "Couldn't get interface index for interface {}: {}",
                                cfg.iface_name,
                                nix::Error::last()
                            )
                            .into());
                        }

                        let mut in6_ifreq = unsafe { std::mem::zeroed::<libc::in6_ifreq>() };
                        in6_ifreq.ifr6_addr = unsafe { std::mem::transmute_copy(inet6) };
                        unsafe { in6_ifreq.ifr6_ifindex = ifreq.ifr_ifru.ifru_ifindex };
                        in6_ifreq.ifr6_prefixlen = *len as u32;

                        #[cfg(not(target_env = "musl"))]
                        let req = unsafe { libc::ioctl(net_fd6, libc::SIOCSIFADDR, &in6_ifreq) };
                        #[cfg(target_env = "musl")]
                        let req =
                            unsafe { libc::ioctl(net_fd6, libc::SIOCSIFADDR as i32, &in6_ifreq) };

                        if req > 0 {
                            return Err(format!(
                                "Couldn't add address {} to interface {}: {}",
                                addr,
                                cfg.iface_name,
                                nix::Error::last()
                            )
                            .into());
                        }
                    }
                }
            }
        }

        let mut ifreq = unsafe { std::mem::zeroed::<libc::ifreq>() };
        ifreq.ifr_name = ifr_name;
        ifreq.ifr_ifru.ifru_mtu = 1500;

        #[cfg(not(target_env = "musl"))]
        let req = unsafe { libc::ioctl(net_fd, libc::SIOCSIFMTU, &ifreq) };
        #[cfg(target_env = "musl")]
        let req = unsafe { libc::ioctl(net_fd, libc::SIOCSIFMTU as i32, &ifreq) };

        if req < 0 {
            return Err(format!("Couldn't set MTU of interface: {}", nix::Error::last()).into());
        }

        #[cfg(not(target_env = "musl"))]
        let req = unsafe { libc::ioctl(net_fd, libc::SIOCGIFFLAGS, &ifreq) };
        #[cfg(target_env = "musl")]
        let req = unsafe { libc::ioctl(net_fd, libc::SIOCGIFFLAGS as i32, &ifreq) };

        if req < 0 {
            return Err(format!("Couldn't get ifreq of interface: {}", nix::Error::last()).into());
        }

        unsafe { ifreq.ifr_ifru.ifru_flags |= libc::IFF_UP as i16 | libc::IFF_RUNNING as i16 };

        #[cfg(not(target_env = "musl"))]
        let req = unsafe { libc::ioctl(net_fd, libc::SIOCSIFFLAGS, &ifreq) };
        #[cfg(target_env = "musl")]
        let req = unsafe { libc::ioctl(net_fd, libc::SIOCSIFFLAGS as i32, &ifreq) };

        if req < 0 {
            return Err(format!("Couldn't set flags to interface: {}", nix::Error::last()).into());
        }

        #[cfg(not(any(feature = "crypto", test)))]
        return Ok(Self { fd: tap_fd });

        #[cfg(any(feature = "crypto", test))]
        if let Some(key) = &cfg.key {
            let key = aead::SecretKey::from_slice(key.as_bytes())?;
            return Ok(Self {
                fd: tap_fd,
                key: Some(key),
            });
        } else {
            return Ok(Self {
                fd: tap_fd,
                key: None,
            });
        }
    }

    pub fn get_tap_handle(&self) -> Result<std::fs::File, std::io::Error> {
        self.fd.try_clone()
    }

    pub fn tap_loop(&mut self, sock_tx: UdpSocket, stats: Arc<Stats>) {
        loop {
            let mut buf = [0; BUF_SIZE];
            let read = self.fd.read(&mut buf);
            if let Ok(read) = read {
                if read == 0 {
                    continue;
                }

                stats.tx_bytes.fetch_add(read, Ordering::Relaxed);
                stats.tx_packets.fetch_add(1, Ordering::Relaxed);

                #[cfg(any(feature = "crypto", test))]
                let (read, buf) = if let Some(key) = &self.key {
                    let buf = aead::seal(key, &buf[0..read]);
                    if let Err(e) = buf {
                        eprintln!("Error encrypting outgoing message: {e}");
                        continue;
                    }
                    let buf = buf.unwrap();
                    (buf.len(), buf)
                } else {
                    (read, buf.to_vec())
                };

                let send = sock_tx.send(&buf[0..read]);
                if let Err(e) = send {
                    eprintln!("Error while sending to thread connection: {e}");
                }
            } else {
                eprintln!("Error while reading from socket: {}", read.unwrap_err());
            }
        }
    }
}

fn parse_bits(bits: &[u8]) -> u8 {
    bits.iter().fold(1, |res, &bit| (res << 1) ^ bit)
}

#[cfg(not(target_arch = "aarch64"))]
fn get_ifrname(iface_name: &str) -> [i8; 16] {
    let mut ifr_name = [0; 16];
    let mut count = 0;
    for b in iface_name.bytes() {
        ifr_name[count] = b as i8;
        count += 1;
    }
    ifr_name
}

#[cfg(target_arch = "aarch64")]
fn get_ifrname(iface_name: &str) -> [u8; 16] {
    let mut ifr_name = [0; 16];
    let mut count = 0;
    for b in iface_name.bytes() {
        ifr_name[count] = b;
        count += 1;
    }
    ifr_name
}
