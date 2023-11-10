# pcap_int
An implementation of freertr's [pcapInt](https://github.com/rare-freertr/freeRtr/blob/master/misc/native/pcapInt.c) in Rust using the 
[libc](https://crates.io/crates/libc) and [pcap](https://crates.io/crates/pcap) crates.

Improvements to the original:

- clearer CLI arguments 
- full IPv6 support
- clearer error messages
- support for daemonizing
- support for resolving hostnames
- optional support for encryption (using the XChaCha20Poly1305 algorithm) using the [orion](https://crates.io/crates/orion) crate.

```
Options:
 --list_interfaces
 --version

Usage:
 --iface_name <str>      Name of the interface to capture on
 --laddr <socket addr>   Listen address
 --raddr <socket addr>   Remote address
 --daemonize             Run as a daemon
 ```

### Features
- `crypto`: encryption using XChaCha20Poly1305. The key must be 32 bits long, and should be the same on both sides of the tunnel.

