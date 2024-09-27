# tap_int
An implementation of freertr's [tapInt](https://github.com/rare-freertr/freeRtr/blob/master/misc/native/tapInt.c) in Rust using the 
[libc](https://crates.io/crates/libc) and [nix](https://crates.io/crates/nix) crates. 

Improvements to the original:

- clearer CLI arguments 
- the ability to add multiple IP addresses easily
- full IPv6 support
- clearer error messages
- support for daemonizing
- support for resolving hostnames
- optional support for encryption (using the XChaCha20Poly1305 algorithm) using the [orion](https://crates.io/crates/orion) crate.

```
Usage:
 --iface_name <str>      Name of the interface
 --laddr <socket addr>   Listen address
 --raddr <socket addr>   Remote address
 --addr <ip addr>        Address to add to the interface (in CIDR format)
 --daemonize             Run as a daemon

 --addr can be used multiple times to add multiple addresses to the interface
 ```

### Features
- `crypto`: encryption using XChaCha20Poly1305. The key must be 32 bits long, and should be the same on both sides of the tunnel.

