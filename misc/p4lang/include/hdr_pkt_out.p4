#ifndef _PKT_OUT_P4_   
#define _PKT_OUT_P4_   

header pkt_out_header_t {   
   PortId_t egress_port;      
   bit<7> _padding;         
}                           

#endif // _PKT_OUT_P4_
