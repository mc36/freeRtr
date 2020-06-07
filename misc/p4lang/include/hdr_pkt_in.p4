#ifndef _PKT_IN_P4_
#define _PKT_IN_P4_

header pkt_in_header_t { 
   bit<7> _padding;      
   PortId_t ingress_port;  
}                        

#endif // _PKT_IN_P4_

