#ifndef _PKT_IN_P4_
#define _PKT_IN_P4_

header pkt_in_header_t { 
   PortId_t ingress_port;  
   bit<7> _padding;      
}                        

#endif // _PKT_IN_P4_

