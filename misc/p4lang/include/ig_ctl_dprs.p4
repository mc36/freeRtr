#ifndef _INGRESS_DEPARSER_P4_ 
#define _INGRESS_DEPARSER_P4_ 

 /*------------------ I N G R E S S  D E P A R S E R ------------------------- */ 

control ig_ctl_dprs(packet_out pkt,                                               
                    in headers hdr) {     
   apply {                                                                        
      /*                                                                          
       * parsed headers that have been modified                                   
       * in ctl_ingress and ctl_egress                                            
       * have to be added again into the pkt.                                     
       * for emission in the wire                                                 
       */                                                                         
      pkt.emit(hdr);                                                              
   }                                                                              
}                                                                                 
                                                                                  
#endif // _INGRESS_DEPARSER_P4_
