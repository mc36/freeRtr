#ifndef _IG_CTL_LLC_P4_                                                                          
#define _IG_CTL_LLC_P4_                                                                          
                                                                                                  
control IngressControlLLC(inout headers hdr,                                                     
              inout ingress_metadata_t ig_md,                                                         
                          inout standard_metadata_t ig_intr_md) {                   
                                                                                                  
   action send_to_cpu() {                                                                         
      /*                                                                                          
       * Prepend cpu header to pkt sent to controller                                          
       * by calling setValid() so it is tekne into account by deparser                            
       */                                                                                         
        ig_md.nexthop_id = CPU_PORT;                                                                   
   }                                                                                              

   apply {                                            
        /*                                            
         * It is a dataplane packet                   
         */                                           
        if (ig_md.llc_valid==1)  { 
           send_to_cpu();
        }                                             
   }                                                  

}

#endif // _IG_CTL_LLC_P4_
