#ifndef _IG_CTL_PPPOE_P4_  
#define _IG_CTL_PPPOE_P4_  

   
control IngressControlPPPOE(inout headers hdr,
                           inout ingress_metadata_t ig_md,
                           inout standard_metadata_t ig_intr_md) {


                                                                                                  
   action send_to_cpu() {                                                                         
      /*                                                                                          
       * Prepend cpu header to pkt sent to controller                                          
       * by calling setValid() so it is tekne into account by deparser                            
       */                                                                                         
        ig_md.nexthop_id = CPU_PORT;                                                                   
   }                                                                                              


   action act_drop() {
      mark_to_drop(ig_intr_md); 
   }

   action act_pppoe_data(SubIntId_t port) {
        ig_md.source_id = port;
        if (hdr.pppoeD.ppptyp == 0x0021) ig_md.ethertype = ETHERTYPE_IPV4;
        if (hdr.pppoeD.ppptyp == 0x0057) ig_md.ethertype = ETHERTYPE_IPV6;
        if (hdr.pppoeD.ppptyp == 0x0281) ig_md.ethertype = ETHERTYPE_MPLS_UCAST;
   } 



   table tbl_pppoe {
      key = {
         ig_md.source_id: exact;  
         hdr.pppoeD.session: exact;  
      }
      actions = {
         act_drop;
         act_pppoe_data;
      }
      size = 1024;
      default_action = act_drop();
   }



   apply {                                            
        /*                                            
         * It is a dataplane packet                   
         */                                           
        if (ig_md.pppoe_ctrl_valid==1)  { 
           send_to_cpu();
           return;
        }
        if (ig_md.pppoe_data_valid==1) {
           tbl_pppoe.apply();
        }
   }                                                  


}   


#endif // _IG_CTL_PPPOE_P4_
   
