#ifndef _EGRESS_CONTROL_P4_                                                      
#define _EGRESS_CONTROL_P4_                                                      
                                                                                 
/*------------------ E G R E S S  M A T C H - A C T I O N ------------------- */ 
                                                                                 
control eg_ctl(                                                                  
   /* User */                                                                    
   inout headers eg_hdr,                       
   inout ingress_metadata_t eg_md,                        
   /* Intrinsic */                                                                    
   inout standard_metadata_t ig_intr_md)               
{                                                                                
   apply {                                                                       
   }                                                                             
}                                                                                
                                                                                 
#endif // _EGRESS_CONTROL_P4_                                                    
