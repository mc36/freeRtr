/*
 * Copyright 2019-present GÉANT RARE project
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed On an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */


#ifndef _NEEDS_P4_
#define _NEEDS_P4_




#undef NEED_PKTLEN

#ifdef HAVE_TUN
#define NEED_PKTLEN
#endif

#ifdef HAVE_PPPOE
#define NEED_PKTLEN
#endif




#undef NEED_UDP2

#ifdef HAVE_L2TP
#define NEED_UDP2
#endif

#ifdef HAVE_GTP
#define NEED_UDP2
#endif

#ifdef HAVE_VXLAN
#define NEED_UDP2
#endif

#ifdef HAVE_PCKOUDP
#define NEED_UDP2
#endif





#undef NEED_REPLICA

#ifdef HAVE_MCAST
#define NEED_REPLICA
#endif

#ifdef HAVE_DUPLAB
#define NEED_REPLICA
#endif

#ifdef HAVE_BIER
#define NEED_REPLICA
#endif



#endif // _NEEDS_P4_
