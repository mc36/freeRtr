/*******************************************************************************
 * BAREFOOT NETWORKS CONFIDENTIAL & PROPRIETARY
 *
 * Copyright (c) 2019-present Barefoot Networks, Inc.
 *
 * All Rights Reserved.
 *
 * NOTICE: All information contained herein is, and remains the property of
 * Barefoot Networks, Inc. and its suppliers, if any. The intellectual and
 * technical concepts contained herein are proprietary to Barefoot Networks, Inc.
 * and its suppliers and may be covered by U.S. and Foreign Patents, patents in
 * process, and are protected by trade secret or copyright law.  Dissemination of
 * this information or reproduction of this material is strictly forbidden unless
 * prior written permission is obtained from Barefoot Networks, Inc.
 *
 * No warranty, explicit or implicit is provided, unless granted under a written
 * agreement with Barefoot Networks, Inc.
 *
 ******************************************************************************/

#ifndef _BUNDLE_P4_
#define _BUNDLE_P4_

#define IPV4_IPV6_HASH   1
#define RANDOM_HASH      2
#define ROUND_ROBIN_HASH 3
#define NO_HASH          4

#if defined(USE_NO_HASH)
#define HASHING NO_HASH
#elif defined(USE_IPV4_IPV6)
#define HASHING IPV4_IPV6_HASH
#elif defined(USE_RANDOM)
#define HASHING RANDOM_HASH
#elif defined(USE_ROUND_ROBIN)
#define HASHING ROUND_ROBIN_HASH
#else
#define HASHING IPV4_IPV6_HASH
#endif

#ifndef RESILIENT_SELECTION
#define RESILIENT_SELECTION 0
#endif

#ifndef MAX_PROFILE_MEMBERS
#define MAX_PROFILE_MEMBERS 2048
#endif

#ifndef MAX_GROUP_SIZE
#define MAX_GROUP_SIZE 120
#endif

#ifndef MAX_GROUPS
#define MAX_GROUPS 1024
#endif

/* The number of required hash bits depends on both the selection algorithm
 * (resilient or fair) and the maximum group size
 *
 * The rules are as follows:
 *
 * if MAX_GROUP_SZIE <= 120:      subgroup_select_bits = 0
 * elif MAX_GROUP_SIZE <= 3840:   subgroup_select_bits = 10
 * elif MAX_GROUP_SIZE <= 119040: subgroup_select_bits = 15
 * else: ERROR
 *
 * The rules for the hash size are:
 *
 * FAIR:      14 + subgroup_select_bits
 * RESILIENT: 51 + subgroup_select_bits
 *
 */
#if RESILIENT_SELECTION == 0
const SelectorMode_t SELECTION_MODE = SelectorMode_t.FAIR;
#define BASE_HASH_WIDTH 14
#else
const SelectorMode_t SELECTION_MODE = SelectorMode_t.RESILIENT;
#define BASE_HASH_WIDTH 51
#endif /* RESILIENT_SELECTION */

#if MAX_GROUP_SIZE <= 120
#define SUBGROUP_BITS 0
#elif MAX_GROUP_SIZE <= 3840
#define SUBGROUP_BITS 10
#elif MAX_GROUP_SIZE <= 119040
#define SUBGROUP_BITS 15
#else
#error "Maximum Group Size cannot exceed 119040 members on Tofino"
#endif /* MAX_GROUP_SIZE */

/*
 * HASH_WIDTH final definition
 */
#define HASH_WIDTH (BASE_HASH_WIDTH + SUBGROUP_BITS)

const bit<32> BUNDLE_SIZE = 16384;

#endif // _BUNDLE_P4_
