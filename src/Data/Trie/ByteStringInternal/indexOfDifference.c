
/* Created by Configure.hs */
#include "indexOfDifference.h"

typedef unsigned char      Word8;
typedef unsigned short     Word16;
typedef unsigned long      Word32;
typedef unsigned long long Word64;

/* Hopefully this makes Nat the most optimal size, or the same as int */
#ifdef __hDataTrie_Nat32__
	typedef Word32 Nat;
#else
#	ifdef __hDataTrie_Nat64__
		typedef Word64 Nat;
#	else
		/* No definition, unknown architecture */
#	endif
#endif


int indexOfDifference(void* p1, void* p2, int bytes) {
	int i = 0;
	
	/* Munge until Nat-aligned */
	// TODO
	
	
	/* Tail-call, checking a Nat at a time until we find
	 * a mismatch.
	 */
	#define READ_NAT(p) (*((Nat*) (p)))
	
	Nat diff = READ_NAT(p1+i) ^ READ_NAT(p2+i);
	while (i + sizeof(Nat) <= bytes && diff != 0) {
		i += sizeof(Nat);
		diff = READ_NAT(p1+i) ^ READ_NAT(p2+i);
	}
	
	
	/* Clean up incomplete Nat at the end of the strings */
	if (!( i + sizeof(Nat) <= bytes )) {
		Nat highestBit = 1 << (bytes * sizeof(Word8) -1);
		
		// BUG: this is bigendian, not littleendian
		diff &= (~(highestBit-1)) ^ highestBit;
		
		if (0 == diff) {
			return (i+bytes);
		}
	}
	
	
	/* Found a difference. Do binary search to identify first
	 * byte in Nat which doesn't match. Maybe it'd be faster
	 * to specialize the Word16 and Word8 iterations so that
	 * we never have to leave Nat size...
	 */
	Word32 w32;
	#ifdef __hDataTrie_Nat32__
		w32 = diff;
	#else
	#	ifdef __hDataTrie_isLittleEndian__
			Word32 first32 = diff & 0x00000000FFFFFFFFull;
	#	else
			Word32 first32 = diff & 0xFFFFFFFF00000000ull;
	#	endif
		if (0 == first32) {
			i += 4;
	#		ifdef __hDataTrie_isLittleEndian__
				w32 = diff & 0xFFFFFFFF00000000ull;
	#		else
				w32 = diff & 0x00000000FFFFFFFFull;
	#		endif
		} else {
			w32 = first32;
		}
	#endif
	
	
	Word16 w16;
	#ifdef __hDataTrie_isLittleEndian__
		Word16 first16 = w32 & 0x0000FFFF;
	#else
		Word16 first16 = w32 & 0xFFFF0000;
	#endif
	if (0 == first16) {
		i += 2;
	#	ifdef __hDataTrie_isLittleEndian__
			w16 = w32 & 0xFFFF0000;
	#	else
			w16 = w32 & 0x0000FFFF;
	#	endif
	} else {
		w16 = first16;
	}
	
	
	Word8 w8;
	#ifdef __hDataTrie_isLittleEndian__
		Word8 first8 = w16 & 0x00FF;
	#else
		Word8 first8 = w16 & 0xFF00;
	#endif
	if (0 == first8) {
		i += 1;
	}
	
	return i;
}
