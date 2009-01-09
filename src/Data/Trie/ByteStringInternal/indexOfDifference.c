
/* Created by Configure.hs */
#include "indexOfDifference.h"

typedef unsigned char      Word8;
typedef unsigned short     Word16;
typedef unsigned long      Word32;
typedef unsigned long long Word64;

/* Hopefully this makes Nat the most optimal size, or the same as int */
#ifdef __hDataTrie_Nat64__
	typedef Word64 Nat;
#elif defined (__hDataTrie_Nat32__)
	typedef Word32 Nat;
#else
	/* No definition. Unknown architecture.
	 * Maybe it'd be worth supporting 16-bit as well...?
	 * or maybe fail back to 8-bit?
	 */
#endif

/* cf also <http://www.monkeyspeak.com/alignment/> */
#define NAT_MISALIGNMENT(p) (((int)(p)) % sizeof(Nat))


#define READ_WORD8(p) (*((Word8*) (p)))
#define READ_NAT(p)   (*((Nat*)   (p)))


/* ---------------------------------------------------------- */
/* Compare up to the first @limit@ bytes of @p1@ against @p2@
 * and return the first index where they differ. */
int indexOfDifference(void* p1, void* p2, int limit) {
	int i = 0;
	if (limit <= 0) return i;
	
	
	/* Munge until Nat-aligned */
	{
		int x1 = NAT_MISALIGNMENT(p1);
		int x2 = NAT_MISALIGNMENT(p2);
		if (x1 != x2) {
			/* BUG: what if they're misaligned differently?
			 * For now we'll use Word8 all the way */
			x1 = limit;
		}
		while (i < x1) {
			if (READ_WORD8(p1+i) == READ_WORD8(p2+i)) {
				++i;
			} else {
				return i;
			}
		}
		if (x1 == limit) {
			return limit;
		} else {
			/* Fall through */
		}
	}
	
	
	/* Check a Nat at a time until we find a mismatch */
	Nat diff;
	do {
		diff = READ_NAT(p1+i) ^ READ_NAT(p2+i);
		
		/* If the diff is valid and zero, then increment the loop */
		if (i + sizeof(Nat) <= limit && diff == 0) {
			i += sizeof(Nat);
		} else {
			break;
		}
	} while (i < limit);
	
	
	/* Clean up incomplete Nat at the end of the strings */
	if (i + sizeof(Nat) > limit) {
		// BUG: this is bigendian, not littleendian. And probably wrong too.
		Nat highestBit = 1 << ((limit-i) * sizeof(Word8));
		diff &= (~(highestBit-1)) ^ highestBit;
		
		if (0 == diff) return limit;
	}
	
	
	/* Found a difference. Do binary search to identify first
	 * byte in Nat which doesn't match. Maybe it'd be faster
	 * to specialize the Word16 and Word8 iterations so that
	 * we never have to leave Nat size...?
	 */
	Word32 w32;
	#ifdef __hDataTrie_Nat64__
	#	ifdef __hDataTrie_isLittleEndian__
			Word32 first32 = (Word32) (diff & 0x00000000FFFFFFFFull);
	#	else
			Word32 first32 = (Word32)((diff & 0xFFFFFFFF00000000ull) >> 4);
	#	endif
		if (0 == first32) {
			i += 4;
	#		ifdef __hDataTrie_isLittleEndian__
				w32 = (Word32)((diff & 0xFFFFFFFF00000000ull) >> 4);
	#		else
				w32 = (Word32) (diff & 0x00000000FFFFFFFFull);
	#		endif
		} else {
			w32 = first32;
		}
	#elif defined (__hDataTrie_Nat32__)
		w32 = diff;
	#else
		/* WTF? */
	#endif
	
	
	Word16 w16;
	#if defined (__hDataTrie_Nat32__) || defined (__hDataTrie_Nat64__)
	#	ifdef __hDataTrie_isLittleEndian__
			Word16 first16 = (Word16) (w32 & 0x0000FFFF);
	#	else
			Word16 first16 = (Word16)((w32 & 0xFFFF0000) >> 2);
	#	endif
		if (0 == first16) {
			i += 2;
	#		ifdef __hDataTrie_isLittleEndian__
				w16 = (Word16)((w32 & 0xFFFF0000) >> 2);
	#		else
				w16 = (Word16) (w32 & 0x0000FFFF);
	#		endif
		} else {
			w16 = first16;
		}
	#else
		/* WTF? */
	#endif
	
	
	Word8 w8;
	#ifdef __hDataTrie_isLittleEndian__
		Word8 first8 = (Word8) (w16 & 0x00FF);
	#else
		Word8 first8 = (Word8)((w16 & 0xFF00) >> 1);
	#endif
	if (0 == first8) {
		i += 1;
	}
	
	return i;
}
/* ---------------------------------------------------------- */
/* ----------------------------------------------------- fin. */
