/*
 * File   : HashTableChained.java
 * Author : DM
 * Based on the provided distribution code
 *  ( Berkeley's cs61b http://www.cs.berkeley.edu/~jrs/61bf06/hw/ )
 *
 * Got-fu? http://gotfu.wordpress.com/
 *
 * Implements a dictionary
 */
package dict;

import list.DList;
import list.InvalidNodeException;
import list.ListNode;

/**
 *  HashTableChained implements a Dictionary as a hash table with chaining.
 *  All objects used as keys must have a valid hashCode() method, which is
 *  used to determine which bucket of the hash table an entry is stored in.
 *  Each object's hashCode() is presumed to return an int between
 *  Integer.MIN_VALUE and Integer.MAX_VALUE.  The HashTableChained class
 *  implements only the compression function, which maps the hash code to
 *  a bucket in the table's range.
 *
 *  DO NOT CHANGE ANY PROTOTYPES IN THIS FILE.
 **/
public class HashTableChained implements Dictionary {

	/**
	 *  Place any data fields here.
	 **/
	protected DList[] table;
	protected int buckets;
	protected int size;

	/**
	 *  Construct a new empty hash table intended to hold roughly sizeEstimate
	 *  entries.  (The precise number of buckets is up to you, but we recommend
	 *  you use a prime number, and shoot for a load factor between 0.5 and 1.)
	 **/
	public HashTableChained(int sizeEstimate) {
		buckets = Math.abs(sizeEstimate) + Math.abs(sizeEstimate) / 4;

		//find the next prime
		while (!isPrime(buckets))
			buckets++;
		
		size = 0;
		initBuckets();
	}

	/**
	 *  Construct a new empty hash table with a default size.  Say, a prime in
	 *  the neighborhood of 100.
	 **/
	public HashTableChained() {
		buckets = 101;
		size = 0;
		initBuckets();
	}

	/**
	 * Initilize each bucket to be a DList object.
	 * This method is used internally by the constructors
	 * and by the makeEmpty() method
	 */
	private void initBuckets() {
		table = new DList[buckets];
		for (int i = 0; i < buckets; i++) {
			table[i] = new DList();
		}
	}

	/**
	 * a slow way to check if a number is prime.
	 * (I copy-pasted this from my projecteuler.net problem solutions)
	 * 
	 * @param n
	 * @return true if n is prime, false otherwise.
	 */
	private static boolean isPrime(long n) {
		if ((n % 2) == 0)
			return false;

		long start = 2;
		long end = (long) Math.ceil(Math.sqrt(n));

		for (long i = start; i < end; i++) {
			if (n % i == 0) {
				return false;
			}
		}
		return true;
	}

	/**
	 *  Converts a hash code in the range Integer.MIN_VALUE...Integer.MAX_VALUE
	 *  to a value in the range 0...(size of hash table) - 1.
	 *
	 *  This function should have package protection (so we can test it), and
	 *  should be used by insert, find, and remove.
	 **/
	int compFunction(int code) {

		// the more sophisticated version actually does bring better results.
		long ret = ((127 * code + 65993) % 1999993) % buckets;
		if (ret < 0)
			ret += buckets;
		return (int) ret; // assuming the user uses less than Integer.MAX_VALUE buckets, this is fine.

		// this is the non-sophisticated version
//		if (code > 0)
//			return code % buckets;
//		else
//			return (code % buckets + buckets) % buckets;
	}

	/**
	 *  Returns the number of entries stored in the dictionary.  Entries with
	 *  the same key (or even the same key and value) each still count as
	 *  a separate entry.
	 *  @return number of entries in the dictionary.
	 **/
	public int size() {
		return size;
	}

	/**
	 *  Tests if the dictionary is empty.
	 *
	 *  @return true if the dictionary has no entries; false otherwise.
	 **/
	public boolean isEmpty() {
		return size == 0;
	}

	/**
	 *  Create a new Entry object referencing the input key and associated value,
	 *  and insert the entry into the dictionary.  Return a reference to the new
	 *  entry.  Multiple entries with the same key (or even the same key and
	 *  value) can coexist in the dictionary.
	 *
	 *  This method should run in O(1) time if the number of collisions is small.
	 *
	 *  @param key the key by which the entry can be retrieved.
	 *  @param value an arbitrary object.
	 *  @return an entry containing the key and value.
	 **/
	public Entry insert(Object key, Object value) {
		if (key == null)
			return null;

		int bucket = compFunction(key.hashCode());

		//make the entry
		Entry entry = new Entry();
		entry.key = key;
		entry.value = value;

		//rely on the DList code to handle the task of inserting
		table[bucket].insertFront(entry);

		size++;
		return entry;
	}

	/**
	 *  Search for an entry with the specified key.  If such an entry is found,
	 *  return it; otherwise return null.  If several entries have the specified
	 *  key, choose one arbitrarily and return it.
	 *
	 *  This method should run in O(1) time if the number of collisions is small.
	 *
	 *  @param key the search key.
	 *  @return an entry containing the key and an associated value, or null if
	 *          no entry contains the specified key.
	 **/
	public Entry find(Object key) {
		// Replace the following line with your solution.
		int bucket = compFunction(key.hashCode());
		ListNode node = table[bucket].front();
		while (node.isValidNode()) {
			try {
				Entry entry = (Entry) node.item();
				if (entry != null && key.equals(entry.key)) {
					return entry;
				}
				node = node.next();
			} catch (InvalidNodeException ex) {
				//not reachable
			}
		}
		return null;
	}

	/**
	 *  Remove an entry with the specified key.  If such an entry is found,
	 *  remove it from the table and return it; otherwise return null.
	 *  If several entries have the specified key, choose one arbitrarily, then
	 *  remove and return it.
	 *
	 *  This method should run in O(1) time if the number of collisions is small.
	 *
	 *  @param key the search key.
	 *  @return an entry containing the key and an associated value, or null if
	 *          no entry contains the specified key.
	 */
	public Entry remove(Object key) {
		// Replace the following line with your solution.
		int bucket = compFunction(key.hashCode());
		ListNode node = table[bucket].front();
		while (node.isValidNode()) {
			try {
				Entry entry = (Entry) node.item();
				if (entry != null && key.equals(entry.key)) {
					node.remove();
					size--;
					return entry;
				}
				node = node.next();
			} catch (InvalidNodeException ex) {
				//not reachable
			}
		}
		return null;
	}

	/**
	 *  Remove all entries from the dictionary.
	 */
	public void makeEmpty() {
		// Your solution here.
		size = 0;
		initBuckets();
	}
}
