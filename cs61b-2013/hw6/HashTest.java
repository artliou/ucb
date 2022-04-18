/*
 * File   : SimpleBoard.java
 * Author : DM
 * License: BSD 3-clause license http://gotfu.wordpress.com/bsd-3-clause-license/
 * Copyright (c) 2012 Got-fu? http://gotfu.wordpress.com/
 *
 * Tests CS61B hw6 using the following key types:
 * - wordlists
 * - series of integers
 * - randomized SimpleBoard instances
 */

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

public class HashTest extends dict.HashTableChained {

	HashTest(int N) {
		super(N);
	}

	public static void main(String[] args) {

		testWordsFromFile("resources/HangmanLexicon.txt");
		testWordsFromFile("resources/words.txt");

		testIntegers(1000);
		testIntegers(10000);
		testIntegers(100000);

		testSimpleBoards(1000);
		testSimpleBoards(10000);
		testSimpleBoards(100000);

	}

	/**
	 * make a graph of hits per bucket... ?
	 * x axis = bucket number
	 * y axis = number of hits.
	 * 1000 = x + x / 4
	 *
	 */
	public static void testIntegers(int N) {
		HashTest ht = new HashTest(N);
		for (int i = 0; i <= N; i++) {
			ht.insert(i, i);
		}
		System.out.println("Results for integer keys in 0.." + N);
		ht.investigate();
	}

	public static void testWordsFromFile(String pathToFile) {
		int sizeEstimate = 0;
		try {
			//count lines.
			{
				BufferedReader rd = new BufferedReader(new FileReader(pathToFile));
				while (rd.readLine() != null)
					sizeEstimate++;
				rd.close();
			}
			// lines counted.

			HashTest ht = new HashTest(sizeEstimate);

			BufferedReader rd = new BufferedReader(new FileReader(pathToFile));
			while (true) {
				String line = rd.readLine();
				if (line == null) {
					break;
				}
				ht.insert(line, line);
			}
			rd.close();

			System.out.println("Results for file: " + pathToFile);
			ht.investigate();
		} catch (IOException ex) {
			throw new Error(ex);
		}
	}

	/**
	 *  Generates a random 8 x 8 SimpleBoard.
	 * this function take from HomeWork6Test.java unchanged.
	 */
	private static SimpleBoard randomBoard() {
		SimpleBoard board = new SimpleBoard();
		for (int y = 0; y < 8; y++) {
			for (int x = 0; x < 8; x++) {
				double fval = Math.random() * 12;
				int value = (int) fval;
				board.setElementAt(x, y, value);
			}
		}
		return board;
	}

	public static void testSimpleBoards(int numBoards) {
		HashTest ht = new HashTest(numBoards);
		for (int i = 0; i < numBoards; i++) {
			ht.insert(randomBoard(), new Integer(i));
		}

		System.out.println("Results for " + numBoards + " random SimpleBoards.");
		ht.investigate();
	}

	public void investigate() {
		int collisions = 0;
		int longestChain = -1;
		int sumLengths = 0;
		Map<Integer, Integer> frequencies = new HashMap<Integer, Integer>();

		for (int i = 0; i < buckets; i++) {
			int len = table[i].length();
			if (len > longestChain)
				longestChain = len;

			sumLengths += len;

			/*
			 * a collision occurs when a key hits an already occupied bucket,
			 * so the 'first' element of the chain is NOT a collision. (hence the -1),
			 * but subsequent ones are.
			 */
			if (len > 1)
				collisions += len - 1;

			//increase the count in the frequencies map.
			if (frequencies.containsKey(len)) {
				Integer freq = frequencies.get(len);
				freq++;
				frequencies.put(len, freq);
			} else {
				frequencies.put(len, 1);
			}
		}
		double loadFactor = (double) sumLengths / (double) buckets;
		/*
		 * see readme.txt
		 * probability of collision
		 * f(n,N) = n - N + N (1 - 1/N)^n
		 * where n is the number of items, and N is the number of keys (buckets)
		 * remember to cast to double!
		 */
		double expected_collisions = size - buckets + (buckets * Math.pow(1 - ((double) 1 / (double) buckets), size));

		System.out.println("---------------------------------------------");
		System.out.println("Size (n): " + size);
		System.out.println("Buckets (N): " + buckets);
		System.out.println("Expected number of collisions: " + expected_collisions);
		System.out.println("Actual Number of Collisions: " + collisions);
		System.out.println("LongestChain: " + longestChain);
		System.out.println("Average (Mean) LoadFactor: " + loadFactor);
		//System.out.println("TODO: Statistical Average of LoadFactor: ???");
		System.out.println("Distinct length frequencies: " + frequencies.size());
		Iterator it = frequencies.entrySet().iterator();

		while (it.hasNext()) {
			Map.Entry entry = (Map.Entry) it.next();
			System.out.println("Frequency of length " + entry.getKey() + " : " + entry.getValue());
		}
		System.out.println("---------------------------------------------");
		System.out.println();
		System.out.println();
	}
}
