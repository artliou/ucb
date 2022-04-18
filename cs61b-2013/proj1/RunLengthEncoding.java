/* RunLengthEncoding.java */

/**
 *  The RunLengthEncoding class defines an object that run-length encodes an
 *  Ocean object.  Descriptions of the methods you must implement appear below.
 *  They include constructors of the form
 *
 *      public RunLengthEncoding(int i, int j, int starveTime);
 *      public RunLengthEncoding(int i, int j, int starveTime,
 *                               int[] runTypes, int[] runLengths) {
 *      public RunLengthEncoding(Ocean ocean) {
 *
 *  that create a run-length encoding of an Ocean having width i and height j,
 *  in which sharks starve after starveTime timesteps.
 *
 *  The first constructor creates a run-length encoding of an Ocean in which
 *  every cell is empty.  The second constructor creates a run-length encoding
 *  for which the runs are provided as parameters.  The third constructor
 *  converts an Ocean object into a run-length encoding of that object.
 *
 *  See the README file accompanying this project for additional details.
 **/

public class RunLengthEncoding {
	
	/**
	 *  Define any variables associated with a RunLengthEncoding object here.
	 *  These variables MUST be private.
	 **/
	
	// since this uses a separate doubly-linked list class, most fields are already defined there
	private int runsleft;
	private DList rle;
	
	/**
	 *  The following methods are required for Part II.
	 **/
	
	/**
	 *  RunLengthEncoding() (with three parameters) is a constructor that creates
	 *  a run-length encoding of an empty ocean having width i and height j,
	 *  in which sharks starve after starveTime timesteps.
	 *  @param i is the width of the ocean.
	 *  @param j is the height of the ocean.
	 *  @param starveTime is the number of timesteps sharks survive without food.
	 **/
	
	public RunLengthEncoding(int i, int j, int starveTime) {
		rle = new DList(i, j, starveTime);
		rle.insertFront(Ocean.EMPTY, i*j);
		runsleft = 1;
	}
	
	/**
	 *  RunLengthEncoding() (with five parameters) is a constructor that creates
	 *  a run-length encoding of an ocean having width i and height j, in which
	 *  sharks starve after starveTime timesteps.  The runs of the run-length
	 *  encoding are taken from two input arrays.  Run i has length runLengths[i]
	 *  and species runTypes[i].
	 *  @param i is the width of the ocean.
	 *  @param j is the height of the ocean.
	 *  @param starveTime is the number of timesteps sharks survive without food.
	 *  @param runTypes is an array that represents the species represented by
	 *         each run.  Each element of runTypes is Ocean.EMPTY, Ocean.FISH,
	 *         or Ocean.SHARK.  Any run of sharks is treated as a run of newborn
	 *         sharks (which are equivalent to sharks that have just eaten).
	 *  @param runLengths is an array that represents the length of each run.
	 *         The sum of all elements of the runLengths array should be i * j.
	 */
	
	public RunLengthEncoding(int i, int j, int starveTime,
							 int[] runTypes, int[] runLengths) {
		rle = new DList(i, j, starveTime);
		if (runTypes.length != runLengths.length) { System.out.println("INCOMPATIBLE ARRAYS");
		} else {
			for (int ii = runTypes.length-1; ii>=0; ii--) {
				if (runTypes[ii] == Ocean.SHARK) {
					rle.insertFront(runTypes[ii], runLengths[ii], starveTime);
				} else {
					rle.insertFront(runTypes[ii], runLengths[ii]);
				}
			}
			runsleft = runTypes.length;
		}
	}
	
	/**
	 *  restartRuns() and nextRun() are two methods that work together to return
	 *  all the runs in the run-length encoding, one by one.  Each time
	 *  nextRun() is invoked, it returns a different run (represented as a
	 *  TypeAndSize object), until every run has been returned.  The first time
	 *  nextRun() is invoked, it returns the first run in the encoding, which
	 *  contains cell (0, 0).  After every run has been returned, nextRun()
	 *  returns null, which lets the calling program know that there are no more
	 *  runs in the encoding.
	 *
	 *  The restartRuns() method resets the enumeration, so that nextRun() will
	 *  once again enumerate all the runs as if nextRun() were being invoked for
	 *  the first time.
	 *
	 *  (Note:  Don't worry about what might happen if nextRun() is interleaved
	 *  with addFish() or addShark(); it won't happen.)
	 */
	
	/**
	 *  restartRuns() resets the enumeration as described above, so that
	 *  nextRun() will enumerate all the runs from the beginning.
	 */
	
	public void restartRuns() {
		runsleft = rle.length();
	}
	
	/**
	 *  nextRun() returns the next run in the enumeration, as described above.
	 *  If the runs have been exhausted, it returns null.  The return value is
	 *  a TypeAndSize object, which is nothing more than a way to return two
	 *  integers at once.
	 *  @return the next run in the enumeration, represented by a TypeAndSize object.
	 */
	
	public TypeAndSize nextRun() {
		if (runsleft == 0) { return null; }
		else {
			int curindex = rle.length()-runsleft+1;
			DListNode curnode = rle.nth(curindex);
			int tip = curnode.type;
			int freq = curnode.consecs;
			runsleft--;
			return new TypeAndSize(tip, freq); }
	}
	
	/**
	 *  toOcean() converts a run-length encoding of an ocean into an Ocean
	 *  object.  You will need to implement the three-parameter addShark method
	 *  in the Ocean class for this method's use.
	 *  @return the Ocean represented by a run-length encoding.
	 */
	
	public Ocean toOcean() {
		int index = 0;
		Ocean newocean = new Ocean(rle.dimx, rle.dimy, rle.starvetime);
		for (int il = 1; il<=rle.length(); il++) { // walks through each node in the rle list
			DListNode curnode = rle.nth(il);
			int tip = curnode.type;
			int freq = curnode.consecs;
			int hunger = curnode.hunger;
			int ia = index;
			for (; ia < index+freq; ia++) {
				if (tip == Ocean.SHARK) {
					newocean.addShark(newocean.coordx(ia), newocean.coordy(ia), hunger);
				} else if (tip == Ocean.FISH) {
					newocean.addFish(newocean.coordx(ia), newocean.coordy(ia));
				} else { } // do nothing, the cell is already EMPTY
			}
			index = ia;
		}
		return newocean;
	}
	
	/**
	 *  The following method is required for Part III.
	 */
	
	/**
	 *  RunLengthEncoding() (with one parameter) is a constructor that creates
	 *  a run-length encoding of an input Ocean.  You will need to implement
	 *  the sharkFeeding method in the Ocean class for this constructor's use.
	 *  @param sea is the ocean to encode.
	 */
	
	public RunLengthEncoding(Ocean sea) {
		int is=0;
		int ir=1;
		int seasize = sea.width()*sea.height();
		rle = new DList(sea.width(), sea.height(), sea.starveTime());
		while (is<seasize) {
			int tip = sea.cellContents(sea.coordx(is), sea.coordy(is));
			int hunger = sea.sharkFeeding(is);
			rle.insertEnd(tip, 1, hunger);
			is++;
			for (; sea.cellContents(sea.coordx(is), sea.coordy(is)) == tip && sea.sharkFeeding(is) == hunger && is<seasize; is++) {
				rle.nth(ir).type = tip;
				rle.nth(ir).consecs++;
				rle.nth(ir).hunger = hunger;
			}
			ir++;
		}
		runsleft = ir-1;
		check();
	}
	
	/**
	 *  The following methods are required for Part IV.
	 */
	
	/**
	 *  addFish() places a fish in cell (x, y) if the cell is empty.  If the
	 *  cell is already occupied, leave the cell as it is.  The final run-length
	 *  encoding should be compressed as much as possible; there should not be
	 *  two consecutive runs of sharks with the same degree of hunger.
	 *  @param x is the x-coordinate of the cell to place a fish in.
	 *  @param y is the y-coordinate of the cell to place a fish in.
	 */
	
	// This helper helps us convert (x,y) into a single-digit position but is slightly modified from the version in Ocean.java to work with RLEs
	public int itempos(int x, int y) {
		x = x%rle.dimx;
		y = y%rle.dimy;
		if (x < 0) { x = x+rle.dimx; }
		if (y < 0) { y = y+rle.dimy; }
		int ind = (rle.dimx*y+x) + 1;
		return ind;
	}

	public void addFish(int x, int y) {
		int index = itempos(x, y);
		int rlepos = 1;
		int i = 1;
		for (; i<rle.length(); i++) {
			rlepos = rlepos + rle.nth(i).consecs;
			if (rlepos > index) { 
				rlepos = rlepos - rle.nth(i).consecs - 1;
				break; }
		}
		if (i>=rle.length()) { System.out.println("Something terribly wrong happened, sorry..."); }
		else if (rle.nth(i).type != Ocean.EMPTY) { System.out.println("Destination cell is not empty"); }
		else actualinsertion: { 
			DListNode removednode = rle.nth(i);
			int after = removednode.consecs - (index - rlepos);
			int before = removednode.consecs - after - 1;
			DListNode insertionnode = new DListNode(Ocean.FISH);
			insertionnode.consecs = 1;
			DListNode beforenode;
			DListNode afternode;
			boolean nodeisgone = false;
			if (after==0 && removednode.next.type==Ocean.FISH && removednode.prev.type!=Ocean.FISH) { // there are no empty spaces, following block is a fish, and previous block is NOT a fish
				removednode.next.consecs++;
				nodeisgone = true;
			}  if (before==0 && removednode.prev.type==Ocean.FISH && removednode.next.type!=Ocean.FISH) { // there are no empty spaces, previous block is fish, and following block is NOT fish
				removednode.prev.consecs++;
				nodeisgone = true;
			} if (after==0 && before==0 && removednode.prev.type!=Ocean.FISH && removednode.next.type!=Ocean.FISH) { // this cell is nestled between two blocks neither of which is a fish
				insertionnode.next = removednode.next;
				insertionnode.next.prev = insertionnode;
				insertionnode.prev = removednode.prev;
				insertionnode.prev.next = insertionnode;
				// we're done here
				break actualinsertion;
			} if (removednode.next.type==Ocean.FISH && removednode.prev.type==Ocean.FISH) { // both previous and next blocks contain fish, so we need to combine three blocks into one
				int combination = removednode.next.consecs + removednode.prev.consecs + 1;
				DListNode overhaul = new DListNode(Ocean.FISH);
				overhaul.consecs = combination;
				overhaul.next = removednode.next.next;
				overhaul.next.prev = overhaul;
				overhaul.prev = removednode.prev.prev;
				overhaul.prev.next = overhaul;
				rle.size = rle.size -2;
				runsleft = rle.length();
				// we're done here
				break actualinsertion;
			} if (nodeisgone) { // if as a result of the previous testcases, our initially created node is no longer needed (nodeisgone == true), we need to combine the before and after frequencies, make a single new node, and link it to the list appropriately
				int combined = after + before;
				if (combined > 0) {
					DListNode superduper = new DListNode(Ocean.EMPTY);
					superduper.consecs = combined;
					superduper.next = removednode.next;
					superduper.prev = removednode.prev;
					superduper.prev.next = superduper;
					superduper.next.prev = superduper;
				} else {
					removednode.prev.next = removednode.next;
					removednode.next.prev = removednode.prev;
					rle.size--;
				}
			}  else if (before>0 || after>0) {
				if (before > 0) { // there are still some empty spaces before this fish
					beforenode = new DListNode(Ocean.EMPTY);
					beforenode.consecs = before;
					beforenode.prev = removednode.prev;
					removednode.prev.next = beforenode;
					beforenode.next = insertionnode;
					insertionnode.prev = beforenode;
					if (after == 0) {
						insertionnode.next = removednode.next;
						insertionnode.next.prev = insertionnode; }
					rle.size++;
					runsleft = rle.length();
				} if (after > 0) { // there are still some empty spaces after this fish
					afternode = new DListNode(Ocean.EMPTY);
					afternode.consecs = after;
					afternode.next = removednode.next;
					afternode.next.prev = afternode;
					insertionnode.next = afternode;
					if (before == 0) {
						insertionnode.prev = removednode.prev;
						insertionnode.prev.next = insertionnode; }
					afternode.prev = insertionnode;
					rle.size++;
					runsleft = rle.length();
				}
			}
		}
		check();
	}
	
	/**
	 *  addShark() (with two parameters) places a newborn shark in cell (x, y) if
	 *  the cell is empty.  A "newborn" shark is equivalent to a shark that has
	 *  just eaten.  If the cell is already occupied, leave the cell as it is.
	 *  The final run-length encoding should be compressed as much as possible;
	 *  there should not be two consecutive runs of sharks with the same degree
	 *  of hunger.
	 *  @param x is the x-coordinate of the cell to place a shark in.
	 *  @param y is the y-coordinate of the cell to place a shark in.
	 */
	
	public void addShark(int x, int y) {
		int index = itempos(x, y);
		int rlepos = 1;
		int i = 1;
		for (; i<rle.length(); i++) {
			rlepos = rlepos + rle.nth(i).consecs;
			if (rlepos > index) { 
				rlepos = rlepos - rle.nth(i).consecs - 1;
				break; }
		}
		if (i>=rle.length()) { System.out.println("Something terribly wrong happened, sorry..."); }
		else if (rle.nth(i).type != Ocean.EMPTY) { System.out.println("Destination cell is not empty"); }
		else actualinsertion: { 
			DListNode removednode = rle.nth(i);
			int destinationhunger = rle.starvetime;
			int after = removednode.consecs - (index - rlepos);
			int before = removednode.consecs - after - 1;
			DListNode insertionnode = new DListNode(Ocean.SHARK, destinationhunger);
			insertionnode.consecs = 1;
			DListNode beforenode;
			DListNode afternode;
			boolean nodeisgone = false;
			if (after==0 && removednode.next.type==Ocean.SHARK && removednode.next.hunger==destinationhunger && removednode.prev.hunger!=destinationhunger) { // there are no empty spaces, following block is a shark with the same hunger, and previous block is NOT a shark with the same hunger
				removednode.next.consecs++;
				nodeisgone = true;
			}  if (before==0 && removednode.prev.type==Ocean.SHARK && removednode.prev.hunger==destinationhunger && removednode.next.hunger!=destinationhunger) { // there are no empty spaces, previous block is a shark with the same hunger, and following block is NOT a shark with the same hunger
				removednode.prev.consecs++;
				nodeisgone = true;
			} if (after==0 && before==0 && removednode.prev.hunger!=destinationhunger && removednode.next.hunger!=destinationhunger) { // this cell is nestled between two blocks that differ from the shark we're inserting.
				insertionnode.next = removednode.next;
				insertionnode.next.prev = insertionnode;
				insertionnode.prev = removednode.prev;
				insertionnode.prev.next = insertionnode;
				// we're done here
				break actualinsertion;
			} if (after==0 && before==0 && removednode.next.type==Ocean.SHARK && removednode.prev.type==Ocean.SHARK && removednode.next.hunger==destinationhunger && removednode.prev.hunger==destinationhunger) { // we're inserting into a cell surrounded by blocks of sharks both of which have the same hunger levels, so we need to combine three blocks into one
				int combination = removednode.next.consecs + removednode.prev.consecs + 1;
				DListNode overhaul = new DListNode(Ocean.SHARK, destinationhunger);
				overhaul.consecs = combination;
				overhaul.next = removednode.next.next;
				overhaul.next.prev = overhaul;
				overhaul.prev = removednode.prev.prev;
				overhaul.prev.next = overhaul;
				rle.size = rle.size -2;
				runsleft = rle.length();
				// we're done here
				break actualinsertion;
			} if (nodeisgone) { // if as a result of the previous testcases, our initially created node is no longer needed (nodeisgone == true), we need to combine the before and after frequencies, make a single new node, and link it to the list appropriately
				int combined = after + before;
				if (combined > 0) {
					DListNode superduper = new DListNode(Ocean.EMPTY);
					superduper.consecs = combined;
					superduper.next = removednode.next;
					superduper.prev = removednode.prev;
					superduper.prev.next = superduper;
					superduper.next.prev = superduper;
				} else {
					removednode.prev.next = removednode.next;
					removednode.next.prev = removednode.prev;
					rle.size--;
				}
			} else if (before>0 || after>0) {
				if (before > 0) { // there are still some empty spaces before this shark or there are sharks but with different hunger levels
					beforenode = new DListNode(Ocean.EMPTY);
					beforenode.consecs = before;
					beforenode.prev = removednode.prev;
					removednode.prev.next = beforenode;
					beforenode.next = insertionnode;
					insertionnode.prev = beforenode;
					if (after == 0) {
						insertionnode.next = removednode.next;
						insertionnode.next.prev = insertionnode; }
					rle.size++;
					runsleft = rle.length();
				} if (after > 0) { // there are still some empty spaces after this shark or there are sharks but with different hunger levels
					afternode = new DListNode(Ocean.EMPTY);
					afternode.consecs = after;
					afternode.next = removednode.next;
					afternode.next.prev = afternode;
					insertionnode.next = afternode;
					if (before == 0) {
						insertionnode.prev = removednode.prev;
						insertionnode.prev.next = insertionnode; }
					afternode.prev = insertionnode;
					rle.size++;
					runsleft = rle.length();
				}
			} 
		}
		check();
	}
	
	/**
	 *  check() walks through the run-length encoding and prints an error message
	 *  if two consecutive runs have the same contents, or if the sum of all run
	 *  lengths does not equal the number of cells in the ocean.
	 */
	
	public void check() {
		int ti = 0;
		for (int i=1; i<rle.length(); i++) {
			if (rle.nth(i).type == rle.nth(i+1).type && rle.nth(i).hunger == rle.nth(i+1).hunger) {
				System.out.print("I've found a bug! You have the same type of cells in two consecutive RLE nodes! Problem is at nodes " + i + " and " + (i+1) + "\n");
			}
		}
		for (int ii=1; ii<=rle.length(); ii++) {
			ti = ti + rle.nth(ii).consecs;
		}	
		if (ti != rle.dimx*rle.dimy) {
			System.out.print("I've found a bug! The sum of run lengths is NOT equal to the Ocean's size - runlengths is " + ti + " vs Ocean size of " + (rle.dimx*rle.dimy) + "\n");
		}
	}
	
}