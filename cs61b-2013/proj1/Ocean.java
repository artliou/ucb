/* Ocean.java */

/**
 *  The Ocean class defines an object that models an ocean full of sharks and
 *  fish.  Descriptions of the methods you must implement appear below.  They
 *  include a constructor of the form
 *
 *      public Ocean(int i, int j, int starveTime);
 *
 *  that creates an empty ocean having width i and height j, in which sharks
 *  starve after starveTime timesteps.
 *
 *  See the README file accompanying this project for additional details.
 */

public class Ocean {
	
	/**
	*  Do not rename these constants.  WARNING:  if you change the numbers, you
	*  will need to recompile Test4.java.  Failure to do so will give you a very
	*  hard-to-find bug.
	*/
	
	public final static int EMPTY = 0;
	public final static int SHARK = 1;
	public final static int FISH = 2;
	
	/**
	*  Define any variables associated with an Ocean object here.  These
	*  variables MUST be private.
	*/
	
	private int x;
	private int y;
	private int starvetime;
	private int[] cells;
	private int[] hunger;
	
	/**
	 *  The following methods are required for Part I.
	 */
	
	/** 
		* Given an x & y coordinate, returns the cell's index in an ocean array
		* if either coordinate is out of bounds due to the ocean's size, convert it accordingly
		* @param x is the x-coord; @param y is the y-coord of the cell
		* @return the index in the array
		**/
	public int cellind(int x, int y) {
		x = x%this.x; //  Math.abs(x)%this.x; gives incorrect values
		y = y%this.y;
		if (x < 0) { x = x+this.x; }
		if (y < 0) { y = y+this.y; }
		int ind = this.x*y+x;
		return ind;
	}
	
	/** @param pos is the index in the ocean array
		the methods below convert an array index into an x and y coordinate, respectively **/
	
	public int coordx(int pos) {
		int i = pos%x;
		return i; }
  	
	public int coordy(int pos) {
		int j = pos/x;
		return j; }
	
	/**
	 *  Ocean() is a constructor that creates an empty ocean having width i and
	 *  height j, in which sharks starve after starveTime timesteps.
	 *  @param i is the width of the ocean.
	 *  @param j is the height of the ocean.
	 *  @param starveTime is the number of timesteps sharks survive without food.
	 */
	
	public Ocean(int i, int j, int starveTime) {
		x = i;
		y = j;
		starvetime = starveTime;
		cells = new int[i*j];
		for (int ic=0; ic<cells.length; ic++) { cells[ic] = EMPTY; } // all cells EMPTY by default
		hunger = new int[i*j];
		for (int ih=0; ih<hunger.length; ih++) { hunger[ih] = 0; } // hunger of all cells 0 by default
	}
	
	/**
	 *  width() returns the width of an Ocean object.
	 *  @return the width of the ocean.
	 */
	
	public int width() {
		return x;
	}
	
	/**
	 *  height() returns the height of an Ocean object.
	 *  @return the height of the ocean.
	 */
	
	public int height() {
		return y;
	}
	
	/**
	 *  starveTime() returns the number of timesteps sharks survive without food.
	 *  @return the number of timesteps sharks survive without food.
	 */
	
	public int starveTime() {
		return starvetime;
	}
	
	/**
	 *  addFish() places a fish in cell (x, y) if the cell is empty.  If the
	 *  cell is already occupied, leave the cell as it is.
	 *  @param x is the x-coordinate of the cell to place a fish in.
	 *  @param y is the y-coordinate of the cell to place a fish in.
	 */
	
	public void addFish(int x, int y) {
		if (cells[this.cellind(x,y)] == EMPTY) {
			cells[this.cellind(x,y)] = FISH;
		}
	}
	
	/**
	 *  addShark() (with two parameters) places a newborn shark in cell (x, y) if
	 *  the cell is empty.  A "newborn" shark is equivalent to a shark that has
	 *  just eaten.  If the cell is already occupied, leave the cell as it is.
	 *  @param x is the x-coordinate of the cell to place a shark in.
	 *  @param y is the y-coordinate of the cell to place a shark in.
	 */
	
	public void addShark(int x, int y) {
		int curpos = this.cellind(x,y);
		if (cells[curpos] == EMPTY) {
			cells[curpos] = SHARK;
			hunger[curpos] = starvetime;
		}
	}
	
	/**
	 *  cellContents() returns EMPTY if cell (x, y) is empty, FISH if it contains
	 *  a fish, and SHARK if it contains a shark.
	 *  @param x is the x-coordinate of the cell whose contents are queried.
	 *  @param y is the y-coordinate of the cell whose contents are queried.
	 */
	
	public int cellContents(int x, int y) {
		return cells[this.cellind(x,y)]; // simply return the contents at the given index
	}
	
	/**
	 *  timeStep() performs a simulation timestep as described in README.
	 *  @return an ocean representing the elapse of one timestep.
	 * required actions:
	 *  1) If a cell contains a shark, and any of its neighbors is a fish, then the
	 *  shark eats during the timestep, and it remains in the cell at the end of the
	 *  timestep.  (We may have multiple sharks sharing the same fish.  This is fine;
	 *  they all get enough to eat.)   
	 *  
	 *  2) If a cell contains a shark, and none of its neighbors is a fish, it gets
	 *  hungrier during the timestep.  If this timestep is the (starveTime + 1)th
	 *  timestep the shark has gone through without eating, then the shark dies
	 *  (disappears).  Otherwise, it remains in the cell.  An example demonstrating
	 *  this rule appears below.   
	 *  
	 *  3) If a cell contains a fish, and all of its neighbors are either empty or are
	 *  other fish, then the fish stays where it is.   
	 *
	 *  4) If a cell contains a fish, and one of its neighbors is a shark, then the
	 *  fish is eaten by a shark, and therefore disappears.   
	 *  
	 *  5) If a cell contains a fish, and two or more of its neighbors are sharks, then
	 *  a new shark is born in that cell.  Sharks are well-fed at birth; _after_ they
	 *  are born, they can survive an additional starveTime timesteps without eating.
	 *  (But they will die at the end of starveTime + 1 consecutive timesteps without
	 *  eating.)  
	 *  
	 *  6) If a cell is empty, and fewer than two of its neighbors are fish, then the
	 *  cell remains empty.
	 *  
	 *  7) If a cell is empty, at least two of its neighbors are fish, and at most one
	 *  of its neighbors is a shark, then a new fish is born in that cell.
	 *    
	 *  8) If a cell is empty, at least two of its neighbors are fish, and at least two
	 *  of its neighbors are sharks, then a new shark is born in that cell.  (The new
	 *  shark is well-fed at birth, even though it hasn't eaten a fish yet.)
	 */
	
	/** @return an array of neighbors around a given position **/
	public int[] neighborarray(int pos) {
		int[] array = new int[8];
		int x = coordx(pos);
		int y = coordy(pos);
		array[0] = cellContents(x-1, y-1); // nw
		array[1] = cellContents(x, y-1); // n
		array[2] = cellContents(x+1, y-1); // ne
		array[3] = cellContents(x+1, y); // e
		array[4] = cellContents(x+1, y+1); // se
		array[5] = cellContents(x, y+1); // s
		array[6] = cellContents(x-1, y+1); // sw
		array[7] = cellContents(x-1, y); // w
		return array; }
	
	/** @return the number of neighbors of a given type around a given position **/
	public int neighbor(int type, int pos) {
		int[] array = neighborarray(pos);
		int number = 0;
		for (int i=0; i<array.length; i++) {
			if (array[i] == type) { number++; } }
		return number; }
	
	/** @return true if the method name's condition is satisfied around for a given position **/
	public boolean caneat(int pos) { // 1 - If a cell contains a shark, and any of its neighbors is a fish
		if (cells[pos] == SHARK && neighbor(FISH, pos)>0) {
			return true;
		} else { return false; } }
	
	public boolean dangerous(int pos) { // 4 - If a cell contains a fish, and one of its neighbors is a shark
		if (cells[pos] == FISH && neighbor(SHARK, pos)==1) {
			return true;
		} else { return false; } }
   	
	public boolean givesbirth(int pos) { /* 5 - If a cell contains a fish, and two or more of its neighbors are sharks
   		8 - If a cell is empty, at least two of its neighbors are fish, and at least two of its neighbors are sharks */
		if (cells[pos] == FISH && neighbor(SHARK, pos)>1) {
			return true;
		} else if (cells[pos] == EMPTY && neighbor(FISH, pos)>1 && neighbor(SHARK, pos)>1) {
			return true;
		} else { return false; } }
   	
	public boolean spawnsfish(int pos) { // 7 - If a cell is empty, at least two of its neighbors are fish, and at most one of its neighbors is a shark
		if (cells[pos] == EMPTY && neighbor(FISH, pos)>1 && neighbor(SHARK, pos)<2) {
			return true;
		} else { return false; } }
	
	public Ocean timeStep() {
		Ocean newocean = new Ocean(x, y, starvetime); // create new Ocean to hold the state after timeStep
		/** Important notes: this new ocean is initially "blank". What this means here:
		  * If someone "dies" below, we don't need to do anything, because the cell in the new ocean is already EMPTY
		  * If any other values (i.e. hunger) change, they need to be written to the new ocean
		  * If the value of a given cell does not change, it MUST be copied to the new ocean
		**/
		for (int pos=0; pos<cells.length; pos++) {
			if (caneat(pos)) {  // the shark eats during the timestep, and it remains in the cell at the end of the timestep
				newocean.cells[pos] = SHARK;
				newocean.hunger[pos] = starvetime; }
			else if (caneat(pos) != true && cells[pos]==SHARK) {
				if (hunger[pos] == 0) { /* If this timestep is the (starveTime + 1)th timestep
					the shark has gone through without eating
					then the shark dies (disappears), so we do nothing since value is EMPTY by default */
				} else { // otherwise decrease its turns to die by 1
					newocean.cells[pos] = SHARK;
					newocean.hunger[pos] = hunger[pos] - 1;
				}
			} else if (dangerous(pos)) { /* the fish is eaten by a shark and disappears, so do nothing */
			} else if (givesbirth(pos)) {  // a new shark is born in that cell; (sharks are well-fed at birth)
				newocean.cells[pos] = SHARK;
				newocean.hunger[pos] = starvetime;
			} else if (spawnsfish(pos)) { newocean.cells[pos] = FISH;  // a new fish is born in that cell
			} else { newocean.cells[pos] = cells[pos]; } // in all other cases, just copy the existing value - CRUCIAL!
			/** 3 - If a cell contains a fish, and all of its neighbors are either empty or are other fish, then the fish stays where it is.
			if (pos.boring) {  }
			6 - If a cell is empty, and fewer than two of its neighbors are fish, then the cell remains empty.
			if (pos.fishy) {  }
			**/
		}
		return newocean; // return the newly-created and modified ocean
	}
	
/**
 *  The following method is required for Part II.
 */

/**
 *  addShark() (with three parameters) places a shark in cell (x, y) if the
 *  cell is empty.  The shark's hunger is represented by the third parameter.
 *  If the cell is already occupied, leave the cell as it is.  You will need
 *  this method to help convert run-length encodings to Oceans.
 *  @param x is the x-coordinate of the cell to place a shark in.
 *  @param y is the y-coordinate of the cell to place a shark in.
 *  @param feeding is an integer that indicates the shark's hunger.  You may
 *  encode it any way you want; for instance, "feeding" may be the
 *  last timestep the shark was fed, or the amount of time that has
 *  passed since the shark was last fed, or the amount of time left
 *  before the shark will starve.  It's up to you, but be consistent.
 */

	public void addShark(int x, int y, int feeding) {
		int curpos = this.cellind(x,y);
		if	(cells[curpos] == EMPTY) {
			cells[curpos] = SHARK;
			hunger[curpos] = feeding;
		}
	}
					 
/**
 *  The following method is required for Part III.
 */

/**
 *  sharkFeeding() returns an integer that indicates the hunger of the shark
 *  in cell (x, y), using the same "feeding" representation as the parameter
 *  to addShark() described above.  If cell (x, y) does not contain a shark,
 *  then its return value is undefined--that is, anything you want.
 *  Normally, this method should not be called if cell (x, y) does not
 *  contain a shark.  You will need this method to help convert Oceans to
 *  run-length encodings.
 *  @param x is the x-coordinate of the cell whose contents are queried.
 *  @param y is the y-coordinate of the cell whose contents are queried.
 */

	public int sharkFeeding(int x, int y) {
		return hunger[this.cellind(x,y)];
	}
	
	/* The method below does the same thing as above, but takes an array position instead of (x,y) coords */
	
	public int sharkFeeding(int pos) {
		return sharkFeeding(coordx(pos), coordy(pos));
	}

}