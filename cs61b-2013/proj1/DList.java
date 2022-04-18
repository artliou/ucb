/* DList.java */

/**
 *  The DList class is a doubly-linked implementation of the linked list
 *  abstraction.  DLists are mutable data structures, which can grow at either
 *  end.
 *
 *  This class is incomplete and only includes the methods necessary for the Oceans project
 **/

public class DList {
	private DListNode head;
	// we have to make this non-private so that we can manipulate size directly when we call addShark() and addFish() on a RLE
	// (at least it makes those manipulations easier)
	int size;
	// Ocean-specific fields
	int dimx;
	int dimy;
	int starvetime;
	
	/**
	 *  DList() constructs an empty list.
	 **/
	
	public DList() {
		size = 0;
		head = new DListNode();
		head.next = head;
		head.prev = head;
	}
	
	/**
	 * Ocean-specific: constructs a new list representing an i by j ocean where sharks die according to starve
	 * set size, dimx, and dimy appropriately; set starvetime to starve param
	 **/
	
	public DList(int i, int j, int starve) {
		size = 0;
		head = new DListNode();
		head.next = head;
		head.prev = head;
		dimx = i;
		dimy = j;
		starvetime = starve;
	}
	
	/**
	 *  isEmpty() indicates whether the list is empty.
	 *  @return true if the list is empty, false otherwise.
	 **/
	
	public boolean isEmpty() {
		return size == 0;
	}
	
	/**
	 *  length() returns the length of this list.
	 *  @return the length of this list.
	 **/
	
	public int length() {
		return size;
	}
	
	/**
	 *  insertFront() inserts a block of cells at the beginning of this list.
	 *  @param type the type of cell to be inserted.
	 *	@param frequency the number of consecutive cells of this type
	 **/
	
	public void insertFront(int type, int frequency, int hunger) {
		DListNode item = new DListNode(type);
		head.next.prev = item;
		item.next = head.next;
		item.prev = head;
		head.next = item;
		item.consecs = frequency;
		item.hunger = hunger;
		size++;
	}
	
	/**
	 *  insertEnd() inserts item "obj" at the end of this list.
	 *  @param type the type of cell to be inserted.
	 *	@param frequency the number of consecutive cells of this type
	 **/
	
	public void insertEnd(int type, int frequency, int hunger) {
		DListNode item = new DListNode(type);
		head.prev.next = item;
		item.next = head;
		item.prev = head.prev;
		head.prev = item;
		item.consecs = frequency;
		item.hunger = hunger;
		size++;
	}
	
	public void insertEnd(int type, int frequency) {
		insertEnd(type, frequency, 0);
	}

	public void insertFront(int type, int frequency) {
		insertFront(type, frequency, 0);
	}

	/**
	 *  nth() returns the NODE at the specified position.  If position < 1 or
	 *  position > this.length(), null is returned.  Otherwise, the item at
	 *  position "position" is returned.  The list does not change.
	 *  @param position the desired position, from 1 to length(), in the list.
	 *  @return the NODE at the given position in the list - this allows us to call .next and .prev on the returned value
	 **/
	
	public DListNode nth(int position) {
		DListNode currentNode;
		if ((position < 1) || (head.next == head)) {
			return null;
		} else {
			currentNode = head.next;
			while (position > 1) {
				currentNode = currentNode.next;
				if (currentNode == head) {
					return null;
				}
				position--;
			}
			return currentNode;
		}
	}
}