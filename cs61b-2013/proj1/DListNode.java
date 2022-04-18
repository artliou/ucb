/* DListNode.java */

/**
 *  DListNode is a class used internally by the DList class.  A DList object
 *  is a doubly-linked list, and a DListNode is a node of a doubly-linked
 *  list.  Each DListNode normally has three references:  one to an object, one to
 *  the next node in the list, and another to the previous node in the list.
 *
 * However, for purposes of the Ocean project this is changed as follows:
 *   The item field is actually named type to better reflect the data that it holds
 *   Consequently, the types are all ints and not objects
 *   The number of fields is FIVE and not three - an additional field is added for the number of consecutive
 *   cells of the given type and another for the shark's current hunger
 *
 */

class DListNode {
	int type;
	int hunger;
	int consecs;
	DListNode next;
	DListNode prev;
	
	/**
	 *  DListNode() (with two parameters) constructs a list node referencing the
	 *  item "obj", whose next list node is to be "next".
	 */
	
	DListNode(int type, int hunger, DListNode next) {
		this.type = type;
		this.hunger = hunger;
		this.next = next;
	}
	
	DListNode(int type, DListNode next) {
		this(type, 0, next);
	}
	
	DListNode(int type, int hunger) {
		this(type, hunger, null);
	}
	
	/**
	 *  DListNode() (with one parameter) constructs a list node referencing the
	 *  item "obj".
	 **/
	
	DListNode(int type) {
		this(type, null);
	}

	DListNode() {
		this(Ocean.EMPTY, null);
	}
}