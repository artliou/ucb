/* DList.java */

package util;

/**
 * A DList is a mutable doubly-linked list ADT. Its implementation is
 * circularly-linked and employs a sentinel node at the head of the list.
 **/

class DList {

	protected int size;
	protected DListNode head;

	/**
	 * DList() constructs for an empty DList.
	 **/
	protected DList() {
		head = new DListNode(null, null, null);
		head.prev = head;
		head.next = head;
		size = 0;
	}

	protected void insertAt(int index, Object item){
		if(index > size)
			index = size;
		DListNode current = head;
		while(size > 0){
			current = current.next;
			size--;
		}
		DListNode toAdd = new DListNode(item, current, current.next);
		current.next = toAdd;
		current.next.prev = toAdd;
		size++;
	}
	
	/**
	 * Pops the first element of the list
	 * @return Object of first element, null if no elements
	 */
	protected Object pop(){
		if(size == 0)
			return null;
		Object item = head.next.item;
		remove(item);
		return item;
	}
	
	/**
	 * insertFront() inserts an item at the front of this DList.
	 * @param item is the item to be inserted.
	 **/
	protected void insertFront(Object item) {
		DListNode toAdd = new DListNode(item, head, head.next);
		head.next = toAdd;
		toAdd.next.prev = toAdd;
		size++;
	}

	/**
	 * insertBack() inserts an item at the back of this DList.
	 * @param item is the item to be inserted.
	 **/
	protected void insertBack(Object item) {
		DListNode toAdd = new DListNode(item, head.prev, head);
		head.prev = toAdd;
		toAdd.prev.next = toAdd;
		size++;
	}
	
	protected boolean remove(Object item){
		DListNode current = head.next;
		while(current != head){
			if(current.item.equals(item)){
				current.prev.next = current.next;
				current.next.prev = current.prev;
				size--;
				return true;
			}
			current = current.next;
		}
		return false;
	}
	
	/**
	 * toString() returns a String representation of this DList.
	 * 
	 * @return a String representation of this DList.
	 */
	public String toString() {
		String result = "[  ";
		DListNode current = head.next;
		while (current != head) {
			result = result + current.item + "  ";
			current = current.next;
		}
		return result + "]";
	}
}