//
//  LockDList.java
//
package list;

public class LockDList extends DList {
	
	protected LockDListNode head;
	protected int size;
	
	protected LockDListNode newNode(Object item, DListNode prev, DListNode next) {
		return new LockDListNode(item, prev, next);
	}
	
	public LockDList() { // super() gets called regardless, so no need to do anything here (do we even need this at all?)
	}
	
	// inherit size() from super
	
	public void insertFront(Object item) {
		LockDListNode node = newNode(item, head, head.next);
		head.next.prev = node;
		head.next = node;
		size++;
	}
	
	public void insertBack(Object item) {
		LockDListNode node = newNode(item, head.prev, head);
		head.prev.next = node;
		head.prev = node;
		size++;
	}
	
	// inherit front() from super
	
	// inherit back() from super
	
	// inherit next() and prev() from super
	
	public void insertAfter(Object item, DListNode node) {
		if (node!=null && node instanceof LockDListNode) { 
			LockDListNode insertion = newNode(item, node, node.next);
			node.next.prev = insertion;
			node.next = insertion;
			size++;
		}
	}
	
	public void insertBefore(Object item, DListNode node) {
		if (node!=null && node instanceof LockDListNode) {
			LockDListNode insertion = newNode(item, node.prev, node);
			node.prev.next = insertion;
			node.prev = insertion;
			size++;
		}
	}
	
	public void remove(DListNode node) {
		if (node!=null && node instanceof LockDListNode && !node.locked) {
			super.remove(node);
		}
	}
	
	public void lockNode(LockDListNode node) {
		node.locked = true;
	}
}