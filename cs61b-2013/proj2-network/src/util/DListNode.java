/* DListNode.java */

package util;

/**
 * A DListNode is a mutable node in a DList (doubly-linked list).
 **/

class DListNode {

	protected Object item;
	protected DListNode prev;
	protected DListNode next;

	/**
	 * DListNode() constructor.
	 * 
	 * @param i
	 *            the item to store in the node.
	 * @param l
	 *            the list this node is in.
	 * @param p
	 *            the node previous to this node.
	 * @param n
	 *            the node following this node.
	 */
	DListNode(Object i, DListNode p, DListNode n) {
		item = i;
		prev = p;
		next = n;
	}
}
