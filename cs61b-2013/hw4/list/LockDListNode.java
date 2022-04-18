
//  LockDListNode.java

package list;

public class LockDListNode extends DListNode {
	
	protected boolean locked; // false by default
	
	LockDListNode(Object i, DListNode p, DListNode n) {
		super(i, p, n); 
		// simply call the super's constructor with the same args; default locked value is assigned automatically
	}	
}