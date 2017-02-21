package gitlet;

import java.io.Serializable;

/** A linked list-like class that connects related commits in a chain.
* @author Arthur Liou and Zoey Kenny */
public class CommitChain implements Serializable {

    /** Creates a new chain of commits.
    *@param curr
    *       The front of the chain.
    (@param nxt
    *        The next chain of commits, or null.
    */
    public CommitChain(Commit curr, CommitChain nxt) {
        this.current = curr;
        this.next = nxt;
    }

    /** Returns true if there is another commit. */
    public boolean hasnext() {
        return next != null;
    }

    /** Returns the next commit. */
    public CommitChain next() {
        return next;
    }

    /** Returns the first commit in the chain. */
    public Commit head() {
        return current;
    }

    /** The current commit in the chain. */
    private Commit current;
    /** The next commit in the chain, its parent. */
    private CommitChain next;
}
