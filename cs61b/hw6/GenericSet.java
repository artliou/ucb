/** Generalized set that can store any Comparable item.
 *
 *  Implementation of this interface is optional for hw6.
 *  @author Josh Hug
 */
public interface GenericSet<T extends Comparable> {


    StringSet s = new BSTStringSet();

    /** Adds X the set. If it is already present in the set, do nothing. */

    public void put(T x) {
        if (contains(x)) {
            return;
        }

        last.next = new Node(x);
        last = last.next;
    }

    /** Returns true if X is in the set. */
    public boolean contains(T x) {
    //Should be recursive tree code
        for (Node x = front; x != null; x = x.next) {
            if (x.compareTo(x.s) == 0) {
                return true;
            }
        }
        return false;
    }
}