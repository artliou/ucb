/** Interface for a basic String set.
  * @author Josh Hug */
public interface StringSet {
    /** Adds the string S to the string set. If it is already present in the
      * set, do nothing.
      */
    public void put(String s) {
        if (contains(s)) {
            return;
        }
        last.next = new Node(s);
        last = last.next;
    }
    
    /** Returns true if S is in the string set. */
    public boolean contains(String s) {
        for (Node x = front; x != null; x = x.next) {
            if (x.compareTo(x.s) == 0) {
                return true;
            }
        }
        return false;
}

}