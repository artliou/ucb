/** Interface for a basic dictionary where words may have only one meaning.
 *
 *  @author Josh Hug
 */
public interface SimpleDictionary {
    /** Associates the given DEFINITION with the WORD. If the word already
      *  has a definition, overwrite it.
      */
    public void put(String word, String definition) {
        if (word(definition) != null) {
            word() = word(definition);
        }
        last.next = new Node(x);
        last = last.next;
    }

    /** Returns true if WORD is in this dictionary. */
    public boolean contains(String word) {
        for (Node x = front; x != null; x = x.next) {
            if (word.compareTo(x.s) == 0) {
                return true;
            }
        }
        return false;
      }
    /** Return the definition of WORD, or null if the word is not in
      * the dictionary.
      */
    public String get(String word) {
        for (Node x = front; x != null; x = x.next) {
            if (String.contains(String word)) {
                return word.definition;
            }
        }
        return null;
  }
}
