import java.util.LinkedList;

class ECHashStringSet extends BSTStringSet {   
	
	private static final int START = 10;
    private static final int MAX_LOAD = 5;   
    private LinkedList<String>[] stored;
    private int size;

    /** Creates a StringSet with the default number. */
    public ECHashStringSet() {
        this(START);
    }

    /** Creates a StringSet with num. */
    public ECHashStringSet(int num) {
        size = 0;
		stored = (LinkedList<String>[]) new LinkedList[num];
        for (int i = 0; i < num; i += 1) {
            stored[i] = new LinkedList<String>();
        }
    }

    /* Returns the size */
    public int size() {
        return size;
    }

	/* Put for StringSet */
    public void put(String s){
        size += 1;
        int stored_len = stored.length;

        if (size > stored_len * MAX_LOAD) {
            int new_len = stored_len * 5;
            resize(new_len);
        }

        int positiveHashcode = s.hashCode() & 0x7ffffff;
        int numbs = positiveHashcode % stored_len;
        LinkedList<String> sl = stored[numbs];
        if (!sl.contains(s)) {
            sl.add(s);
        }
    }

    /** Returns true if S is in the string set. */
    public boolean contains(String s) {
		int stored_len = stored.length;
        int numbs = s.hashCode() % stored_len;
        LinkedList<String> sl = stored[numbs];
        return sl.contains(s);
    }
    
    /** Resizes the list to NEWSIZE. */
    public void resize(int newSize) {
        int newsizes = size * 5;
        ECHashStringSet timer = new ECHashStringSet(newsizes);

        for (int i = 0; i < stored.length; i++) {
            for (String s : stored[i]) {
                timer.put(s);
            }
        }
        stored = timer.stored;
    }

}
