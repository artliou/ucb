/* NOTE: The file Utils.java contains some functions that may be useful
 * in testing your answers. */

/** HW #2, Problem #1. */

/** List problem.
 *  @author
 */
class Lists {
    /** Return the list of lists formed by breaking up L into "natural runs":
     *  that is, maximal strictly ascending sublists, in the same order as
     *  the original.  For example, if L is (1, 3, 7, 5, 4, 6, 9, 10, 10, 11),
     *  then result is the four-item list ((1, 3, 7), (5), (4, 6, 9, 10), (10, 11)).
     *  Destructive: creates no new IntList items, and may modify the
     *  original list pointed to by L. */
    static IntList2 naturalRuns(IntList L) {
        /* *Replace this body with the solution. */
        if (L == null) {
            return null;
        }

        IntList rest = null;
        IntList head = L;
        IntList start = L;

        for (int t = L.head; L != null && t <= L.head; L = L.tail) {
            t = L.head;
            rest = L.tail;
            head = L;

            if (L.tail != null && t == L.tail.head) {
                break;
            }
        }
        if (head != null) {
            head.tail = null;
        }
        return new IntList2(start, naturalRuns(rest));
    }
}
/*
        int i = L.head;
        IntList a, b;
        IntList c, d;
        a = b = L;
        c = d = new IntList(L, null);
        for (;a.tail != null;) {
            if (i < a.tail.head;){
                a = a.tail;
                i = a.head;
            }
            else {
                b = a.tail;
                a.tail = null;
                a = b;
                i = a.head;
                a.tail = new IntList(b, null);
                d = d.tail;
            }
        }

        return c;
    }
}*/