import org.junit.Test;
import static org.junit.Assert.*;
import java.util.Arrays;
import java.util.Collections;
import java.util.ArrayList;
import java.util.List;

/** HW #8, Problem 3.
 *  @author
  */
public class Intervals {
    /** Assuming that INTERVALS contains two-element arrays of integers,
     *  <x,y> with x <= y, representing intervals of ints, this returns the
     *  total length covered by the union of the intervals. */
    public static int coveredLength(List<int[]> intervals) {
        // REPLACE WITH APPROPRIATE STATEMENTS.
        int N = intervals.size();
        ArrayList<int[]> ends = new ArrayList<>();
        for (int[] i : intervals) {
            ends.add(new int[] { i[0], -1 } );
            ends.add(new int[] { i[1], 1 } );
        }
        Collections.sort(ends, new EndpointComparator());
        int n, start, total;
        n = total = 0;
        start = 0;
        for (int[] intvl : ends) {
            if (n == 0) {
                start = intvl[0];
            }
            n -= intvl[1];
            if (n == 0) {
                total += intvl[0] - start;
            }
        }
        return total;
    }
    
    /** Lexicographic ordering of 2-element arrays. */
    static class EndpointComparator implements Comparator<int[]> {
        @Override
        public int compare(int[] i0, int[] i1) {
            if (i0[0] == i1[0]) {
                return i0[1] - i1[1];
            } else {
                return i0[0] - i1[0];
            }
        }
    }
    /** Performs a basic functionality test on the stabbingCount method. */
    @Test
    public void basicTest() {
        int[][] intervals = {
            {19, 30},  {8, 15}, {3, 10}, {6, 12}, {4, 5},
        };
        assertEquals(23, coveredLength(Arrays.asList(intervals)));
    }

    /** Runs provided JUnit test. ARGS is ignored. */
    public static void main(String[] args) {
        System.exit(ucb.junit.textui.runClasses(Intervals.class));
    }

}
