/** HW #8, Optional Problem 5a.
 *  @author
 */
public class SortInts {

    /** Sort A into ascending order.  Assumes that 0 <= A[i] < n*n for all
     *  i, and that the A[i] are distinct. */
    static void sort(long[] A) {
        // FILL IN
        int k = A.length;
		int N = Math.min(k, A.length);
		for (int i = 0; i < N; i++) {
			for (int j = i; j > 0 && array[j] < array[j-1]; j--) {
				swap(array, j, j-1);
                }                
            }
    }

}

