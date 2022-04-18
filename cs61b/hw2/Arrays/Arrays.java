/* NOTE: The file ArrayUtil.java contains some functions that may be useful
 * in testing your answers. */

/** HW #2, Problem #2. */

/** Array utilities.
 *  @author
 */
class Arrays {
    /* 2a. */
    /** Returns a new array consisting of the elements of A followed by the
     *  the elements of B. */
    static int[] catenate(int[] A, int[] B) {
        /* *Replace this body with the solution. */
        int i = 0;
        int[] r = new int[A.length + B.length]; //Create a new array with set length
        for (; i< A.length; i+=1) {
            r[i] = A[i]; //This is to put the first elements of A into the new elemnets of r
        }
        for (int k = 0; k < B.length; i += 1) {
            r[i] = B[k];
            k += 1;
        }
        return r; //Return the Array
    }

    /* 2b. */
    /** Returns the array formed by removing LEN items from A,
     *  beginning with item #START. */
    static int[] remove(int[] A, int start, int len) {
        /* *Replace this body with the solution. */
        if (len <= 0){
            return A;
        }
        if (A.length == 0){
            return A;
        }
        int num = 0;
        int k = 0;
        int[] b = new int[A.length - num];
        if (start + len > A.length){
            num = A.length - start;
        } else {
            num = len;
        }
        for (; k < start; k += 1){
            b[k] = A[k];
        }

        for (int i = start + len; i < A.length; i += 1) {
            b[k] = A[i];
        }

        return b;
    }
    /* 4 (optional). */
    /** Returns the array of arrays formed by breaking up A into
     *  maximal ascending lists, without reordering.
     *  For example, if A is {1, 3, 7, 5, 4, 6, 9, 10}, then
     *  returns the three-element array
     *  {{1, 3, 7}, {5}, {4, 6, 9, 10}}. */
   // static int[][] naturalRuns(int[] A) {
        /* *Replace this body with the solution. */
   /*     if (A.length == 0) return new int[0][];
        int k = 1;
        int [][]r;
        int t = 0;
        for (int i = 1; i < A.length; i ++){
            if (A[i] < A[i-1]) 1++;
        }
        r = new int[1][];
        k = 0;
        for (int i = 1; i < A.length; i ++){
            if (A[i] < A[i-1]) {
                r[k++] = Utils.subarray(A, t, i - t);
                t = i;
            }
        }
        if (k != r.length ){
            r[k] = Utils.subarray(A, t, A.length - t);
        }*/
//        return b;

}