import org.junit.Test;
import static org.junit.Assert.*;

/** FIXME
 *  @author FIXME
 */

public class ArraysTest {
    /** FIXME
     */

        public static void main(String[] args) {
        int[] test = {1, 3, 7, 5, 4, 6, 9, 10};
        System.out.println(Lists.naturalRuns(IntList.list(test)));
        
        int[] empty = {}; //empty set
        int[] a = {1, 2, 3};
        int[] b = {4, 5, 6};
        int[] c = {1, 2, 3, 4, 5, 6};

        Utils.print(Arrays.catenate(a, b));
        System.out.println();

        Utils.print(Arrays.catenate(a, empty));
        System.out.println();

        Utils.print(Arrays.catenate(empty, empty));
        System.out.println();

        Utils.print(Arrays.catenate(empty, a));
        System.out.println();



        Utils.print(Arrays.remove(c, 2, 4));
        System.out.println();

        Utils.print(Arrays.remove(c, 1, 7));
        System.out.println();

        Utils.print(Arrays.remove(c, 2, 0));
        System.out.println();

        Utils.print(Arrays.remove(empty, 3, 2));
        System.out.println();


    public static void main(String[] args) {
        System.exit(ucb.junit.textui.runClasses(ArraysTest.class));
    }
}
/*
 *  * int[] B; ... B = Utils.readIntArray();
 *       will, for example, read the input
 *          [ 1, 2, 3 ]
 *       from System.in and set B to point to the 3-element array
 *       whose elements are 1, 2, and 3.
 *
 *  * int[][] B; ... B = Utils.readIntArray2();
 *       reads a 2-D array (array of array of ints) from System.in,
 *       in the form
 *           [ [ 1, 2, 3 ], [ 0, 12, 14 ], [ 42 ] ]


 *  * Utils.equals (A, B), where A and B are 1- or 2-D arrays of ints
 *       is true iff both represent the same sequences of values.
*/