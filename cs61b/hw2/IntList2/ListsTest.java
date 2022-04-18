import org.junit.Test;
import static org.junit.Assert.*;

/** FIXME
 *
 *  @author FIXME
 */

public class ListsTest {
    /** FIXME
     */

    // It might initially seem daunting to try to set up
    // Intlist2 expected.
   	//
    // There is an easy way to get the IntList2 that you want in just
    // few lines of code! Make note of the IntList2.list method that
    // takes as input a 2D array.
    @Test
    public void testNaturalRuns() {
        IntList L = IntList.list((1, 3, 7, 5, 4, 6, 9, 10, 10, 11);
        IntList R = IntList.list((1, 3, 7), (5), (4, 6, 9, 10), (10, 11))
        IntList.naturalRuns(L);
        assertEquals(R, L);
    }

    @Test
    public void testNaturalRuns() {
        IntList L = IntList.list();
        IntList.naturalRuns(L);
        assertEquals(L, null);
    }

    @Test
    public void testRuns() {
        IntList L = IntList.list((1, 3, 7, 9, 10, 11);
        IntList.naturalRuns(L);
        assertEquals(IntList.list((1, 3, 7, 9, 10, 11), L);
    }

    public static void main(String[] args) {
        System.exit(ucb.junit.textui.runClasses(ListsTest.class));


//One "normal" input and a couple of corner cases is likely enough for 61B HW purposes!
    }
}
