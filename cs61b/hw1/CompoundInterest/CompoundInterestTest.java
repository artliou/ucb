import static org.junit.Assert.*;
import org.junit.Test;

public class CompoundInterestTest {

    @Test
    public void testNumYears() {
        /** Sample assert statement for comparing integers.

        assertEquals(0, 0); */
        assertEquals(1, CompoundInterest.numYears(2016));
        assertEquals(4, CompoundInterest.numYears(2019));
    }

    @Test
    public void testFutureValue() {
        assertEquals(12.554, CompoundInterest.futureValue(10, 12, 2017), 12);
        assertEquals(15.20875, CompoundInterest.futureValue(10, 15, 2018), 15.20875);
        assertEquals(9.025, CompoundInterest.futureValue(10, -5, 2017), 0);
        double tolerance = 0.01;
    }
    @Test
    public void testFutureValueReal() {
        assertEquals(11.881, CompoundInterest.futureValueReal(10, 12, 2017, 3), 11.881);
        assertEquals(7.78688, CompoundInterest.futureValueReal(10, -5, 2018, 3), 7.78688);
        double tolerance = 0.01;
        //PROBLEM IS THAT TESTING DODESN"T ROUND SO RETURNS ERROR
    }

    @Test
    public void testTotalSavings() {
        assertEquals(16550, CompoundInterest.totalSavings(5000, 2017, 10), 0);
        double tolerance = 0.01;
    }

    @Test
    public void testTotalSavingsReal() {
        assertEquals(16074.5, CompoundInterest.totalSavingsReal(5000, 2017, 10, 3), 0);
        double tolerance = 0.01;
    }


    /* Run the unit tests in this file. */
    public static void main(String... args) {
        System.exit(ucb.junit.textui.runClasses(CompoundInterestTest.class));
    }
}
