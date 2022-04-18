/** Functions to increment and sum the elements of a WeirdList. */


//Fill in other classes here


class WeirdListClient {

    /** Return the result of adding N to each element of L. */
    static WeirdList add(WeirdList L, int n) {
        Adder adder = new Adder(n);
        return L.map(adder); // REPLACE THIS LINE WITH THE RIGHT ANSWER.
    }

    /** Return the sum of the elements in L. */
    static int sum(WeirdList L) {

        Summer summer = new Summer();
        L.map(summer);
        return summer.getS(); // REPLACE THIS LINE WITH THE RIGHT ANSWER.
    }

    /* As with WeirdList, you'll need to add an additional class or
     * perhaps more for WeirdListClient to work. Again, you may put
     * those classes either inside WeirdListClient as private static
     * classes, or in their own separate files.

     * You are still forbidden to use any of the following:
     *       if, switch, while, for, do, try, or the ?: operator.
     */
}
