package loa;

import ucb.junit.textui;
import org.junit.Test;
import static org.junit.Assert.*;

import static loa.Piece.BP;
import static loa.Piece.EMP;
import static loa.Piece.WP;

/** The suite of all JUnit tests for the loa package.
 *  @author Art
 */
public class UnitTest {

    /** Run the JUnit tests in the loa package. Add xxxTest.class entries to
     *  the arguments of runClasses to run other JUnit tests. */
    public static void main(String[] ignored) {
        textui.runClasses(UnitTest.class);
    }

    Piece[][] testBoard = {
        { EMP, EMP, BP, BP, EMP, BP, BP, EMP },
        { EMP, EMP, WP, EMP, EMP, EMP, EMP, WP },
        { WP, BP, EMP, EMP, BP, EMP, EMP, WP },
        { WP, EMP, EMP, EMP, EMP, EMP, EMP, WP },
        { WP, EMP, BP, EMP, EMP, WP, EMP, EMP },
        { EMP, EMP, EMP, WP, EMP, EMP, BP, WP },
        { WP, EMP, EMP, EMP, EMP, EMP, EMP, WP },
        { EMP, BP, EMP, BP, BP, BP, EMP, EMP }};

    /** Test get. */
    @Test
    public void boardGetTest() {
        Board b = new Board(testBoard, BP);
        Piece e3 = b.get(5, 3);
        assertEquals(BP, e3);
    }

    /** Test initialize. */
    @Test
    public void boardInitializeTest() {
        Board b = new Board();
        b.initialize(testBoard, BP);
        Piece e3 = b.get(5, 3);
        assertEquals(BP, e3);
    }

    /** Test the copyFrom method in the Board class. */
    @Test
    public void boardCopyFromTest() {
        Board a = new Board(testBoard, BP);
        Board b = new Board();

        Piece ae3 = a.get(5, 3);
        Piece be3 = b.get(5, 3);

        assertEquals(BP, ae3);
        assertEquals(EMP, be3);

        b.copyFrom(a);
        be3 = b.get(5, 3);
        assertEquals(BP, be3);
    }

    /** Test the piecesContiguous method in the Board class. */
    @Test
    public void testPiecesContiguous() {
        Piece[][] blackContiguous = {
            { EMP, BP, BP, BP, BP, BP, BP, EMP },
            { BP, EMP, EMP, EMP, EMP, EMP, EMP, BP },
            { BP, BP, EMP, EMP, EMP, EMP, BP, WP },
            { WP, EMP, EMP, EMP, EMP, EMP, BP, WP },
            { WP, EMP, EMP, EMP, EMP, EMP, EMP, WP },
            { WP, EMP, EMP, EMP, EMP, EMP, EMP, WP },
            { WP, EMP, EMP, EMP, EMP, EMP, EMP, WP },
            { EMP, EMP, EMP, EMP, EMP, EMP, EMP, EMP }};

        Piece[][] blackNoncontiguous = {
            { EMP, BP, BP, BP, BP, BP, BP, EMP },
            { BP, EMP, EMP, EMP, EMP, EMP, EMP, BP },
            { BP, BP, EMP, EMP, EMP, EMP, BP, WP },
            { WP, EMP, EMP, EMP, EMP, EMP, BP, WP },
            { WP, EMP, EMP, EMP, BP, EMP, EMP, WP },
            { WP, EMP, EMP, EMP, EMP, EMP, EMP, WP },
            { WP, EMP, EMP, EMP, EMP, EMP, EMP, WP },
            { EMP, EMP, EMP, EMP, EMP, EMP, EMP, EMP } };

        Board contiguous = new Board(blackContiguous, BP);
        Board nonContiguous = new Board(blackNoncontiguous, BP);

        assertEquals(true, contiguous.piecesContiguous(BP));
        assertEquals(false, nonContiguous.piecesContiguous(BP));
    }
}
