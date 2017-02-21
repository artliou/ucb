package canfield;

import static org.junit.Assert.*;
import org.junit.Test;

/** Tests of the Game class.
 *  @author
 */

public class GameTest {

    /** Example. */
    @Test
    public void testInitialScore() {
        Game g = new Game();
        g.deal();
        assertEquals(5, g.getScore());
    }
    /** Testing Undo. Execute a move, then save the top card as first. Then
        another move, then undo. And then resave the top card as second.
        Then compare first and second.
    */

    @Test
    public void testUndo() {
        Game g = new Game();
        g.deal();
        g.stockToWaste();
        Card first = g.topWaste();
        g.stockToWaste();
        g.undo();
        Card second = g.topWaste();
        assertEquals(first, second);
    }
}
