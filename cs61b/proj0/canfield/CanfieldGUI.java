package canfield;

import ucb.gui.TopLevel;
import ucb.gui.LayoutSpec;

import java.awt.event.MouseEvent;

/** A top-level GUI for Canfield solitaire.
 *  @author Arthur Liou
 */
class CanfieldGUI extends TopLevel {

    /** A new window with given TITLE and displaying GAME. */
    CanfieldGUI(String title, Game game) {
        super(title, true);
        _game = game;
        addLabel("Sorry, no graphical interface yet",
                 new LayoutSpec("y", 0, "x", 0));
        addButton("Quit", "quit", new LayoutSpec("y", 0, "x", 1));

        _display = new GameDisplay(game);
        add(_display, new LayoutSpec("y", 2, "width", 2));
        _display.setMouseHandler("click", this, "mouseClicked");
        _display.setMouseHandler("release", this, "mouseReleased");
        _display.setMouseHandler("drag", this, "mouseDragged");

        display(true);
    }

    /** Respond to "Quit" button. */
    public void quit(String dummy) {
        System.exit(1);
    }

    /** Creates first click locations. */
    private boolean firstclick = true;
    /** Creates  second click locations. */
    private boolean secondclick = false;
    /** Saves first click location. **/
    private int a, b;
    /*
    Waste 0
    Reserve 1
    Stock 2
    Foundation 3-6
    Tableau 7-10
    */

    /** Waste Location. **/
    public static final double WASTE = 0;
    /** RESERVE Location. **/
    public static final double RESERVE = 1;
    /** STOCK Location. **/
    public static final double STOCK = 2;
    /** Foundation1 Location. **/
    public static final double FOUNDATION_1 = 3;
    /** Foundation2 Location. **/
    public static final double FOUNDATION_2 = 4;
    /** Foundation3 Location. **/
    public static final double FOUNDATION_3 = 5;
    /** Foundation4 Location. **/
    public static final double FOUNDATION_4 = 6;
    /** TABLEAU1 Location. **/
    public static final double TABLEAU_1 = 7;
    /** TABLEAU2 Location. **/
    public static final double TABLEAU_2 = 8;
    /** TABLEAU3 Location. **/
    public static final double TABLEAU_3 = 9;
    /** TABLEAU4 Location. **/
    public static final double TABLEAU_4 = 10;

    /** Action in response to mouse-clicking event EVENT. */
    public synchronized void mouseClicked(MouseEvent event) {

        int x = event.getX();
        int y = event.getY();

        if (firstclick) {
            if (area(x, y) == STOCK) {
                _game.stockToWaste();
                firstclick = !firstclick;
                secondclick = !secondclick;
                _display.repaint();
            }
            a = x;
            b = y;
            firstclick = !firstclick;
            secondclick = !secondclick;
        } else {
            if (area(a, b) == WASTE && area(x, y) >= 3 && area(x, y) <= 6) {
                _game.wasteToFoundation();
            }
            if (area(a, b) == RESERVE && area(x, y) >= 7 && area(x, y) <= 10) {
                _game.reserveToFoundation();
            }
            if (area(a, b) >= 7 && area(a, b) <= 10
                && area(x, y) >= 7 && area(x, y) <= 10) {
                _game.tableauToFoundation(area(a, b) - 6);
            }
            if (area(a, b) >= 7 && area(a, b) <= 10
                && area(x, y) >= 7 && area(x, y) <= 10) {
                _game.tableauToTableau((area(a, b) - 6), (area(x, y) - 6));
            }
            if (area(a, b) >= 3 && area(a, b) <= 6
                && area(x, y) >= 7 && area(x, y) <= 10) {
                _game.foundationToTableau((area(a, b) - 2), (area(x, y) - 6));
            }
            if (area(a, b) == WASTE && area(x, y) >= 7 && area(x, y) <= 10) {
                _game.wasteToTableau(area(x, y) - 6);
            }
            if (area(a, b) == RESERVE && area(x, y) >= 7 && area(x, y) <= 10) {
                _game.reserveToTableau(area(x, y) - 6);
            }

            firstclick = !firstclick;
            secondclick = !secondclick;
            _display.repaint();
        }
    }

    /** Creates a helper method to determine where clicks are.
 @param x This is the x-type parameter
 @param y This is the y-Axis parameter
 @return <-1> This is a random number.
    **/
    public int area(int x, int y) {

        if (x >= 125 && x <= 250 && y >= 300 && y <= 425) {
            return 0;
        }
        if (x >= 10 && x <= 100 && y >= 150 && y <= 275) {
            return 1;
        }
        if (x >= 10 && x <= 100 && y >= 300 && y <= 425) {
            return 2;
        }
        if (x >= 300 && x <= 390 && y >= 10 && y <= 135) {
            return 3;
        }
        if (x >= 400 && x <= 490 && y >= 10 && y <= 135) {
            return 4;
        }
        if (x >= 500 && x <= 490 && y >= 10 && y <= 135) {
            return 5;
        }
        if (x >= 600 && x <= 590 && y >= 10 && y <= 135) {
            return 6;
        }
        if (x >= 300 && x <= 390 && y >= 150 && y <= 275) {
            return 7;
        }
        if (x >= 400 && x <= 490 && y >= 150 && y <= 275) {
            return 8;
        }
        if (x >= 500 && x <= 590 && y >= 150 && y <= 275) {
            return 9;
        }
        if (x >= 600 && x <= 690 && y >= 150 && y <= 275) {
            return 10;
        }
        return -1;
    }


    /** Action in response to mouse-released event EVENT. */
    public synchronized void mouseReleased(MouseEvent event) {
        /*FIXME*/
        _display.repaint();
    }

    /** Action in response to mouse-dragging event EVENT. */
    public synchronized void mouseDragged(MouseEvent event) {
        /*FIXME*/
        /* Not needed if picture does not change. */

        _display.repaint();
    }

    /** The board widget. */
    private final GameDisplay _display;

    /** The game I am consulting. */
    private final Game _game;
}
