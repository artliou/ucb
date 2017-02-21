package canfield;

import ucb.gui.Pad;

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Rectangle;

import javax.imageio.ImageIO;

import java.io.InputStream;
import java.io.IOException;

/** A widget that displays a Pinball playfield.
 *  @author P. N. Hilfinger
 */
class GameDisplay extends Pad {

    /** Color of display field. */
    private static final Color BACKGROUND_COLOR = Color.blue;

    /* Coordinates and lengths in pixels unless otherwise stated. */

    /** Preferred dimensions of the playing surface. */
    private static final int BOARD_WIDTH = 700, BOARD_HEIGHT = 500;

    /** Displayed dimensions of a card image. */
    private static final int CARD_HEIGHT = 125, CARD_WIDTH = 90;

    /** A graphical representation of GAME. */
    public GameDisplay(Game game) {
        _game = game;
        setPreferredSize(BOARD_WIDTH, BOARD_HEIGHT);
    }

    /** Return an Image read from the resource named NAME. */
    private Image getImage(String name) {
        InputStream in =
            getClass().getResourceAsStream("/canfield/resources/" + name);
        try {
            return ImageIO.read(in);
        } catch (IOException excp) {
            return null;
        }
    }

    /** Return an Image of CARD. */
    private Image getCardImage(Card card) {
        return getImage("playing-cards/" + card + ".png");
    }

    /** Return an Image of the back of a card. */
    private Image getBackImage() {
        return getImage("playing-cards/blue-back.png");
    }

    /** Draw CARD at X, Y on G. */
    private void paintCard(Graphics2D g, Card card, int x, int y) {
        if (card != null) {
            g.drawImage(getCardImage(card), x, y,
                        CARD_WIDTH, CARD_HEIGHT, null);
        }
    }

    /** Draw card back at X, Y on G. */
    private void paintBack(Graphics2D g, int x, int y) {
        g.drawImage(getBackImage(), x, y, CARD_WIDTH, CARD_HEIGHT, null);
    }

    /** Clean Magic Numbers. **/
    private static final int TOP_CARD = 10, SIDE_CARD = 150,
        END_CARD = 300, START = 200;

    @Override
    public synchronized void paintComponent(Graphics2D g) {
        g.setColor(BACKGROUND_COLOR);
        Rectangle b = g.getClipBounds();
        g.fillRect(0, 0, b.width, b.height);

        paintCard(g, _game.topReserve(), TOP_CARD, SIDE_CARD);
        paintBack(g, TOP_CARD, END_CARD);
        paintCard(g, _game.topWaste(), CARD_HEIGHT, END_CARD);

        for (int i = 1; i < 5; i += 1) {
            paintCard(g, _game.topFoundation(i), START + 100 * i, TOP_CARD);
        }
        for (int i = 1; i < 5; i += 1) {
            paintCard(g, _game.topTableau(i), START + 100 * i, SIDE_CARD);
        }
    }

    /** Game I am displaying. */
    private final Game _game;

}
