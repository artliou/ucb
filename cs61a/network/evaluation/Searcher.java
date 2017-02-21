package evaluation;

import game.Board;
import player.Move;

public interface Searcher {
	/**
     * minimax
     * performs a recursive search on a given board at a specified depth
     * @param b - the current board to compute minimax on
     * @param color - the color of the person making a move
     * @param depth - maximum minimax recursion depth
     * @return the best Move to be made
     */
     public Move search(Board b, int color, int depth);
}
