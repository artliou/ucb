package evaluation;

import game.Board;
import player.Move;

public interface Evaluator {
	/**
	 * Determines how good a move is for a player given a board
	 * @param b - current board
	 * @param m - the Move to be judged
	 * @param color - the given player
	 * @return value in range (?, ?)
	 */
	public double evaluate(Board b, int color, Move m);
}
