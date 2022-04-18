package evaluation;

import game.Board;

public interface Evaluator {
	/**
	 * Determines how likely the given player is to win in the current board
	 * @param b - current board
	 * @param color - the given player
	 * @return value in range (?, ?)
	 */
	public double evaluate(Board b, int color);
}
