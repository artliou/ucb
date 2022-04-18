/* MachinePlayer.java */

package player;

import evaluation.Evaluator;
import evaluation.Searcher;
import game.Board;
import game.Minimax;

/**
 * An implementation of an automatic Network player. Keeps track of moves made
 * by both players. Can select a move for itself.
 */


public class MachinePlayer extends Player implements Evaluator, Searcher {

	
	Board ownBoard;
	Minimax searcher;
	int color;
	// Creates a machine player with the given color. Color is either 0 (black)
	// or 1 (white). (White has the first move.)
	public MachinePlayer(int color) {
		this(color, 3);
	}

	// Creates a machine player with the given color and search depth. Color is
	// either 0 (black) or 1 (white). (White has the first move.)
	public MachinePlayer(int color, int searchDepth) {
		ownBoard = new Board();
		this.color = color;
		searcher = new Minimax(ownBoard, color, searchDepth);
	}

	// Returns a new move by "this" player. Internally records the move (updates
	// the internal game board) as a move by "this" player.
	public Move chooseMove() {
		Move toMove = searcher.search(color);
		ownBoard.doMove(toMove, color);
		return toMove;
	}

	// If the Move m is legal, records the move as a move by the opponent
	// (updates the internal game board) and returns true. If the move is
	// illegal, returns false without modifying the internal state of "this"
	// player. This method allows your opponents to inform you of their moves.
	public boolean opponentMove(Move m) {
		return ownBoard.doMove(m, otherColor(color));
	}

	// If the Move m is legal, records the move as a move by "this" player
	// (updates the internal game board) and returns true. If the move is
	// illegal, returns false without modifying the internal state of "this"
	// player. This method is used to help set up "Network problems" for your
	// player to solve.
	public boolean forceMove(Move m) {
		return ownBoard.doMove(m, color);
	}
	
	public static int otherColor(int color){
		return (color + 1) % 2;
	}

	@Override
	public Move search(Board b, int color, int depth) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public double evaluate(Board b, int color) {
		// TODO Auto-generated method stub
		return 0;
	}

}
