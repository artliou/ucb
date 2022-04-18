/* MachinePlayer.java */

package player;

import util.Vector;
import evaluation.Evaluator;
import evaluation.Searcher;
import game.Board;

/**
 * An implementation of an automatic Network player. Keeps track of moves made
 * by both players. Can select a move for itself.
 */


public class MachinePlayer extends Player implements Evaluator, Searcher{
	
	private Board ownBoard; //internal board representation
    private int color;      //color it is playing
    private int moveCount;  // how many moves has it made
    private int searchDepth;// how deep its minimax searches
    private Move[] goodStartingMoves = {new Move(2,2), new Move(2,5), new Move(5,2), new Move(5,5)}; //good starting moves
	
	// Creates a machine player with the given color. Color is either 0 (black)
	// or 1 (white). (White has the first move.)
	public MachinePlayer(int color) {
		this(color, 2);
	}

	// Creates a machine player with the given color and search depth. Color is
	// either 0 (black) or 1 (white). (White has the first move.)
	public MachinePlayer(int color, int searchDepth) {
		moveCount = 0;
		ownBoard = new Board();
		this.color = color;
		this.searchDepth = searchDepth;
	}

	// Returns a new move by "this" player. Internally records the move (updates
	// the internal game board) as a move by "this" player.
	public Move chooseMove() {
		moveCount++;
		if(moveCount < 3){
			for (Move m : goodStartingMoves) {
				if(ownBoard.validateMove(m, color)){
					ownBoard.doMove(m, color);
					return m;
				}
			}
		}		
		
		Move toMove = search(ownBoard, color, searchDepth);
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
		moveCount++;
		return ownBoard.doMove(m, color);
	}
	
	/**
	 * evaluate
	 * given board, player turn, and a move, Machine Player will judge the move
	 * @param player - current player
	 * @param m - move to be judged
	 * @return
	 */
	public double evaluate(Board b, int player, Move m){
		int pathLength = b.longestPath(player);
		int otherPathLength = b.longestPath(otherColor(player));
		
		b.doMove(m, player);
		if(b.hasValidNetwork(MachinePlayer.otherColor(player))){
			b.undoMove(m);
			return Integer.MIN_VALUE + 1;
		}else if (b.hasValidNetwork(player)){
			b.undoMove(m);
			return Integer.MAX_VALUE - 1;
		}
		
		int newPathLength = b.longestPath(player);
		int newOtherPathLength = b.longestPath(MachinePlayer.otherColor(player));
		
		int increase = newPathLength - pathLength;
		int otherIncrease = newOtherPathLength - otherPathLength;
				
		b.undoMove(m);
		return (increase * 100.0) - (otherIncrease * 25.0);
	}
	
	/**
	 * Search
	 * 
     * performs a minimax recursive search on a given board at a specified depth
     * @param b - the current board to compute minimax on
     * @param color - the color of the person making a move
     * @param depth - maximum minimax recursion depth
     * @return the best Move to be made
     */
	public Move search(Board b, int color, int depth) {
		Best found = search(b, color, Integer.MIN_VALUE, Integer.MAX_VALUE, depth);
		return found.move;
	}
	
	/**
	 * Recursive method to perform minimax
	 * @param b - board to be computed on
	 * @param side - color of person making a move
	 * @param alpha, beta - pruning values
	 * @param depth - depth of recursive search
	 * @return best - the best move,value pair obtainable
	 */
	private Best search(Board b, int side, double alpha, double beta, int depth){
		Best myBest, reply;
		
		if(b.hasValidNetwork(MachinePlayer.otherColor(color))){
			return new Best(new Move(), Integer.MIN_VALUE + 1 + (searchDepth - depth));
		}else if(b.hasValidNetwork(color)){
			return new Best(new Move(), Integer.MAX_VALUE - 1 - (searchDepth - depth));
		}
		
		Vector<Move> possibleMoves = b.getValidMoves(side);
		if(possibleMoves.size() == 0){
			return new Best(new Move(), 0);
		}
		
		if(side == color){
			myBest = new Best(null, Integer.MIN_VALUE);
		}else{
			myBest = new Best(null, Integer.MAX_VALUE);
		}
		
		
		if (depth == 0) {
			for (Move move : possibleMoves) {
				// evaluate the move here (call the evaluation function)
				double score = evaluate(b, side, move);
				if (side == color) {
					if (score > myBest.score) {
						if(score > Integer.MAX_VALUE - 1 - searchDepth){
							myBest = new Best(move, Integer.MAX_VALUE - 1 - (searchDepth - depth));
						} else {
							myBest = new Best(move, score);
						}
					}
				}else{
					if(score > Integer.MAX_VALUE -1){
						score = -(score -1);
					}else{
						score = -score;
					}
					if(score < myBest.score){
						myBest = new Best(move, score);
					}
				}
			}
			return myBest;
		}
		
		
		for (Move move : possibleMoves) {
			ownBoard.doMove(move, side);
			reply = search(b, otherColor(side), alpha, beta, depth -1);
			ownBoard.undoMove(move);
			
			if((side == MachinePlayer.otherColor(color)) && reply.score < myBest.score){
				myBest.move = move;
				if(reply.score < Integer.MIN_VALUE - depth + 1){
					myBest.score = reply.score + 1;
				}else{
					myBest.score = reply.score;
				}				
				beta = myBest.score;
			}else if (side == color){
				if(reply.score > myBest.score){
					if(reply.score > Integer.MAX_VALUE - searchDepth){ //if its a mate in X
						if(reply.score >= Integer.MAX_VALUE - (searchDepth - depth) - 1){
							myBest.move = move;
							myBest.score = reply.score - 1;
						}
					} else {
						myBest.move = move;
						myBest.score = reply.score;
					}
				}
//			}
//				myBest.move = move;
//				if(reply.score > Integer.MAX_VALUE - depth - 1){
//					myBest.score = reply.score - 1;
//				}else{
//					myBest.score = reply.score;
//				}	
				alpha = myBest.score;
			}
			if(alpha > beta){
				return myBest;
			}
		}

		return myBest;
	}
	
	public static int otherColor(int color){
		return (color + 1) % 2;
	}
}
