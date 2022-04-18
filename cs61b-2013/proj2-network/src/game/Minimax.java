package game;

import player.MachinePlayer;
import player.Move;
import util.Vector;

public class Minimax {
	
	private Board ownBoard;
	private int color;
	private int maxDepth;
	
	public Minimax(Board b, int color, int depth){
		this.ownBoard = b;
		this.color = color;
		maxDepth = depth;
	}
	
	public Move search(int side){
		return search(side, maxDepth);
	}
	
	Move search(int side, int depth){
		return search(side, Integer.MIN_VALUE, Integer.MAX_VALUE, depth).move;
	}
	
	private Best search(int side, double alpha, double beta, int depth){
//		Best myBest = new Best(new Move(), Integer.MIN_VALUE);
		Best myBest;
		Best reply;
		
		Vector<Move> possibleMoves = ownBoard.getValidMoves(side);
		
		if(ownBoard.hasValidNetwork(MachinePlayer.otherColor(color))){
			return new Best(new Move(), Integer.MIN_VALUE + 1);
		}else if(ownBoard.hasValidNetwork(color)){
			return new Best(new Move(), Integer.MAX_VALUE - 1);
		}else if(possibleMoves.size() == 0){
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
				double score = ownBoard.evaluate(color, move);
				if (side == color) {
					if (score > myBest.score) {
						myBest = new Best(move, score);
					}
				}else{
					if(score < myBest.score){
						myBest = new Best(move, score);
					}
				}

				// myBest = new Best(move, 2);
//				ownBoard.undoMove(move);
			}
			return myBest;
		}
		
		
		for (Move move : possibleMoves) {
			ownBoard.doMove(move, side);
//			the following line is for debugging purposes, erase when complete
//			System.out.println("Analyzing move: " + move + " for player: "+ colorToString(side) + " " + " at a depth of " + (1 + maxDepth - depth));
			reply = search(MachinePlayer.otherColor(side), alpha, beta, depth -1);
			ownBoard.undoMove(move);
			
			if((side == MachinePlayer.otherColor(color)) && reply.score < myBest.score){
				myBest.move = move;
				if(reply.score < Integer.MIN_VALUE + depth + 1){
					myBest.score = reply.score + 1;
				}else{
					myBest.score = reply.score;
				}				
				beta = myBest.score;
			}else if ((side == color && reply.score > myBest.score)){
				myBest.move = move;
				if(reply.score < Integer.MAX_VALUE - depth - 1){
					myBest.score = reply.score -1;
				}else{
					myBest.score = reply.score;
				}	
				alpha = myBest.score;
			}
			if(alpha > beta){
				return myBest;
			}
		}
		return myBest;
	}

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		// TODO Auto-generated method stub

	}

}
