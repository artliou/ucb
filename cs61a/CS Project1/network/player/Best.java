package player;

/**
 * Best
 * dummy class to hold a pair of values, Move and its corresponding score
 * to be used in Minimax search
 */
class Best {
	double score;
	Move move;
	
	Best(Move bestMove, double score){
		this.score = score;
		this.move = bestMove;
	}
	
}
