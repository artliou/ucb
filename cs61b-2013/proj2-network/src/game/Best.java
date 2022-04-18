package game;

import player.Move;

class Best {
	double score;
	Move move;
	
	Best(Move bestMove, double score){
		this.score = score;
		this.move = bestMove;
	}
	
}
