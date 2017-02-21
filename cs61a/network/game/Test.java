package game;

import player.Move;

public class Test {
	public static void main(String[] args) {
		Board b = new Board();
		int BLACK = 0; int WHITE = 1;
		
		Move bl1 = new Move(5,2);
//		Move bl2 = new Move(5,5);
		Move bl3 = new Move(2,5);
//		Move bl4 = new Move(5,1);
//		Move bl5 = new Move(1,6);

		b.doMove(bl1, BLACK);
//		b.doMove(bl2, BLACK);
		b.doMove(bl3, BLACK);
//		b.doMove(bl4, BLACK);
//		b.doMove(bl5, BLACK);

//		Move w1 = new Move(2,4);
		Move w2 = new Move(2,2);
		Move w3 = new Move(4,2);
		Move w4 = new Move(4,4);
//		Move w5 = new Move(1,5);
//		Move w6 = new Move(4,5);

//		b.doMove(w1, WHITE);
		b.doMove(w2, WHITE);
		b.doMove(w3, WHITE);
		b.doMove(w4, WHITE);
//		b.doMove(w5, WHITE);
//		b.doMove(w6, WHITE);
		System.out.println(b);
		double maxEval = Double.MIN_VALUE;
		Move max = null;
		for(Move m: b.getValidMoves(BLACK)){
//			double score = b.evaluate(BLACK, m);
//			if(score > maxEval){
//				maxEval = score;
//				max = m;
//			}
		}
		System.out.println(maxEval);
		System.out.println(max);
//		Minimax m = new Minimax(b, BLACK, 3);
//		System.out.println(m.search(BLACK));
	}
}
