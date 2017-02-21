package loa;

import java.util.ArrayList;
import java.util.Iterator;

/** An automated Player.
 *  @author Arthur */
class MachinePlayer extends Player {
    /** Best Search-depth for Search Trees. **/
    protected static final int SEARCH_DEPTH = 2;

    /** Creates a new game. **/
    protected Game _game;

    /** A MachinePlayer that plays the SIDE pieces in GAME. */
    MachinePlayer(Piece side, Game game) {
        super(side, game);
    }

    /** makes the best possible move. **/
    @Override
    Move makeMove() {
        Move best = findBestMove(side(), SEARCH_DEPTH, Integer.MIN_VALUE,
            Integer.MAX_VALUE, getBoard());
        Piece color = side();
        System.out.println(color.abbrev().toUpperCase()
            + "::" + best.toString());
        return best;
    }

    /** Finds Best Legal Move - Game Tree!
     * @param side    The current player
     * @param depth   The number of moves we look ahead
     * @param alpha   The maximum value
     * @param beta    The minimum value
     * @param game    The current game being played
     * @return        Best Move
     */
    Move findBestMove(Piece side, int depth, int alpha, int beta, Board game) {
        Move bestMove = null;
        int best = Integer.MIN_VALUE;
        int next_best = Integer.MIN_VALUE;
        Board b = new Board(game);
        Iterator<Move> legalMoves = b.legalMoves();
        while (legalMoves.hasNext()) {
            Move next = legalMoves.next();
            b.makeMove(next);
            next_best = checkrest(side.opposite(), b, alpha, beta, depth - 1);
            if (next_best > best) {
                bestMove = next;
                best = next_best;
            }
            b.retract();
        }
        return bestMove;
    }

    /** Finds Best Legal Move down the AB tree.
     * @param side    The current player
     * @param game   The number of moves we look ahead
     * @param alpha   The maximum value
     * @param beta    The minimum value
     * @param depth   Depth Level
     * @return        Best Move
     */
    int checkrest(Piece side, Board game, int alpha, int beta, int depth) {
        if (game.gameOver() || depth < 1) {
            return 0;
        }
        Board b = new Board(game);
        Iterator<Move> legalMoves = b.legalMoves();
        ArrayList<Board> boards = new ArrayList<>();
        while (legalMoves.hasNext()) {
            b.makeMove(legalMoves.next());
            boards.add(b);
            b.retract();
        }
        if (game.turn().equals(side)) {
            for (Board b1 : boards) {
                int score = checkrest(side.opposite(), b1,
                    alpha, beta, depth - 1);
                if (score > alpha) {
                    alpha = score;
                }
                if (alpha >= beta) {
                    return alpha;
                }
            }
            return alpha;
        } else {
            for (Board b1 : boards) {
                int score = checkrest(side().opposite(), b1, alpha, beta,
                        depth - 1);
                if (score < beta) {
                    beta = score;
                }
                if (alpha >= beta) {
                    return beta;
                }
            }
            return beta;
        }
    }
}
