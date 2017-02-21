package loa;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.Formatter;
import java.util.NoSuchElementException;

import java.util.regex.Pattern;

import static loa.Piece.*;
import static loa.Direction.*;

/** Represents the state of a game of Lines of Action.
 * @author Arthur Liou */
class Board implements Iterable<Move> {

    /** Size of a board. */
    static final int M = 8;

    /** Pattern describing a valid square designator (cr). */
    static final Pattern ROW_COL = Pattern.compile("^[a-h][1-8]$");

    /** A Board whose initial contents are taken from INITIALCONTENTS and in
     * which the player playing TURN is to move. The resulting Board has
     * get(col, row) == INITIALCONTENTS[row-1][col-1] Assumes that PLAYER is not
     * null and INITIALCONTENTS is MxM.
     *
     * CAUTION: The natural written notation for arrays initializers puts the
     * BOTTOM row of INITIALCONTENTS at the top. */
    Board(Piece[][] initialContents, Piece turn) {
        initialize(initialContents, turn);
    }

    /** A new board in the standard initial position. */
    Board() {
        clear();
    }

    /** A Board whose initial contents and state are copied from BOARD. */
    Board(Board board) {
        copyFrom(board);
    }

    /** Set my state to CONTENTS with SIDE to move. */
    void initialize(Piece[][] contents, Piece side) {
        _moves.clear();
        data = contents;
        for (int c = 1; c <= M; c += 1) {
            for (int r = 1; r <= M; r += 1) {
                set(c, r, data[r - 1][c - 1]);
            }
        }
        _turn = side;
    }

    /** Set me to the initial configuration. */
    void clear() {
        initialize(INITIAL_PIECES, BP);
    }

    /** Set my state to a copy of BOARD. */
    void copyFrom(Board board) {
        if (board == this) {
            return;
        }
        _moves.clear();
        _moves.addAll(board._moves);
        _turn = board._turn;
        data = board.data;
    }

    /** Return the contents of column C, row R, where 1 <= C,R <= 8, where
     * column 1 corresponds to column 'a' in the standard notation. */
    Piece get(int c, int r) {
        if (inBoard(r, c)) {
            return data[r - 1][c - 1];
        } else {
            return null;
        }
    }

    /** Return the contents of the square SQ. SQ must be the standard printed
     * designation of a square (having the form cr, where c is a letter from a-h
     * and r is a digit from 1-8). */
    Piece get(String sq) {
        return get(col(sq), row(sq));
    }

    /** Return the column number (a value in the range 1-8) for SQ. SQ is as for
     * {@link get(String)}. */
    static int col(String sq) {
        if (!ROW_COL.matcher(sq).matches()) {
            throw new IllegalArgumentException("bad square designator");
        }
        return sq.charAt(0) - 'a' + 1;
    }

    /** Return the row number (a value in the range 1-8) for SQ. SQ is as for
     * {@link get(String)}. */
    static int row(String sq) {
        if (!ROW_COL.matcher(sq).matches()) {
            throw new IllegalArgumentException("bad square designator");
        }
        return sq.charAt(1) - '0';
    }

    /** Set the square at column C, row R to V, and make NEXT the next side to
     * move, if it is not null. */
    void set(int c, int r, Piece v, Piece next) {
        data[r - 1][c - 1] = v;
        if (next != null) {
            _turn = next;
        }
    }

    /** Set the square at column C, row R to V. */
    void set(int c, int r, Piece v) {
        set(c, r, v, null);
    }

    /** Assuming isLegal(MOVE), make MOVE. */
    void makeMove(Move move) {
        assert isLegal(move);
        _moves.add(move);
        Piece replaced = move.replacedPiece();
        int c0 = move.getCol0(), c1 = move.getCol1();
        int r0 = move.getRow0(), r1 = move.getRow1();
        if (replaced != EMP) {
            set(c1, r1, EMP);
        }
        set(c1, r1, move.movedPiece());
        set(c0, r0, EMP);
        _turn = _turn.opposite();
    }

    /** Retract (unmake) one move, returning to the state immediately before
     * that move. Requires that movesMade () > 0. */
    void retract() {
        assert movesMade() > 0;
        Move move = _moves.remove(_moves.size() - 1);
        Piece replaced = move.replacedPiece();
        int c0 = move.getCol0(), c1 = move.getCol1();
        int r0 = move.getRow0(), r1 = move.getRow1();
        Piece movedPiece = move.movedPiece();
        set(c1, r1, replaced);
        set(c0, r0, movedPiece);
        _turn = _turn.opposite();
    }

    /** Return the Piece representing who is next to move. */
    Piece turn() {
        return _turn;
    }

    /** Return true iff MOVE is legal for the player currently on move. */
    boolean isLegal(Move move) {
        if (move == null) {
            return false;
        }
        if (move.length() != pieceCountAlong(move)) {
            return false;
        }
        if (blocked(move)) {
            return false;
        }
        int c = move.getCol0();
        int r = move.getRow0();
        if (get(c, r) != _turn) {
            return false;
        }
        return true;
    }

    /** Return a sequence of all legal moves from this position. */
    Iterator<Move> legalMoves() {
        return new MoveIterator();
    }

    @Override
    public Iterator<Move> iterator() {
        return legalMoves();
    }

    /** Return true if there is at least one legal move for the player on
     * move. */
    public boolean isLegalMove() {
        return iterator().hasNext();
    }

    /** Return true iff either player has all his pieces contiguous. */
    boolean gameOver() {
        return piecesContiguous(BP) || piecesContiguous(WP);
    }

    /** Return an array of the positions of pieces from one side.
     * @param side What side the piece is on
     * */
    ArrayList<Integer[]> locations(Piece side) {
        ArrayList<Integer[]> positions = new ArrayList<>();
        for (int row = 1; row <= M; row++) {
            for (int col = 1; col <= M; col++) {
                if (get(row, col) == side) {
                    Integer[] coordinate = new Integer[2];
                    coordinate[0] = row;
                    coordinate[1] = col;
                    positions.add(coordinate);
                }
            }
        }
        return positions;
    }

    /** Returns true if PIECE is adjacent.
     * @param piece  Piece we have
     * @param notcurr Not the current piece
     * @return       True if next to. */
    static boolean adjacent(Integer[] piece, ArrayList<Integer[]> notcurr) {
        for (Integer[] r : notcurr) {
            int x = Math.abs(piece[0] - r[0]);
            int y = Math.abs(piece[1] - r[1]);
            if (x <= 1 && y <= 1) {
                return true;
            }
        }
        return false;
    }

    /** Return true iff SIDE's pieces are contiguous. */
    boolean piecesContiguous(Piece side) {
        ArrayList<Integer[]> locations = locations(side);
        ArrayList<Integer[]> connected = new ArrayList<>();

        if (locations.size() > 1) {
            connected.add(locations.remove(0));
            boolean split;
            while (locations.size() != 0) {
                split = true;
                for (Integer[] p : locations) {
                    if (adjacent(p, connected)) {
                        connected.add(p);
                        locations.remove(p);
                        split = false;
                        break;
                    }
                }
                if (split) {
                    return false;
                }
            }
            return true;
        } else {
            return true;
        }
    }

    /** Return the total number of moves that have been made (and not
     * retracted). Each valid call to makeMove with a normal move increases this
     * number by 1. */
    int movesMade() {
        return _moves.size();
    }

    /** Checks each square of OBJ and this to see if pieces are the same. */
    @Override
    public boolean equals(Object obj) {
        Board b = (Board) obj;
        if (!_turn.equals(b.turn())) {
            return false;
        }
        for (int r = 1; r <= M; r += 1) {
            for (int c = 1; c <= M; c += 1) {
                if (!get(c, r).equals(b.get(c, r))) {
                    return false;
                }
            }
        }
        return true;
    }

    @Override
    public int hashCode() {
        return 0;
    }

    @Override
    public String toString() {
        Formatter out = new Formatter();
        out.format("===%n");
        for (int r = M; r >= 1; r -= 1) {
            out.format("    ");
            for (int c = 1; c <= M; c += 1) {
                out.format("%s ", get(c, r).abbrev());
            }
            out.format("%n");
        }
        out.format("Next move: %s%n===", turn().fullName().toLowerCase());
        return out.toString();
    }

    /** Returns TRUE if the square row R and column C is a square that
     *  exists. */
    public boolean inBoard(int c, int r) {
        return 1 <= c && c <= M && 1 <= r && r <= M;
    }

    /** Returns the pieces of row NUM.
     * @param num  Row number
     * @return     All items in row */
    public Piece[] getRow(int num) {
        Piece[] row = new Piece[M];
        for (int i = 0; i < M; i += 1) {
            row[i] = data[num - 1][i];
        }
        return row;
    }

    /** Returns the pieces of column NUM.
     * @param num    The number of the column
     * @return       All items in column
     */
    public Piece[] getColumn(int num) {
        Piece[] col = new Piece[M];
        for (int i = 0; i < M; i += 1) {
            col[i] = get(num, i + 1);
        }
        return col;
    }

    /** Return the pieces on the diagonal from row0 to row1 and col0
     * and col1. Pieces are listed from left to right.
     * @param row0   Indicates the row of the first part of the move
     * @param col0   Indicates the column of the first part of the move
     * @param row1     Indicates the row of the second part of the move
     * @param col1     Indicates the column of the second part of the move
     * @return           A Piece[] that holds the contents of the diagonal,
     *                   from left to right. */
    Piece[] diagonal(int row0, int col0, int row1, int col1) {
        boolean line = true;

        if (row0 > row1 && col0 < col1) {
            line = true;
        } else if (row0 < row1 && col0 > col1) {
            line = true;
        } else if (row0 > row1 && col0 > col1) {
            line = false;
        } else if (row0 < row1 && col0 < col1) {
            line = false;
        }

        int rowadd = -1;
        int coladd = 1;

        if (!line) {
            rowadd = 1;
            coladd = 1;
        }

        ArrayList<Piece> diag = new ArrayList<>();
        int dataRow = row0;
        int dataCol = col0;

        while (inBoard(dataCol, dataRow)) {
            dataCol -= coladd;
            dataRow -= rowadd;
        }

        dataCol += coladd;
        dataRow += rowadd;

        while (inBoard(dataCol, dataRow)) {
            diag.add(get(dataCol, dataRow));
            dataCol += coladd;
            dataRow += rowadd;
        }
        Piece[] rtn = diag.toArray(new Piece[diag.size()]);
        return rtn;
    }
    /** Return the number of pieces in the line of action indicated by MOVE. */
    private int pieceCountAlong(Move move) {
        int row0 = move.getRow0();
        int row1 = move.getRow1();
        int col0 = move.getCol0();
        int col1 = move.getCol1();

        int numbers = 0;
        if (row0 == row1) {
            for (Piece piece : getRow(row0)) {
                if (piece != EMP) {
                    numbers += 1;
                }
            }
        } else if (col0 == col1) {
            for (Piece piece : getColumn(col0)) {
                if (piece != EMP) {
                    numbers += 1;
                }
            }
        } else {
            for (Piece piece : diagonal(row0, col0, row1,
                    col1)) {
                if (piece != EMP) {
                    numbers += 1;
                }
            }
        }
        return numbers;
    }

    /** Return the number of pieces in the line of action in direction DIR and
     * containing the square at column C and row R. */
    private int pieceCountAlong(int c, int r, Direction dir) {
        int numbers = 0;
        if (!inBoard(c, r)) {
            return numbers;
        }
        if (dir.equals(N) || dir.equals(S)) {
            for (Piece piece : getColumn(c)) {
                if (piece != EMP) {
                    numbers += 1;
                }
            }
        } else if (dir.equals(E) || dir.equals(W)) {
            for (Piece piece : getRow(r)) {
                if (piece != EMP) {
                    numbers += 1;
                }
            }
        } else {
            int col = c;
            int row = r;
            int rowadd = -dir.dr;
            int coladd = dir.dc;
            while (inBoard(col, row)) {
                if (get(col, row) != EMP) {
                    numbers += 1;
                }
                col += coladd;
                row += rowadd;

            }
            col = c - coladd;
            row = r - rowadd;
            while (inBoard(col, row)) {
                if (get(col, row) != EMP) {
                    numbers += 1;
                }
                col -= coladd;
                row -= rowadd;
            }
        }
        return numbers;
    }

    /** Return true iff MOVE is blocked by an opposing piece or by a friendly
     * piece on the target square. */
    private boolean blocked(Move move) {
        Piece piece = move.movedPiece();
        int row0 = move.getRow0();
        int row1 = move.getRow1();
        int col0 = move.getCol0();
        int col1 = move.getCol1();
        Piece start = get(row0, col0);

        if (row0 == row1) {
            Piece[] row = getRow(row0);
            if (col0 < col1) {
                for (int i = col0; i < col1; i += 1) {
                    if (row[i - 1].equals(piece.opposite())) {
                        return true;
                    }
                }
            } else {
                for (int i = col0; i > col1; i -= 1) {
                    if (row[i - 1].equals(piece.opposite())) {
                        return true;
                    }
                }
            }
        } else if (col0 == col1) {
            Piece[] column = getColumn(col0);
            if (row0 < row1) {
                for (int i = row0; i < row1; i += 1) {
                    if (column[i - 1].equals(piece.opposite())) {
                        return true;
                    }
                }
            } else {
                for (int i = row0; i > row1; i -= 1) {
                    if (column[i - 1].equals(piece.opposite())) {
                        return true;
                    }
                }
            }
        } else if (Math.abs(col0 - col1) == Math
                .abs(row0 - row1)) {
            Piece[] diag = diagonal(row0, col0, row1, col1);
            if (col0 < col1) {
                for (int i = 0; i < col1 - col0; i += 1) {
                    if (diag[i].equals(piece.opposite())) {
                        return true;
                    }
                }
            } else {
                for (int i = col0 - col1 - 1; i > 0; i -= 1) {
                    if (diag[i].equals(piece.opposite())) {
                        return true;
                    }
                }
            }
        }
        return false;
    }

    /** The standard initial configuration for Lines of Action. */
    static final Piece[][] INITIAL_PIECES = {
            { EMP, BP, BP, BP, BP, BP, BP, EMP },
            { WP, EMP, EMP, EMP, EMP, EMP, EMP, WP },
            { WP, EMP, EMP, EMP, EMP, EMP, EMP, WP },
            { WP, EMP, EMP, EMP, EMP, EMP, EMP, WP },
            { WP, EMP, EMP, EMP, EMP, EMP, EMP, WP },
            { WP, EMP, EMP, EMP, EMP, EMP, EMP, WP },
            { WP, EMP, EMP, EMP, EMP, EMP, EMP, WP },
            { EMP, BP, BP, BP, BP, BP, BP, EMP } };

    /** List of all unretracted moves on this board, in order. */
    private final ArrayList<Move> _moves = new ArrayList<>();
    /** current side on move. */
    private Piece _turn;

    /** Returns the board.
     *  @return The current board. */
    Board currentBoard() {
        return this;
    }

    /** Returns a Piece[][] of the current board. */
    Piece[][] data() {
        return data;
    }

    /** The current state of the board. */
    private Piece[][] data = { { EMP, BP, BP, BP, BP, BP, BP, EMP },
        { WP, EMP, EMP, EMP, EMP, EMP, EMP, WP },
        { WP, EMP, EMP, EMP, EMP, EMP, EMP, WP },
        { WP, EMP, EMP, EMP, EMP, EMP, EMP, WP },
        { WP, EMP, EMP, EMP, EMP, EMP, EMP, WP },
        { WP, EMP, EMP, EMP, EMP, EMP, EMP, WP },
        { WP, EMP, EMP, EMP, EMP, EMP, EMP, WP },
        { EMP, BP, BP, BP, BP, BP, BP, EMP } };

    /** An iterator returning the legal moves from the current board. */
    private class MoveIterator implements Iterator<Move> {
        /** current piece under consideration. */
        private int _c, _r;
        /** Next direction of current piece to return. */
        private Direction _dir;
        /** Next move. */
        private Move _move;
        /** The board we're using. */
        private Board _board = currentBoard();

        /** A new move iterator for turn(). */
        MoveIterator() {
            _c = 1;
            _r = 1;
            _dir = Direction.NOWHERE;
            _board = currentBoard();
            incr();
        }

        @Override
        public boolean hasNext() {
            return _move != null;
        }

        @Override
        public Move next() {
            if (_move == null) {
                throw new NoSuchElementException("no legal move");
            }

            Move move = _move;
            incr();
            return move;
        }

        @Override
        public void remove() {
        }

        /** Advance to the next legal move. */
        private void incr() {
            _dir = _dir.succ();
            if (_dir == null) {
                _c += 1;
                _dir = NOWHERE;
                incr();
            } else if (get(_c, _r) == null) {
                if (_c > M) {
                    _c = 1;
                    _r += 1;
                    _dir = NOWHERE;
                    incr();
                }
                if (_r > M) {
                    _move = null;
                    return;
                }
            } else if (!get(_c, _r).equals(_turn)) {
                _c += 1;
                _dir = NOWHERE;
                incr();
            } else if (get(_c, _r).equals(_turn)) {
                int numSteps = pieceCountAlong(_c, _r, _dir);
                Move temp = Move.create(_c, _r, numSteps, _dir, _board);
                if (isLegal(temp)) {
                    _move = temp;
                    return;
                } else {
                    incr();
                }
            }
        }
    }

}
