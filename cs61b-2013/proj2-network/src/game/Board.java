package game;

import player.MachinePlayer;
import player.Move;
import util.Vector;

public class Board{
	
	final static int WHITE = 1;
	final static int BLACK = 0;
	final static int EMPTY = -1;
	final static int SIZE = 8;
	final static int MAXPIECES = 10;
	private Piece[][] grid;
	private Vector<Piece> whitePieces;
	private Vector<Piece> blackPieces;
	
	
	/**
	 * Create a new Board
	 */
	public Board() {
		// TODO Auto-generated constructor stub
		grid = new Piece[SIZE][SIZE];
		whitePieces = new Vector<Piece>();
		blackPieces = new Vector<Piece>();
	}
	
    /**
     * validateMove
     * checks whether any given move is valid on this board
     * @param m - the move to be made
     * @param b - the board that the move is being on
     * @return true if the move is valid, false otherwise
     */
    public boolean validateMove(Move m, int color){
    	if(m.moveKind == Move.QUIT)
    		return true;
//    	check for adding out of bounds
    	if(m.x1 < 0 || m.x1 >= SIZE || m.y1 < 0 || m.y1 >= SIZE){
    		return false;
    	}
//    	check for adding in corners
    	if (m.x1 == 0 && (m.y1 == 0 || m.y1 == SIZE -1) || m.x1 == SIZE-1 && (m.y1 == 0 || m.y1 == SIZE -1)){
    		return false;
    		
//    	check for moving a piece that does not belong to given player
    	}else if (m.moveKind == Move.STEP && (grid[m.x2][m.y2] == null || grid[m.x2][m.y2].color != color)){
    		return false;    		
    	}
    	
//    	check for moving a piece to its own current square
    	if(m.moveKind == Move.STEP && m.x1 == m.x2 && m.y1 == m.y2){
    		return false;
    	}
    	
//    	check if there is already a piece there
    	else if (grid[m.x1][m.y1] != null){
    		return false;
    	}
    	
    	Vector<Piece> getColorPieces = getColorPieces(color);
//    	check if adding piece when player already has MAXPIECES pieces
    	if (getColorPieces.size() == MAXPIECES && m.moveKind == Move.ADD){
    		return false;
    	}
    	
//    	check for adding pieces in opposite color goal
    	if (color == BLACK && (m.x1 == 0 || m.x1 == SIZE-1)){
    		return false;
    	}else if (color == WHITE && (m.y1 == 0 || m.y1 == SIZE -1)){
    		return false;
    	}
    	
//    	check for neighboring pieces 
    	
    	if(m.moveKind == Move.ADD && searchNeighborChain(color, m.x1, m.y1, null, null) > 1){
    		return false;
    	}else if (m.moveKind == Move.STEP && searchNeighborChain(color, m.x1, m.y1, grid[m.x2][m.y2], null) > 1){
    		return false;
    	}    	
    	
    	return true;
    }
    
    
    /**
     * returns a number that represents the number of neighbors to a piece (of the same color)
     * @param color - color of piece
     * @param x, y - location of piece on board
     * @param exclude, exclude2 - pieces to exclude from count
     * @return - number of pieces that are same color and neighbors (or neighbors of neighbors, etc.)
     * **note - this number may not be correct for #neighbors > 2 - may just return 2
     */
    private int searchNeighborChain(int color, int x, int y, Piece exclude, Piece exclude2){
		int xStart = x - 1, xEnd = x + 1, yStart = y - 1, yEnd = y + 1;
		int total = 0;
		
		switch (x) {
		case 0:
			xStart++;
		case SIZE - 1:
			xEnd--;
		}
    	
    	switch (y){
    	case 0:
    		yStart ++;
    	case SIZE -1:
    		yEnd --;
    	}

		for (int i = xStart; i <= xEnd; i++) {
			for (int j = yStart; j <= yEnd; j++) {
				if (total > 1){
					return total;
				}
				if (grid[i][j] == null || i == x && y == j) {
					continue;
				} else if (grid[i][j].color == color && !grid[i][j].equals(exclude) && !grid[i][j].equals(exclude2)) {
					total++;
					total += searchNeighborChain(color, i, j, grid[x][y], exclude);
				}
			}
		}
    	
    	return total;
    }
    
    /**
     * getValidMoves
     * gathers a list of moves that are legal on this board
     * @param color - the color of whose turn it is to make a move
     * @return all the valid moves for a given position and person's turn
     */
    public Vector<Move> getValidMoves(int color){
    	Vector<Move> moves = new Vector<Move>();
    	Vector<Piece> getColorPieces = this.getColorPieces(color);
    	Move test;
		if (getColorPieces.size() == MAXPIECES) {
			// can only do moving moves, no add
			for (Piece piece : getColorPieces) {
				for (int i = 0; i < SIZE; i++) {
					for (int j = 0; j < SIZE; j++) {
						test = new Move(i, j, piece.x, piece.y);
						if (validateMove(test, color)) {
							moves.add(test);
						}
					}
				}
			}
    		
    	}else{
//    		can only do adding moves, no location change moves
    		for (int i = 0; i < SIZE; i++) {
				for (int j = 0; j<SIZE; j++){
					test = new Move(i,j);
					if (validateMove(test, color)){
						moves.add(test);
					}
				}				
			}    		
    	}
    	return moves;
    }

    public boolean doMove(Move m, int color){
    	if(validateMove(m, color)){
    		if(m.moveKind == Move.ADD){
    			addPiece(m.x1, m.y1, color);
    		}else if (m.moveKind == Move.STEP){
    			movePiece(m.x2, m.y2, m.x1, m.y1);
    		}
    		return true;
    	}
    	return false;
    }
    
//    please only use this method for undoing the LAST move. Otherwise, anything can happen..
    void undoMove(Move m){
    	try{
    	if(m.moveKind == Move.ADD){
    		removePiece(m.x1, m.y1);
    	}else if (m.moveKind == Move.STEP){
    		movePiece(m.x1, m.y1, m.x2, m.y2);
    	}
    	}catch (Exception e){
    		System.out.println();
    	}
    }
    
    
	/**
	 * adds a piece of the given color to the board, with coord x and y
	 * @param x,y - coordinates to add the piece to
	 * @param color - of the piece being added
	 */
	void addPiece(int x, int y, int color){
		if(!isCoordinateOnGrid(x,y) || grid[x][y] != null)
			return;
		
		Piece toAdd = new Piece(x, y, color);
		grid[x][y] = toAdd;
		getColorPieces(color).add(toAdd);
		
		for(int dir = 0 /*From North*/; dir <= 3 /*to SOUTHEAST */; dir++){
			Piece a = findPieceInDirection(x,y,dir);
			Piece b = findPieceInDirection(x,y,Direction.getOpposite(dir));
			
			//casework for building/breaking connections
			if(a == null && b == null){			// both null
				continue;
			} else if(a == null || b == null){  // exactly one piece exists
				Piece exists = (a != null) ? a : b;
				if(exists.color == toAdd.color){
					toAdd.connections.add(exists);
					exists.connections.add(toAdd);
				}
			} else { 							// both pieces exists
				if(a.color == b.color){ // need to break a connection
					a.connections.remove(b); b.connections.remove(a);
					if(a.color == toAdd.color){
						a.connections.add(toAdd); b.connections.add(toAdd);
						toAdd.connections.add(a); toAdd.connections.add(b);
					}
				} else { 				// create a connection with only one piece
					Piece sameColor = (a.color == toAdd.color) ? a: b;
					sameColor.connections.add(toAdd); toAdd.connections.add(sameColor);
				}
			}
		}
	}
	
	private Piece findPieceInDirection(int x, int y, int dir){
		int dx = Direction.relativeDirections[dir][0];
		int dy = Direction.relativeDirections[dir][1];
	
		x += dx; y += dy;
		while(isCoordinateOnGrid(x, y)){
			if(grid[x][y] != null)
				return grid[x][y];
			x += dx; y += dy;
		}
		//found nothing
		return null;
	}
	
	private boolean isCoordinateOnGrid(int x, int y){
//    	check for adding out of bounds
    	if(x < 0 || x >= SIZE || y < 0 || y >= SIZE){
    		return false;
    	}
//    	check for adding in corners
    	if (x == 0 && (y == 0 || y == SIZE -1) || x == SIZE-1 && (y == 0 || y == SIZE -1))
    		return false;
    	
    	//else
    	return true;
	}
	
	private Vector<Piece> getColorPieces(int color){
		if(color == WHITE)
			return whitePieces;
		else
			return blackPieces;
	}
	/**
	 * moves a piece from x0, y0 to x1, y1
	 * @param x0, y0 - from coordinates 
	 * @param x1, y1 - to coordinates
	 */
	void movePiece(int x0, int y0, int x1, int y1){
		int color = grid[x0][y0].color;
		removePiece(x0, y0);
		addPiece(x1, y1, color);
	}
	
	/**
	 * removes the piece at the given location
	 * @param x, y - coordinates of which piece to remove
	 */
	void removePiece(int x, int y){
		if(!isCoordinateOnGrid(x,y) || grid[x][y] == null)
			return;
		
		Piece toRemove = grid[x][y];
		for(int dir = 0 /*From North*/; dir <= 3 /*to SOUTHEAST */; dir++){
			Piece a = findPieceInDirection(x,y,dir);
			Piece b = findPieceInDirection(x,y,Direction.getOpposite(dir));
			
			//casework for building/breaking connections
			if(a == null && b == null){			// both null
				continue;
			} else if(a == null || b == null){  // exactly one piece exists
				Piece exists = (a != null) ? a : b;
				if(exists.color == toRemove.color){
//					toRemove.connections.add(exists);
					exists.connections.remove(toRemove);
				}
			} else { 							// both pieces exists
				if(a.color == b.color){ // need to break a connection
					a.connections.add(b); b.connections.add(a);
					if(a.color == toRemove.color){
						a.connections.remove(toRemove); b.connections.remove(toRemove);
					}
				} else { 				// remove the one connection
					Piece sameColor = (a.color == toRemove.color) ? a: b;
					sameColor.connections.remove(toRemove);
				}
			}
		}
		grid[x][y] = null; //nuke the piece to remove
		getColorPieces(toRemove.color).remove(toRemove);
	}
	
	private Vector<Piece> endZonePieces(int color){
		Vector<Piece> endZone = new Vector<Piece>();
		if(color == BLACK){
			for (int i = 0; i < grid.length; i++) {
				if(grid[i][0]!= null){
					endZone.add(grid[i][0]);
				}
			}
		}else{
			for (int i = 0; i < SIZE; i++) {
				if(grid[0][i] != null){
					endZone.add(grid[0][i]);
				}
			}
		}
		
		return endZone;
	}
	
	private Vector<Piece> goalPieces(int color){
		Vector<Piece> goal = new Vector<Piece>();
		if(color == BLACK){
			for (int i = 0; i < grid.length; i++) {
				if(grid[i][SIZE -1]!= null){
					goal.add(grid[i][SIZE -1]);
				}
			}
		}else{
			for (int i = 0; i < SIZE; i++) {
				if(grid[SIZE -1][i] != null){
					goal.add(grid[SIZE -1][i]);
				}
			}
		}
		
		return goal;
	}
	
	public boolean hasValidNetwork(int color){
//		Vector<Piece> pieces = getColorPieces(color);
		Vector<Piece> pieces = getColorPieces(color);
		Vector<Piece> endZonePieces = endZonePieces(color);
		Vector<Piece> goalPieces = goalPieces(color);
		
//		this line is for fast returning if there is a quick way to determine no network exists
		if(endZonePieces.size() == 0 || goalPieces.size() == 0){
			return false;
		}
		
		for (Piece p: pieces){
			p.visited = false;
		}
		
//		remove the endzone (starting area) pieces from the pieces vector
		for (Piece piece : endZonePieces) {
//			pieces.remove(piece);
			piece.visited = true;
		}
		
		for (Piece p: endZonePieces){
//			System.out.println("Searching network for piece @ " + p);
//			p.visited = true;
			if(networkSearch(p, pieces, 1, null, goalPieces)){
//				p.visited = false;
//				System.out.println("Network found!");
				return true;
			}
			
//			p.visited = false;
		}
		return false;
	}
	
	/**
	 * searches to find a valid network off of the given piece
	 * @param p - start of the mini-network
	 * @param pieces, excluding pieces that have already been visited
	 * @param count - number of pieces visited up to and including p
	 * @param prevDirection - direction to get to p
	 * @return
	 */
	private boolean networkSearch(Piece p, Vector<Piece> pieces, int count, Direction prevDirection, Vector<Piece> goalPieces){
		if (goalPieces.contains(p)){
			if(count >= 6){
				return true;
			}else{
				return false;
			}
		}else if (allVisited(pieces)){
			return false;
		}
		
		for (Piece next : p.connections) {
//			check for whether or not this connection would be valid to help form a network
			if(p.direction(next) == prevDirection || next.visited || !pieces.contains(next)){
				continue;
			}
			
//			pieces.remove(next);
			next.visited = true;
			if(networkSearch(next, pieces, count +1, p.direction(next), goalPieces)){
//				next.visited = false;
//				pieces.add(next);
				return true;
			}			
			next.visited = false;
//			pieces.add(next);			
		}		
		return false;
	}
	
	/**
	 * tells whether or not all the pieces in the parameter have already been visited (true if they all have)
	 * @param pieces
	 * @return
	 */
	private boolean allVisited(Vector<Piece> pieces){
		for (Piece piece : pieces) {
			if(!piece.visited){
				return false;
			}
		}
		return true;
	}
	
	double evaluate(int player, Move m){
		doMove(m, player);
		if(hasValidNetwork(MachinePlayer.otherColor(player))){
			undoMove(m);
			return Integer.MIN_VALUE + 1;
		}else if (hasValidNetwork(player)){
			undoMove(m);
			return Integer.MAX_VALUE - 1;
		}
		
		int total = 0;
		for (Piece p : getColorPieces(player)) {
			total += p.connections.size();
		}
		
		for (Piece p : getColorPieces(MachinePlayer.otherColor(player))) {
			total -= p.connections.size();
		}
		
		
		undoMove(m);
		return total;
	}
	
	public String toString(){
		StringBuilder str = new StringBuilder();
		for (int j = 0; j < SIZE; j++) {
			str.append("|");
			for (int i = 0; i < SIZE; i++) {
				if(grid[i][j] == null)
					str.append(" |");
				else if(grid[i][j].color == BLACK){
					str.append("B|");
				} else {
					str.append("W|");
				}
			}
			str.append('\n');
		}
		return str.toString();
	}
	

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		Board b = new Board();
	
		Move m1 = new Move(1,3);
		Move m2 = new Move(6,7);
		Move m3 = new Move(1,2);
		Move m4 = new Move(6,5);
		Move m5 = new Move(0,5);
		Move m6 = new Move(4,7);
		Move m7 = new Move(3,2);
		Move m8 = new Move(6,3);
		Move m9 = new Move(3,6);
		Move m10 = new Move(1,0);
		Move m11 = new Move(1,5);
//		Move m12 = new Move(5,2);
//		Move m13 = new Move(7,3);
//		Move m14 = new Move(0,1);
//		Move m15 = new Move(5,4);
		b.doMove(m1, WHITE);
		b.doMove(m2, BLACK);
		b.doMove(m3, WHITE);
		b.doMove(m4, BLACK);
		b.doMove(m5, WHITE);
		b.doMove(m6, BLACK);
		b.doMove(m7, WHITE);
		b.doMove(m8, BLACK);
		b.doMove(m9, WHITE);
		b.doMove(m10, BLACK);
		b.doMove(m11, WHITE);
//		b.doMove(m12, BLACK);
//		b.doMove(m13, WHITE);
//		b.doMove(m14, WHITE);
//		b.doMove(m15, WHITE);
		System.out.println(b);
//		System.out.println(m1 + " : "+ b.validateMove(m1, WHITE));
//		System.out.println(m2 + " : "+ b.validateMove(m2, WHITE));
		System.out.println(b.hasValidNetwork(WHITE));
//		System.out.println(b.getValidMoves(WHITE));
		Minimax m = new Minimax(b, BLACK, 3);
		System.out.println(m.search(WHITE));
	}

}
