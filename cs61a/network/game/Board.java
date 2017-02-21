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
	
	// instance variables
	private Piece[][] grid;
	private Vector<Piece> whitePieces;
	private Vector<Piece> blackPieces;
	
	public Board() {
		grid = new Piece[SIZE][SIZE];
		whitePieces = new Vector<Piece>();
		blackPieces = new Vector<Piece>();
	}
	
    /**
     * validateMove
     * checks whether any given move is valid on this board
     * @param m - the move to be made
     * @param color - the color for the player making the move
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
		int total = 0;

		for (int i = x-1; i <= x+1 && i < SIZE; i++) {
			for (int j = y-1; j <= y+1 && j < SIZE; j++) {
				if(!isCoordinateOnGrid(i,j) || i == x && j == y)
					continue;
				if (total > 1){
					return total;
				}
				if (grid[i][j] == null) {
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
		if (getColorPieces.size() == MAXPIECES) { // can only do moving moves
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
    	} else { // can only do adding moves
			for (int i = 0; i < SIZE; i++) {
				for (int j = 0; j < SIZE; j++) {
					test = new Move(i, j);
					if (validateMove(test, color)) {
						moves.add(test);
					}
				}
			}   		
    	}
    	return moves;
    }

    /**
     * doMove makes a move for a player
     * @param m - move to be made
     * @param color - color of player making move
     * @return true if move was successful, false if invalid
     */
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
    
   /**
    * undoMove takes the last move and undo's its affects
    * 
    * IMPORTANT *** can only undo the last move performed. MUST be legal 
    * @param m - move to be undone
    */
    
    public void undoMove(Move m){
    	if(m.moveKind == Move.ADD){
    		removePiece(m.x1, m.y1);
    	}else if (m.moveKind == Move.STEP){
    		movePiece(m.x1, m.y1, m.x2, m.y2);
    	}
    }
    
    
	/**
	 * adds a piece of the given color to the board, while building connections between pieces 
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
	
	/**
	 * moves a piece from x0, y0 to x1, y1
	 * @param x0, y0 - from coordinates 
	 * @param x1, y1 - to coordinates
	 */
	void movePiece(int x0, int y0, int x1, int y1){
		try {
		int color = grid[x0][y0].color;
		removePiece(x0, y0);
		addPiece(x1, y1, color);
		} catch (Exception e) {
			System.out.println();
		}
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

	/**
	 * findPieceInDirection
	 * takes a (x,y) coordinate and a direction and finds the nearest piece
	 * that can be reached starting from (x,y) and traveling in given direction
	 * @param x, y - starting coordinate
	 * @param dir - given direction
	 * @return nearest piece in path. Null if no piece
	 */
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
	 
	/**
	 * isCoordinateOnGrid
	 * checks if (x,y) is a valid space on the grid. corners are invalid
	 * @param x, y - location to be checked
	 * @return boolean if the location is invalid 
	 */
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
	
	/**
	 * getColorPiece
	 * helper to return the corresponding vector of pieces with a color
	 * @param color - pieces to return
	 * @return Vector of pieces
	 */
	private Vector<Piece> getColorPieces(int color){
		if(color == WHITE)
			return whitePieces;
		else
			return blackPieces;
	}
	
	/**
	 * endZonePieces
	 * helper to return the pieces of one color that live in the "endzone"
	 * @param color - color of pieces to return
	 * @return Vector of pieces
	 */
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
	
	/**
	 * goalPieces
	 * helper to return the pieces of one color that live in the "goal"
	 * @param color - color of pieces to return
	 * @return Vector of pieces
	 */
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
	
	/**
	 * hasValidNetwork
	 * checks the current board if given player has a winning Network
	 * @param color - color of player to be checked
	 * @return true if color has a network, false otherwise
	 */
	public boolean hasValidNetwork(int color){
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
			piece.visited = true;
		}
		
		for (Piece p: endZonePieces){
			if(networkSearch(p, pieces, 1, null, goalPieces)){
				return true;
			}
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
			
			next.visited = true;
			if(networkSearch(next, pieces, count +1, p.direction(next), goalPieces)){
				return true;
			}			
			next.visited = false;
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
	
	/**
	 * longestPath
	 * approximates the longest path of a given player in a given board
	 * @param color - color of player to check
	 * @return an int representing their approximate longest network
	 */
	public int longestPath(int color){
		Vector<Piece> pieces = getColorPieces(color);

		for(Piece piece: pieces){
			piece.connectivity = 0;
			boolean inEndZone = isInEndZone(piece);
			boolean[] directionCovered = new boolean[8];
			for(Piece other: piece.connections){
				if(inEndZone && isInEndZone(other)){
					continue;
				}
				Direction dir = piece.direction(other);
				if(directionCovered[dir.direction] == false || directionCovered[Direction.getOpposite(dir.direction)] == false){
					directionCovered[dir.direction] = true;
					directionCovered[Direction.getOpposite(dir.direction)] = true;
					piece.connectivity ++;
				}
			}
		}	
		int maxPathSize = 0;
		for(Piece start: pieces){
			int pathSize = 1;
			start.visited = true;
			Piece cur = start;
			Piece next = getNextPiece(start, null);

			while(next != null){
				next.visited = true;
				pathSize++;
				
				Piece temp = getNextPiece(next, cur.direction(next));
				cur = next;
				next = temp;
			}

			if(pathSize > maxPathSize){
				maxPathSize = pathSize;
			}

			//reset visited flags
			for(Piece p: pieces){
				p.visited = false;
			}
		}

		return maxPathSize;
	}

	/**
	 * getNextPiece
	 * helper to determine out of a pieces connections, which other piece represents the best connectivity
	 * and/ best connection
	 * @param cur - piece to find the next of
	 * @param oldDir - direction coming into CUR. direction cannot be the same
	 * @return - Piece of the next piece, or null if no more pieces
	 */
	private Piece getNextPiece(Piece cur, Direction oldDir){
		int maxConnectivity = 0;
		Piece maxConnectivityPiece = null;
		for(Piece next: cur.connections){
			if(next.visited == true || cur.direction(next).equals(oldDir) || isInEndZone(cur) && isInEndZone(next))
				continue;
			if(next.connectivity > maxConnectivity){
				maxConnectivity = next.connectivity;
				maxConnectivityPiece = next;
			}
		}
		return maxConnectivityPiece;
	}

	/**
	 * isInEndZone
	 * private helper to determine if a piece is in its own endzone
	 * @param p - piece to check
	 * @return true if p is in endzone, false otherwise
	 */
	private boolean isInEndZone(Piece p){
		if (p.color == WHITE && (p.x == 0 || p.x == SIZE-1)){
			return true;
		} else if (p.color == BLACK && (p.y == 0 || p.y == SIZE -1)){
			return true;
		} else {
			return false;
		}
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
	 * Test case
	 */
	public static void main(String[] args) {
//		Board b = new Board();
	MachinePlayer m = new MachinePlayer(WHITE, 2);
		
		Move bl1 = new Move(1,1);
		Move bl2 = new Move(2,1);
		Move bl3 = new Move(4,1);
		Move bl4 = new Move(5,1);
		Move bl5 = new Move(1,6);

		Move w1 = new Move(0,2);
		Move w2 = new Move(1,2);
		Move w3 = new Move(4,2);
		Move w4 = new Move(1,5);
		Move w5 = new Move(4,5);
//		Move w6 = new Move(4,5);

//		b.doMove(w1, WHITE);
//		b.doMove(w2, WHITE);
//		b.doMove(w3, WHITE);
//		b.doMove(w4, WHITE);
//		b.doMove(w5, WHITE);
//		b.doMove(w6, WHITE);
		
		m.forceMove(w1);
		m.opponentMove(bl1);
		m.forceMove(w2);
		m.opponentMove(bl2);
		m.forceMove(w3);
		m.opponentMove(bl3);
		m.forceMove(w4);
		m.opponentMove(bl4);
		m.forceMove(w5);
		m.opponentMove(bl5);
		
		System.out.println(m.chooseMove());
		
		MachinePlayer m2 = new MachinePlayer(WHITE, 2);
		
		bl1 = new Move(1,1);
		bl2 = new Move(2,1);
		bl3 = new Move(4,1);
		bl4 = new Move(5,1);
		bl5 = new Move(1,6);
		Move bl6 = new Move(2,6);
		Move bl7 = new Move(5,5);

		w1 = new Move(0,2);
		w2 = new Move(1,3);
		w3 = new Move(5,3);
		w4 = new Move(1,5);
		w5 = new Move(3,5);
		Move w6 = new Move(3,2);
		Move w7 = new Move(6,1);
		Move w8 = new Move(6,5);
		
		m2.forceMove(w1);
		m2.opponentMove(bl1);
		m2.forceMove(w2);
		m2.opponentMove(bl2);
		m2.forceMove(w3);
		m2.opponentMove(bl3);
		m2.forceMove(w4);
		m2.opponentMove(bl4);
		m2.forceMove(w5);
		m2.opponentMove(bl5);
		m2.forceMove(w6);
		m2.opponentMove(bl6);
		m2.forceMove(w7);
		m2.opponentMove(bl7);
		m2.forceMove(w8);
		
		
		System.out.println(m2.chooseMove());
		
//		System.out.println(m.search(WHITE));
//		b.doMove(m.search(WHITE), WHITE);
//		System.out.println("Should now have valid network: true? : " + b.hasValidNetwork(WHITE) );
		
//		System.out.println(m1 + " : "+ b.validateMove(m1, WHITE));
//		System.out.println(m2 + " : "+ b.validateMove(m2, WHITE));
//		System.out.println(b.hasValidNetwork(WHITE));
//		System.out.println(b.getValidMoves(WHITE));
		
	}

}
