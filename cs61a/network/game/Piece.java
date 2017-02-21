package game;

import util.Vector;

public class Piece implements Cloneable{

	protected int x,y;
	int color;
	int connectivity;
	
	Vector<Piece> connections;
	
	boolean visited;
	
	
	/**
	 * creates a new piece
	 * @param x
	 * @param y
	 * @param color
	 */
	public Piece(int x, int y, int color) {
		this.x = x; this.y = y;
		this.color = color;
		connections = new Vector<Piece>();
		visited = false;
	}
	
	public boolean equals(Piece other){
		if(other == null){
			return false;
		}
		return x == other.x && y == other.y && color == other.color;
	}
	
	
	/**
	 * Finds the direction from this piece to another piece
	 * @param other - the other piece for direction TO
	 * @return the direction FROM this piece TO the other piece
	 * returns null if called on itself
	 */
	Direction direction(Piece other){
		int delX = other.x - x;
		int delY = other.y - y;
		
		if(delX == 0 && delY == 0){
			return null;
		}
		
		if (delX == 0){
			if(delY < 0){
				return Direction.NORTH;
			}else{
				return Direction.SOUTH;
			}		
		}else if (delY == 0){
			if (delX > 0){
				return Direction.EAST;
			}else{
				return Direction.WEST;
			}		
		}
		
		if (delX > 0) {
			if(delY < 0){
				return Direction.NORTHEAST;
			}else{
				return Direction.SOUTHEAST;
			}
		}else{
			if(delY < 0){
				return Direction.NORTHWEST;
			}else{
				return Direction.SOUTHWEST;
			}
		}
	}
	
	
	
	public String toString(){
		String ret;
		if(color == Board.BLACK){
			ret = "BLACK @ (";
		}else{
			ret = "WHITE @ (";
		}
		
		ret += x + ", " + y + ")";		
		return ret;
	}

	public static void main(String[] args) {
		// TODO Auto-generated method stub
		Piece p1 = new Piece(3, 3, Board.BLACK);
		Piece p2 = new Piece(6, 0, Board.BLACK);
		System.out.println(p2.direction(p1));
		System.out.println(p1.direction(p2));
		System.out.println(Integer.MAX_VALUE);
		System.out.println(-(Integer.MAX_VALUE + 1));

	}

}
