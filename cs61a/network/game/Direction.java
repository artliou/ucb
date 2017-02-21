package game;

/**
 * Direction Enum
 * 
 * stores Direction information and provides conversion
 * between direction enum and integers
 */
enum Direction {
		NORTH(0), SOUTH(4), 
		EAST(2), WEST(6),
		NORTHEAST(1), SOUTHWEST(5),
		SOUTHEAST(3), NORTHWEST(7);
		
		//relativeDirections[i] gives a (dx,dy) tuple that are the 
		//delta x and y values for a step in direction i 
		static int [][] relativeDirections = {
				{0,-1},{1,-1},{1,0},{1,1},{0,1},{-1,1},{-1,0},{-1,-1}
		};
		
		int direction;		
		Direction(int d){
			this.direction = d;
		}
		
		public boolean equals(Direction other){
			if(other == null)
				return false;
			else return this.direction == other.direction;
		}
		
		static int getOpposite(int d){
			return (d + 4) % 8;
		}
}

