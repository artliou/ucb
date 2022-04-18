package game;

//directions are filtered by ([x, y])
enum Direction {
		NORTH(0), SOUTH(4), 
		EAST(2), WEST(6),
		NORTHEAST(1), SOUTHWEST(5),
		SOUTHEAST(3), NORTHWEST(7);
		
		static int [][] relativeDirections = {
				{0,-1},{1,-1},{1,0},{1,1},{0,1},{-1,1},{-1,0},{-1,-1}
		};
		
		private int direction;		
		Direction(int d){
			this.direction = d;
		}
		
		static int getOpposite(int d){
			return (d + 4) % 8;
		}
}

