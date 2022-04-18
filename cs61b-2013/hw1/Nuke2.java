import java.io.*;

public class Nuke2 {
	
		public static void main(String[] arg) throws Exception{
		
			BufferedReader keyboard;
			String inputLine;

			keyboard = new BufferedReader(new InputStreamReader(System.in));
			inputLine = keyboard.readLine();
	    
			String one = inputLine.substring(0, 1);
			String two = inputLine.substring(2);
			System.out.println(one + two);
	    
	  }
}
