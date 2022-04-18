import java.util.HashSet;
import java.util.Set;

/** Test
 *  @author You
 */
public class SetDemo {

    /** Declare a Set that holds Strings.*/
	public static void main(String[] args) {
    	String elements[] = { "papa", "bear", "mama", "bear", "baby", "bear" }; //instantiates the set
    	Set set = new HashSet(Arrays.asList(elements)); //declares a set that holds String elements

		set.add();
		for (String item) {
            System.out.println(item); //prints the set at each string
          }
    }
}