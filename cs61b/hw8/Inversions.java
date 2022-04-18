import java.util.Arrays;
import java.util.List;

/** HW #8, Optional Problem 5b.
 *  @author
 */
public class Inversions {

    /** A main program for testing purposes.  Prints the number of inversions
     *  in the sequence ARGS. */
    public static void main(String[] args) {
        System.out.println(inversions(Arrays.asList(args)));
    }

    /** Return the number of inversions of T objects in ARGS. */
    public static <T extends Comparable<? super T>> 
        int inversions(List<T> args)
    {
        return inversions1(new ArrayList<T>(args)); // REPLACE WITH YOUR ANSWER
    }
/** Return the number of inversions of T objects in ARGS,
     *  sorting ARGS in the process. */
    public static <T extends Comparable<? super T>> 
    int inversions1(List<T> args)
    {
        if (args.size() <= 1) {
            return 0;
        }
        int M = args.size() / 2;
        List<T> left = args.subList(0, M),
            right = args.subList(M, args.size());
        ArrayList<T> result = new ArrayList<>();
        int i, j, n;
        n = inversions1(left) + inversions1(right);
        for (i = 0, j = 0; i < left.size() && j < right.size(); ) {
            if (left.get(i).compareTo(right.get(j)) <= 0) {
                result.add(left.get(i));
                i += 1;
                n += j;
            } else {
                result.add(right.get(j));
                j += 1;
            }
        }
        
        n += right.size() * (left.size() - i);
        result.addAll(left.subList(i, left.size()));
        result.addAll(right.subList(j, right.size()));
        for (int k = 0; k < result.size(); k += 1) {
            args.set(k, result.get(k));
        }
        return n;
    }
}
