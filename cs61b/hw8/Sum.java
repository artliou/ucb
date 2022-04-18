/** HW #8, Optional Problem 5c.
 * @author
 */
public class Sum {

    /** Returns true iff A[i]+B[j] = M for some i and j. */
    public static boolean sumsTo(int[] A, int[] B, int m) {
		// REPLACE WITH YOUR ANSWER
		A = Arrays.copyOf(A, A.length);
        Arrays.sort(A);
        for (int b : B) {
            int k = Arrays.binarySearch(A, m - b);
            if (k >= 0 && k < A.length && A[k] + b == m) {
                return true;
            }
        }
        return false;
    }

    public static void main(String... args) {
        int M = Integer.parseInt(args[0]);
        Random r = new Random(Integer.parseInt(args[1]));
        int AL = r.nextInt(1000000), BL = r.nextInt(1000000);
        int[] A = new int[AL];
        int[] B = new int[BL];
        for (int i = 0; i < AL; i += 1) {
            A[i] = r.nextInt(1000000) * 2 * M;
        }        
        for (int i = 0; i < BL; i += 1) {
            B[i] = r.nextInt(1000000) * 2 * M;
        }        
        if (sumsTo(A, B, M)) {
            System.out.printf("False positive: m = %d%n", M);
            System.exit(1);
        } 
        A[r.nextInt(AL)] = M - B[r.nextInt(BL)];
        if (!sumsTo(A, B, M)) {
            System.out.printf("False negative: m = %d%n", M);
            System.exit(1);
        }
        System.out.println("OK");
    }
}
