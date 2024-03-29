import java.util.Arrays;

/**
 * Class containing all the sorting algorithms from 61B to date.
 *
 * You may add any number instance variables and instance methods
 * to your Sorting Algorithm classes.
 *
 * You may also override the empty no-argument constructor, but please
 * only use the no-argument constructor for each of the Sorting
 * Algorithms, as that is what will be used for testing.
 *
 * Feel free to use any resources out there to write each sort,
 * including existing implementations on the web or from DSIJ.
 *
 * All implementations except Distribution Sort adopted from Algorithms, 
 * a textbook by Kevin Wayne and Bob Sedgewick. Their code does not
 * obey our style conventions.
 */
public class MySortingAlgorithms {

    /**
     * Java's Sorting Algorithm. Java uses Quicksort for ints.
     */
    public static class JavaSort implements SortingAlgorithm {
        @Override
        public void sort(int[] array, int k) {
            Arrays.sort(array, 0, k);
        }

        @Override
        public String toString() {
            return "Built-In Sort (uses quicksort for ints)";
        }        
    }

    /** Insertion sorts the provided data. */
    public static class InsertionSort implements SortingAlgorithm {
        @Override
        public void sort(int[] array, int k) {
            // TO-DO
            int N = Math.min(k, array.length);
            for (int i = 0; i < N; i++) {
                for (int j = i; j > 0 && array[j] < array[j-1]; j--) {
                    swap(array, j, j-1);
                }                
            }
        }

        @Override
        public String toString() {
            return "Insertion Sort";
        }
    }

    /**
     * Selection Sort for small K should be more efficient
     * than for larger K. You do not need to use a heap,
     * though if you want an extra challenge, feel free to
     * implement a heap based selection sort (i.e. heapsort).
     */
    public static class SelectionSort implements SortingAlgorithm {
        @Override
        public void sort(int[] array, int k) {
            // TO-DO
            int N = Math.min(k, array.length);
            for (int i = 0; i < N; i++) {
                int min = i;
                for (int j = i+1; j < N; j++) {
                    if (array[j] < array[min]) {
                        min = j;
                    }
                }
                swap(array, i, min);
            }
        }

        @Override
        public String toString() {
            return "Selection Sort";
        }
    }

    /** Your mergesort implementation. An iterative merge
      * method is easier to write than a recursive merge method.
      * Note: I'm only talking about the merge operation here,
      * not the entire algorithm, which is easier to do recursively.
      */
    public static class MergeSort implements SortingAlgorithm {
        @Override
        public void sort(int[] array, int k) {            
            // TO-DO
            int[] aux = new int[array.length];            
            int hi = Math.min(k - 1, array.length - 1);
            sort(array, aux, 0, hi);
        }

        // may want to add additional methods

        @Override
        public String toString() {
            return "Merge Sort";
        }
    }

    /**
     * Your Distribution Sort implementation.
     * You should create a count array that is the
     * same size as the value of the max digit in the array.
     */
    public static class DistributionSort implements SortingAlgorithm {
        @Override
        public void sort(int[] array, int k) {
            // TODO: to be implemented
            int maxVal = Integer.MIN_VALUE;
            int N = Math.min(k, array.length);

            for (int i = 0; i < N; i += 1) {
                if (array[i] > maxVal) {
                    maxVal = array[i];
                }
            }

            int counts[] = new int[maxVal + 1];

            for (int i = 0; i < N; i += 1) {
                int dataPoint = array[i];
                counts[dataPoint] += 1;
            }

            int startingPoints[] = new int[maxVal + 1];
            for (int i = 1; i <= maxVal; i += 1) {
                startingPoints[i] = startingPoints[i-1] + counts[i-1];
            }

            int[] sorted = new int[N];
            for (int i = 0; i < N; i += 1) {
                int dataPoint = array[i];
                int targetLocation = startingPoints[dataPoint];
                sorted[targetLocation] = dataPoint;
                startingPoints[dataPoint] += 1;
            }
           
           for (int i = 0; i < N; i += 1) {
                array[i] = sorted[i];
           }
        }

        // may want to add additional methods

        @Override
        public String toString() {
            return "Distribution Sort";
        }
    }

    /** Your Heapsort implementation.
     */
    public static class HeapSort implements SortingAlgorithm {
        @Override
        public void sort(int[] array, int k) {
            // TODO   
            int N = Math.min(array.length, k);

            for (int m = N/2; m >= 1; m--) 
                sink(array, m, N);
            
            while (N > 1) {
                exch(array, 1, N);
                N -= 1;
                sink(array, 1, N);
            }
        }

        private static void sink(int[] pq, int m, int N) {
            while (2*m <= N) {
                int j = 2*m;
                if (j < N && less(pq, j, j+1)) j++;
                if (!less(pq, m, j)) break;
                exch(pq, m, j);
                m = j;
            }
        }

        @Override
        public String toString() {
            return "Heap Sort";
        }        
    }

    /** Your Quicksort implementation.
     */
    public static class QuickSort implements SortingAlgorithm {
        @Override
        public void sort(int[] array, int k) {
            // TODO   
            int N = Math.min(k - 1, array.length - 1);
            quicksort(array, 0, N);
        }

        /** Puts A[LO..HI] into sorted order. */
        private static void quicksort(int[] a, int lo, int hi) {
            if (hi <= lo) {
                return;
            }
            int j = partition(a, lo, hi);
            quicksort(a, lo, j - 1);
            quicksort(a, j + 1, hi);
        }

        @Override
        public String toString() {
            return "Quicksort";
        }        
    }

    /*
     * Your LSD Sort implementation, treating ints
     * as a sequence of 8 bit characters. Or if you want 
     * to do less bit-hacking, you can treat them as strings
     * of decimal digits.
     */
    public static class LSDSort implements SortingAlgorithm {
        @Override
        /** For an example implementation, see Kevin Wayne and 
          * Bob Sedgewick's Algorithms textbook.
          *
          * http://algs4.cs.princeton.edu/51radix/LSD.java.html
          */
        public void sort(int[] a, int k) {
            // TODO

            int BITS = 32;    
            int BITS_PER_BYTE = 8;            
            int W = BITS / BITS_PER_BYTE; 
            int R = 1 << BITS_PER_BYTE;   
            int MASK = R - 1;              

            int N = Math.min(a.length, k);
            int[] aux = new int[N];

            for (int d = 0; d < W; d++) {         

                // compute frequency counts
                int[] count = new int[R+1];
                for (int i = 0; i < N; i++) {           
                    int c = (a[i] >> BITS_PER_BYTE*d) & MASK;
                    count[c + 1]++;
                }

                // compute cumulates
                for (int r = 0; r < R; r++)
                    count[r+1] += count[r];

                // for most significant byte, 0x80-0xFF comes before 0x00-0x7F
                if (d == W-1) {
                    int shift1 = count[R] - count[R/2];
                    int shift2 = count[R/2];
                    for (int r = 0; r < R/2; r++)
                        count[r] += shift1;
                    for (int r = R/2; r < R; r++)
                        count[r] -= shift2;
                }

                // move data
                for (int i = 0; i < N; i++) {
                    int c = (a[i] >> BITS_PER_BYTE*d) & MASK;
                    aux[count[c]++] = a[i];
                }

                // copy back
                for (int i = 0; i < N; i++)
                    a[i] = aux[i];
            }
        }
        }

        @Override
        public String toString() {
            return "LSD Sort";
        }
    }

    /**
     * Tricky: Your MSD Sort implementation, treating ints
     * as a string of 8 bit characters.
     */
    public static class MSDSort implements SortingAlgorithm {
        @Override
        /** For an example implementation, see Kevin Wayne and 
          * Bob Sedgewick's Algorithms textbook.
          *
          * http://algs4.cs.princeton.edu/51radix/MSD.java.html
          */
        public void sort(int[] a, int k) {
            // TODO
            int N = Math.min(a.length, k);
            int[] aux = new int[N];
            sort(a, 0, N-1, 0, aux);
        }

        public void sort(int[] a, int lo, int hi, int d, int[] aux) {
            /* Each int is 32 bits and are thus 4 byes wide.
               Each byte holds a value between 0 and 255.
               MASK is used to mask off 8 bits at a time */
            int BITS_PER_INT = 32;    
            int BITS_PER_BYTE = 8;  
            int R = 1 << BITS_PER_BYTE;   
            int MASK = R - 1;              

            // compute frequency counts (need R = 256)
            int[] count = new int[R+1];
            int mask = R - 1;   // 0xFF;
            int shift = BITS_PER_INT - BITS_PER_BYTE*d - BITS_PER_BYTE;
            for (int i = lo; i <= hi; i++) {
                int c = (a[i] >> shift) & mask;
                count[c + 1]++;
            }

            // transform counts to indicies
            for (int r = 0; r < R; r++)
                count[r+1] += count[r];

            for (int i = lo; i <= hi; i++) {
                int c = (a[i] >> shift) & mask;
                aux[count[c]++] = a[i];
            }

            // copy back
            for (int i = lo; i <= hi; i++) 
                a[i] = aux[i - lo];

            // no more bits
            if (d == 4) return;

            // recursively sort for each character
            if (count[0] > 0)
                sort(a, lo, lo + count[0] - 1, d+1, aux);
            for (int r = 0; r < R; r++)
                if (count[r+1] > count[r])
                    sort(a, lo + count[r], lo + count[r+1] - 1, d+1, aux);
        }
        }

        @Override
        public String toString() {
            return "MSD Sort";
        }
    }

    // swap a[i] and a[j]
    private static void swap(int[] a, int i, int j) {
        int swap = a[i];
        a[i] = a[j];
        a[j] = swap;
    }

}


