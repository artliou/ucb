/*
Consider decimal numerals containing only the digits 1–3. A numeral
is considered “good” if no two adjacent non-empty substrings of it are equal; otherwise it is “bad.”
Hence, the numerals ‘1’, ‘12’, and ‘1213’ are good, while ‘11’, ‘32121’, and ‘121321312’ are bad.
You are to write a program that, given n > 0, finds the smallest good n-digit numeral. The
input consists of a sequence of positive integers. For each of these integers, n, the output is to
contain a line of the form
"The smallest good numeral of length n is s""
where s is the answer.

*/
public class P3 {

    public static void main(String input) {
		int n, int s;
		ArrayList good = new ArrayList(String);
		while (int i = 0; i < input.length; i ++) {
			s = good[1];
			if (s > good[1]) {
				s = good[2];
			}
			System.out.println("The smallest good numeral of length" + n + "is" + s)
		}
	}
}