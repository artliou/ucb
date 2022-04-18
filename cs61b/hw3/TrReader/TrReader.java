import java.io.Reader;
import java.io.IOException;

/** Translating Reader: a stream that is a translation of an
 *  existing reader.
 *  @author
 */
public class TrReader extends Reader {
    /** A new TrReader that produces the stream of characters produced
     *  by STR, converting all characters that occur in FROM to the
     *  corresponding characters in TO.  That is, change occurrences of
     *  FROM.charAt(0) to TO.charAt(0), etc., leaving other characters
     *  unchanged.  FROM and TO must have the same length. */
    private final Reader _subReader;
    private final String _from, _to;

    public TrReader(Reader str, String from, String to) {
        _subReader = str;
        _from = from;
        _to = to;

    private char convertChar(char in) {
        int c = _from.indexOf(in);
        if (c == -1) {
            return in;
        } else {
            return _to.charAt(c);
        }
    }
        // FILL IN
    }

    @Override
    public int read(char[] buff, int off, int len) throws IOException { //Declares Abstract
        int actualRead = _subReader.read(buff, off, len);
        for (int i = off; i < off + actualRead; i += 1) {
            buff[i] = convertChar(buff[i]);
        }
        return actualRead;
    } 
    // FILL IN
    // NOTE: Until you fill in the right methods, the compiler will
    //       reject this file, saying that you must declare TrReader
    //     abstract.  Don't do that; define the right methods instead!
}