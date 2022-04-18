import java.util.Iterator;
import utils.Filter;

/** A kind of Filter that lets all the VALUE elements of its input sequence
 *  through.
 *  @author You
 */
class TrivialFilter<Value> extends Filter<Value> {

    /** A filter of values from INPUT that simply delivers all of them. */
    TrivialFilter(Iterator<Value> input) {
        super(input);
        // FIXME
        List<Integer> L = Arrays.asList(input);
        
        Iterator<Value> k = new Iterator<Value>();
        for (int i = 0; i < super.length; i += 1){
            int val = super[i];
            k.append(val);
          }
        return k;

    }

    @Override
    protected boolean keep() {
        if (input != null){
        	return true;}
        return false;    // FIXME
    }
}
