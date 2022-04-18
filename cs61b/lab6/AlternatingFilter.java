import java.util.Iterator;
import utils.Filter;

/** A kind of Filter that lets through every other VALUE element of
 *  its input sequence, starting with the first.
 *  @author You
 */
class AlternatingFilter<Value> extends Filter<Value> {

    /** A filter of values from INPUT that lets through every other
     *  value. */
    AlternatingFilter(Iterator<Value> input) {
        super(input); //FIXME?
        // FIXME
        Iterator<Value> k = new Iterator<Value>();
        for (int i = 0; i < super.length; i += 2){
            int val = super[i];
            k.append(val);
          }
        return k;
    }

    @Override
    protected boolean keep() {
        return false;  // FIXME
    }
    return k;
    // FIXME

}
