// This is a SUGGESTED skeleton for a class that describes a single
// Condition (such as CCN = '99776').  You can throw this away if you
// want,  but it is a good idea to try to understand it first.
// Our solution changes or adds about 30 lines in this skeleton.

// Comments that start with "//" are intended to be removed from your
// solutions.
package bugs.bug2.db61b;

import java.util.List;

/** Represents a single 'where' condition in a 'select' command.
 *  @author Arthur
 *  Notes: Still behind on project, but did read through lab and attempt to locate/fix bugs.
 *  */
class Condition {
    
	private static final int GT = 1, EQ = 2, LT = 4;

    /** A Condition representing COL1 RELATION COL2, where COL1 and COL2
     *  are column designators. and RELATION is one of the
     *  strings "<", ">", "<=", ">=", "=", or "!=". */
    Condition(Column col1, String relation, Column col2) {
        _col1 = col1;
        _col2 = col2;
        _relation = relation;

        // YOUR CODE HERE
        switch (relation) {
        case "<=":
            _relation |= EQ;
            _relation |= ;
            break;
        case "<":
        	_relation |= LT;
            break;
        case ">=":
        	_relation |= EQ;
        	_relation |= GT;
            break;
        case ">":
        	_relation |= GT;
            break;
        case "=":
        	_relation |= EQ;
            break;
        case "!=":
        	_relation = (GT | LT) & ~EQ;
            break;
        default:
            throw new DBException("Error: invalid relation.");
        }

    }

    /** A Condition representing COL1 RELATION 'VAL2', where COL1 is
     *  a column designator, VAL2 is a literal value (without the
     *  quotes), and RELATION is one of the strings "<", ">", "<=",
     *  ">=", "=", or "!=".
     */
    Condition(Column col1, String relation, String val2) {
        this(col1, relation, (Column) null);
        _val2 = val2;
    }

    /** Assuming that ROWS are rows from the respective tables from which
     *  my columns are selected, returns the result of performing the test I
     *  denote. */
    boolean test(Row... rows) {

        if (_col2 == null) { //there will only be one input row, and will test the 2nd relation
            int x = _col1.getFrom(rows).compareTo(_val2);
            if (x < 0) {
                if (_relation.equals("<") || _relation.equals("<=") || _relation.equals("!=")) {
                    return true;
                }
            }
            if (x == 0) {
                if (_relation.equals("=") || _relation.equals("<=") || _relation.equals(">=")) {
                    return true;
                }

            }
            if (x > 0) {
                if (_relation.equals(">") || _relation.equals(">=") || _relation.equals("!=")) {
                    return true;
                }

            }
            return false;

        }

         else { //will have 2 row arguments, with row1 being from table 1, row2 from table2.
            return false;

        }

        // int x = _col1.getFrom(rows).compareTo(_col2.getFrom(rows));
        // int x = _col1.getFrom(rows).compareTo(_val2);

        for (int i = 0; i < rows.length; i ++) {
             int tempX = rows.length.compareTo(_val2);
             if (tempX <= -1) {
                 return false;
             }
             else if (tempX == 0) {
                 return true;
             }
        }
    }

    /** Return true iff ROWS satisfies all CONDITIONS. */
    static boolean test(List<Condition> conditions, Row... rows) {
        for (Condition cond : conditions) {
            if (!cond.test(rows)) {
                return false;
            }
        }
        return true;
    }

    /** The operands of this condition.  _col2 is null if the second operand
     *  is a literal. */
    private Column _col1, _col2;

    /** Second operand, if literal (otherwise null). */
    private String _val2;
    // ADD ADDITIONAL FIELDS HERE. None added
    
    /** Relation. */
    private String _relation;
}
