package db61b;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.List;

import org.junit.Test;

import ucb.junit.textui;

public class AllTests {
    /**
     * Runs all of the  tests for this project.
     */
	private Table tab;
	private static List<Row> _rows;
	private static final String[] COLS = new String[] { "SID", "NAME", "LEVEL", "CLASS" };
	private static final String NAME = "TEST";
    
	@Test
    public void testRow() {
        Row row_test = new Row(new String[] { "One", "Two", "Three" });
        assertEquals("One", row_test.get(0));
        assertEquals("Two", row_test.get(1));
        assertEquals("Three", row_test.get(2));
    }
    
    @Test
    public void testSize() {
        Row row_test = new Row(new String[] { "One", "Two", "Three" });
        assertEquals(row_test.size(), 3);
        Row empty = new Row(new String[] {});
        assertEquals(empty.size(), 0);
    }
    private static Table _tab;
    private static Column col1;
    private static Column col2;
    private TableIterator tableiterate;
    
    @Test
    public void testColumn() {
        Column _col1 = new Column(_tab, COLS[0]);
        Column _col2 = new Column(null, COLS[1]);
        assertEquals(COLS[0], _col1.name());
        assertEquals(COLS[1], _col2.name());
    }
    
    @Test
    public void testTable() {
        _rows = new ArrayList<Row>();
        _rows.add(new Row(new String[] { "123", "Arthur", "10", "Architecture" }));
        _rows.add(new Row(new String[] { "234", "Brian", "10", "Business" }));
        _rows.add(new Row(new String[] { "345", "Connie", "10", "Chemistry" }));
        _rows.add(new Row(new String[] { "456", "Darcy", "10", "Deontology" }));

        _tab = new Table(NAME, COLS);

        for (Row row : _rows) {
            _tab.add(row);
        }
        col1 = new Column(_tab, COLS[0]);
        col2 = new Column(_tab, COLS[1]);
    }
}