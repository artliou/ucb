
import java.util.ArrayList;
import java.util.LinkedList;


public class IntDList {

    private DNode _front, _back;

    public IntDList() {
        _front = _back = null;
    }

    public IntDList(Integer... values) {
        _front = _back = null;
        for (int val : values) {
            insertBack(val);
        }
    }

    public int getFront() {
        return _front._val;
    }

    /** Returns the last item in the IntDList. */
    public int getBack() {
        return _back._val;
    }

    /** Return value #I in this list, where item 0 is the first, 1 is the
     *  second, ...., -1 is the last, -2 the second to last.... */
    public int get(int i) {
        if (i == 0){
            return _front._val;}
        int count = 0;
        DNode n = _front;
        while (count < i){
            n = n._next;
            count += 1;}
        return n._val;   // Your code here
    }
//this.list()
    /** The length of this list. */
    public int size() {
        if (this._front == null){
            return 0;
        }
        int len = 1; //Length
        DNode n = this._front;
        while (n != this._back){
            n = n._next;
            len += 1;
        }
        return len;  // Your code here
    }

    /** Adds D to the front of the IntDList. */
    public void insertFront(int d) {
        if (this._front == null):{
            this._front = new Node(d);
            this._back = this._front;
            this._front.next = this._back
    }
        else{
            this._front.next = this._front
            this._front = new Node(d);
        }
// Your code here 
    }

    /** Adds D to the back of the IntDList. */
    public void insertBack(int d) {
        if (this._front == null):{}
            this._back = new Node(d);
            this._front.element = elem;
            this._back = this._front;
            this._front.next = this._back
    }
        else{
            this._back = new Node(d);
            this._back = this._back.next
            this._back.d = d
        }
        return null;        // Your code here 
    }

    /** Removes the last item in the IntDList and returns it.
     * This is an extra challenge problem. */
    public int deleteBack() {
        return 0;   // Your code here

    }

    /** Returns a string representation of the IntDList in the form
     *  [] (empty list) or [1, 2], etc. 
     * This is an extra challenge problem. */
    public String toString() {
        return null;   // Your code here
    }

    /* DNode is a "static nested class", because we're only using it inside
     * IntDList, so there's no need to put it outside (and "pollute the
     * namespace" with it. */
    private static class DNode {
        protected DNode _prev;
        protected DNode _next;
        protected int _val;

        private DNode(int val) {
            this(null, val, null);
        }

        private DNode(DNode prev, int val, DNode next) {
            _prev = prev;
            _val = val;
            _next = next;
        }
    }

}
