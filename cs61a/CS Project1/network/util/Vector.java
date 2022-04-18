package util;

import java.util.Iterator;

public class Vector<E> implements Iterable<E>{
	
	private DList items;
	
	public Vector(){
		items = new DList();
	}
	
	public Vector(Vector<E> v){
		this();
		for(E item: v){
			this.add(item);
		}
	}
	
	public int size(){
		return items.size;
	}
	
	public void add(E item){
		items.insertBack(item);
	}
	
	public E pop(){
		return (E) items.pop();
	}
	
	public boolean remove(E item){
		return items.remove(item);
	}

	public Iterator<E> iterator() {
		return new Iterate_Vector<E>(items);
	}
	
	public boolean contains(E search){
		for (E item : this) {
			if(item.equals(search)){
				return true;
			}
		}		
		return false;
	}
	
	
	public String toString(){
		String ret = "";
		for (E item : this) {
			ret += item.toString() + ", ";
		}
		
		return ret;
	}
	
	public static void main(String[] args){
		Vector<String> a = new Vector<String>();
		char[] toAdd = {'a','b','c','d','e','f'};
		for(char c: toAdd){
			a.add(c + "");
		}
		a.remove("a");
		for(String s: a){
			System.out.println(s);
		}
	}
}

class Iterate_Vector<E> implements Iterator<E>{

	private DListNode current;
	private DListNode end;
	
	Iterate_Vector(DList items){
		current = items.head;
		end = items.head;
	}
	
	public boolean hasNext() {
		return (current.next != end);
	}

	public E next() {
		current = current.next;
		return (E) current.item;
	}

	@Override
	public void remove() {
		int i = 0;
		i++;
	}	
}
