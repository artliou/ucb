/* ListSorts.java */

import list.*;

public class ListSorts {

  private final static int SORTSIZE = 1000;

  /**
   *  makeQueueOfQueues() makes a queue of queues, each containing one item
   *  of q.  Upon completion of this method, q is empty.
   *  @param q is a LinkedQueue of objects.
   *  @return a LinkedQueue containing LinkedQueue objects, each of which
   *    contains one object from q.
   **/
  public static LinkedQueue makeQueueOfQueues(LinkedQueue q) {
    // Replace the following line with your solution.
	  LinkedQueue returnq = new LinkedQueue();
		LinkedQueue temp;
		while (q.size() > 0) {
			try {
				temp = new LinkedQueue();
				temp.enqueue(q.dequeue());
				returnq.enqueue(temp);
			} catch (QueueEmptyException e) {
				System.out.println("QueueEmptyException error");
			}
		}
		return returnq;
  }

  /**
   *  mergeSortedQueues() merges two sorted queues into a third.  On completion
   *  of this method, q1 and q2 are empty, and their items have been merged
   *  into the returned queue.
   *  @param q1 is LinkedQueue of Comparable objects, sorted from smallest 
   *    to largest.
   *  @param q2 is LinkedQueue of Comparable objects, sorted from smallest 
   *    to largest.
   *  @return a LinkedQueue containing all the Comparable objects from q1 
   *   and q2 (and nothing else), sorted from smallest to largest.
   **/
  public static LinkedQueue mergeSortedQueues(LinkedQueue q1, LinkedQueue q2) {
    // Replace the following line with your solution.
	  LinkedQueue rq = new LinkedQueue();
		Comparable larger = null;
		LinkedQueue removal = null;
		Comparable one;
		Comparable two;
		while (removal==null || removal.size() > 0) { // I like how this works
			try {
				if (larger == null) {
				one = (Comparable) q1.dequeue();
				two = (Comparable) q2.dequeue();
				} else {
					if (removal == q1) {
						one = (Comparable) removal.dequeue();
						two = larger;
					} else {
						one = larger;
						two = (Comparable) removal.dequeue();
					}
				}
				if (one.compareTo(two) <= 0) {
					rq.enqueue(one);
					larger = two;
					removal = q1;
				} else {
					rq.enqueue(two);
					larger = one;
					removal = q2;
				}
			} catch (QueueEmptyException e) {
				System.out.println("QueueEmptyException thrown while we think there are still items on the queue...");
			}
		}
		if (larger !=null) {
			rq.enqueue(larger);
		}
		while (q1.size() > 0) {
			try {
				rq.enqueue(q1.dequeue());
			} catch (QueueEmptyException e) {
				System.out.println("We somehow managed to throw QueueEmptyException while size is > 0. Weird.");
			}
		}
		while (q2.size() > 0) {
			try {
				rq.enqueue(q2.dequeue());
			} catch (QueueEmptyException e) {
				System.out.println("We somehow managed to throw QueueEmptyException while size is > 0. Weird.");
			}
		}
		return rq;
  }

  /**
   *  partition() partitions qIn using the pivot item.  On completion of
   *  this method, qIn is empty, and its items have been moved to qSmall,
   *  qEquals, and qLarge, according to their relationship to the pivot.
   *  @param qIn is a LinkedQueue of Comparable objects.
   *  @param pivot is a Comparable item used for partitioning.
   *  @param qSmall is a LinkedQueue, in which all items less than pivot
   *    will be enqueued.
   *  @param qEquals is a LinkedQueue, in which all items equal to the pivot
   *    will be enqueued.
   *  @param qLarge is a LinkedQueue, in which all items greater than pivot
   *    will be enqueued.  
   **/   
  public static void partition(LinkedQueue qIn, Comparable pivot, 
                               LinkedQueue qSmall, LinkedQueue qEquals, 
                               LinkedQueue qLarge) {
    // Your solution here.
	  while (qIn.size() > 0) {
			try {
				Comparable temp = (Comparable) qIn.dequeue();
				if (temp.compareTo(pivot) < 0) {
					qSmall.enqueue(temp);
				} else if (temp.compareTo(pivot) == 0) {
					qEquals.enqueue(temp);
				} else {
					qLarge.enqueue(temp);
				} 
			} catch (QueueEmptyException e) {
				System.out.println("Managed to throw a QueueEmptyException despite having size > 0. What the hell.");
			}
		}
		if (qSmall.size() > 1) {
			quickSort(qSmall);
		}
		if (qLarge.size() > 1) {
			quickSort(qLarge);
		}
		qIn.append(qSmall);
		qIn.append(qEquals);
		qIn.append(qLarge);
	
  }

  /**
   *  mergeSort() sorts q from smallest to largest using mergesort.
   *  @param q is a LinkedQueue of Comparable objects.
   **/
  public static void mergeSort(LinkedQueue q) {
    // Your solution here.
  }

  /**
   *  quickSort() sorts q from smallest to largest using quicksort.
   *  @param q is a LinkedQueue of Comparable objects.
   **/
  public static void quickSort(LinkedQueue q) {
    // Your solution here.
	  LinkedQueue lqq = makeQueueOfQueues(q);
		while (lqq.size() > 1) {
			try {
				lqq.enqueue(mergeSortedQueues((LinkedQueue) lqq.dequeue(), (LinkedQueue) lqq.dequeue()));
			} catch (QueueEmptyException e) {
				System.out.println("Throwing QueueEmptyException while lqq's size > 1. Talk about strange.");
			}
		}
		try {
			q.append((LinkedQueue) lqq.dequeue());
		} catch (QueueEmptyException e) {
			System.out.println("Throwing QueueEmptyException when dequeueing the one large queue one last time. Damnit.");
		}
  }

  /**
   *  makeRandom() builds a LinkedQueue of the indicated size containing
   *  Integer items.  The items are randomly chosen between 0 and size - 1.
   *  @param size is the size of the resulting LinkedQueue.
   **/
  public static LinkedQueue makeRandom(int size) {
    LinkedQueue q = new LinkedQueue();
    for (int i = 0; i < size; i++) {
      q.enqueue(new Integer((int) (size * Math.random())));
    }
    return q;
  }

  /**
   *  main() performs some tests on mergesort and quicksort.  Feel free to add
   *  more tests of your own to make sure your algorithms works on boundary
   *  cases.  Your test code will not be graded.
   **/
  public static void main(String [] args) {

    LinkedQueue q = makeRandom(10);
    System.out.println(q.toString());
    mergeSort(q);
    System.out.println(q.toString());

    q = makeRandom(10);
    System.out.println(q.toString());
    quickSort(q);
    System.out.println(q.toString());

    Timer stopWatch = new Timer();
    q = makeRandom(SORTSIZE);
    stopWatch.start();
    mergeSort(q);
    stopWatch.stop();
    System.out.println("Mergesort time, " + SORTSIZE + " Integers:  " +
                       stopWatch.elapsed() + " msec.");

    stopWatch.reset();
    q = makeRandom(SORTSIZE);
    stopWatch.start();
    quickSort(q);
    stopWatch.stop();
    System.out.println("Quicksort time, " + SORTSIZE + " Integers:  " +
                       stopWatch.elapsed() + " msec.");
    */
  }

}
