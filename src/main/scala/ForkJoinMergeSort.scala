import java.util.concurrent.{ForkJoinPool, RecursiveTask}

import scala.util.Random

object ForkJoinMergeSort extends App {
  val length = 100
  val randomList = (for (i <- 1 to length) yield Random.nextInt(100)).toList
  println(randomList)

  val pool = new ForkJoinPool()

  class MergeTask(left: List[Int], right: List[Int]) extends RecursiveTask[List[Int]] {
    override def compute(): List[Int] = {
      if(left == Nil) right
      else if(right == Nil) left
      else if(left.head < right.head) {
        val merge = new MergeTask(left.tail, right)
        merge.fork()
        left.head :: merge.join()
      }
      else {
        val merge = new MergeTask(left, right.tail)
        merge.fork()
        right.head :: merge.join()
      }
    }
  }

  class MergeSortTask(list: List[Int]) extends RecursiveTask[List[Int]] {
    override def compute(): List[Int] = {
      val n = list.length / 2
      if(n == 0) {
        list
      }else {
        val (left, right) = list.splitAt(n)
        val leftTask = new MergeSortTask(left)
        val rightTask = new MergeSortTask(right)
        leftTask.fork()
        rightTask.fork()
        val merge = new MergeTask(leftTask.join(), rightTask.join())
        merge.fork()
        merge.join()
      }
    }
  }

  val sortedList = pool.invoke(new MergeSortTask(randomList))
  println(sortedList)
}