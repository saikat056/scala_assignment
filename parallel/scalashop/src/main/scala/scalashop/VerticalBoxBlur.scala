package scalashop

import java.util

import org.scalameter._
import common._

import scala.collection.parallel.ForkJoinTasks

object VerticalBoxBlurRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val radius = 3
    val width = 1920
    val height = 1080
    val src = new Img(width, height)
    val dst = new Img(width, height)
    val seqtime = standardConfig measure {
      VerticalBoxBlur.blur(src, dst, 0, width, radius)
    }
    println(s"sequential blur time: $seqtime ms")

    val numTasks = 32
    val partime = standardConfig measure {
      VerticalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }

}

/** A simple, trivially parallelizable computation. */
object VerticalBoxBlur {

  /** Blurs the columns of the source image `src` into the destination image
   *  `dst`, starting with `from` and ending with `end` (non-inclusive).
   *
   *  Within each column, `blur` traverses the pixels by going from top to
   *  bottom.
   */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    // TODO implement this method using the `boxBlurKernel` method
    var height = src.height

    for(x <- from until end){
      for(y <- 0 until height){
        dst.update(x, y, boxBlurKernel(src, x, y, radius))
      }
    }

  }

  /** Blurs the columns of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  columns.
   */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    // TODO implement using the `task` construct and the `blur` method
    var width = src.width
    var each_len = width / numTasks

    var start = 0
    val list: util.LinkedList[java.util.concurrent.ForkJoinTask[Unit]] = new util.LinkedList

    while (width >= each_len) {

      list.add(task(blur(src, dst, start, start + each_len, radius)))

      start = start + each_len
      width -= each_len
    }

    if (width > 0) {
      list.add(task(blur(src, dst, start, start + width, radius)))
    }

    var iter : util.Iterator[java.util.concurrent.ForkJoinTask[Unit]] = list.iterator()

    var v = 0
    while (!list.isEmpty) {
      println("k: " + v)
      v = v + 1
      val t : java.util.concurrent.ForkJoinTask[Unit] = list.peek()
      t.join()
      list.removeFirst()
    }


  }

}
