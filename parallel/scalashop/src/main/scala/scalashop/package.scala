
import common._

package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Restricts the integer into the specified range. */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))
    def apply(x: Int, y: Int): RGBA = data(y * width + x)
    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

  def isInRange(v: Int, min: Int, max: Int) : Boolean = {
    if (v < min || v >= max) false
    else true
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    // TODO implement using while loops
    var c = src.apply(x, y)
    var sum_red = 0
    var sum_green = 0
    var sum_blue = 0
    var sum_alpha = 0
    var total_points = 0
    var sum_color = 0


    sum_red += red(c)
    sum_green += green(c)
    sum_blue += blue(c)
    sum_alpha += alpha(c)
    total_points += 1
    sum_color += c

    if(radius < 1)
      c
    else {
      var r = 1
      while (r <= radius) {
        var offset = 0
        while (offset < (2 * radius) + 1) {
          var x1 = x + r
          var y1 = y + offset - radius
          if(isInRange(x1, 0, src.width) && isInRange(y1,0,src.height)) {
            c = src.apply(x1, y1)
            sum_red += red(c)
            sum_green += green(c)
            sum_blue += blue(c)
            sum_alpha += alpha(c)
            total_points += 1
            sum_color += c
//            println("(" + x1 + "," + y1 + "): " + c)
          }


          x1 = x - r
          y1 = y + offset - radius
          if(isInRange(x1, 0, src.width) && isInRange(y1,0,src.height)) {
            c = src.apply(x1, y1)
            sum_red += red(c)
            sum_green += green(c)
            sum_blue += blue(c)
            sum_alpha += alpha(c)
            total_points += 1
            sum_color += c
//            println("(" + x1 + "," + y1 + "): " + c)
          }

          offset += 1
        }

        r += 1
      }

      r = 1
      while (r <= radius) {
        var x2 = x
        var y2 = y + r
        if(isInRange(x2, 0, src.width) && isInRange(y2,0,src.height)) {
          c = src.apply(x2, y2)
          sum_red += red(c)
          sum_green += green(c)
          sum_blue += blue(c)
          sum_alpha += alpha(c)
          total_points += 1
          sum_color += c
//          println("(" + x2 + "," + y2 + "): " + c)
        }

        x2 = x
        y2 = y - r
        if(isInRange(x2, 0, src.width) && isInRange(y2,0,src.height)) {
          c = src.apply(x2, y2)
          sum_red += red(c)
          sum_green += green(c)
          sum_blue += blue(c)
          sum_alpha += alpha(c)
          total_points += 1
          sum_color += c
//          println("(" + x2 + "," + y2 + "): " + c)
        }

        r += 1
      }



      rgba((sum_red / total_points),
        (sum_green / total_points),
        (sum_blue / total_points),
        (sum_alpha / total_points)
      )

//      println("=====================")
//      println("x: " + x)
//      println("y: " + y)
//      println("width: " + src.width)
//      println("height: " + src.height)
//      println("c: " + src.apply(x, y))
//      println("sum: " + sum_color)
//      println("radius: " + radius)
//      println("total: " + total_points)
//      println("\n\n\n")

      sum_color /total_points
    }
  }

}
