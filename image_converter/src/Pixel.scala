import scala.math._
import javax.imageio.ImageIO
import java.awt.image.BufferedImage

class Pixel(index: (Int, Int), image_size: Int, image: BufferedImage,OoB:Boolean) {

  def cartesian: Array[(Double, Double)] = {
    val output = Array.ofDim[(Double, Double)](4)
    val w = image.getWidth() / 2
    val h = image.getHeight() / 2
    output(0) = (this.index._2, this.index._1)
    output(1) = (output(0)._1 + 1, output(0)._2)
    output(2) = (output(1)._1, output(1)._2 + 1)
    output(3) = (output(0)._1, output(0)._2 + 1)
    output.map(x => ((x._2 - (w)), -x._1 + (h)))
  }

  def polar: Array[(Double, Double)] = {
    this.cartesian
      .map { case (x, y) => (sqrt(pow(x, 2) + pow(y, 2)), atan2(y, x)) }
  }

  def rgb: (Int, Int, Int) = {
    if(this.OoB){
      (0,0,0)
    } else {
    val raw = this.image.getRGB(index._1, index._2)
    val r = (raw >> 16) & 0xff
    val g = (raw >> 8) & 0xff
    val b = raw & 0xff
    (r, g, b)
    }
  }
}