import javax.imageio.ImageIO
import java.awt.image.BufferedImage
import java.io._
import scala.util.{ Try, Success, Failure }
import scala.math._


object Converter extends App {
  private var imageSize = 16
  private var ticks = 45
  private var leds = 8
  private var led_separation = 1
  private var led_displacement = 4

  def convert(): Unit = {
    var potImg: Option[BufferedImage] = None
    do {
      val imageLocation = readLine("Enter image location: ")
      potImg = this.getImage(imageLocation)
    } while (!potImg.isDefined)
    val img = potImg.get
    if (img.getWidth() != img.getHeight()) {
      println("Image must be square.")
      this.convert()
    } else {
      val pixels = this.pixelList(img)
      val ledPixels = Array.ofDim[Pixel](ticks, leds)
      val nOfIterations = leds * ticks
      for (t <- 0 to ticks - 1) {
        for (l <- 0 to leds - 1) {
          val polar = ((l + 1) * led_separation, (((2 * scala.math.Pi) / ticks) * t))
          val cartesian = (polar._1 * cos(polar._2).floor,polar._1 * sin(polar._2).floor)
          val matrixCoordinates = (((this.imageSize/2)-cartesian._2),(((this.imageSize/2)-1) + cartesian._1))
          val idx = (matrixCoordinates._1 * 16 + matrixCoordinates._2).toInt
          var currentPixel: Option[Pixel] = pixels.lift(idx)
          if(!currentPixel.isDefined){
            println("Could not find pixel with " + cartesian)
          }
          ledPixels(t)(l) = currentPixel.getOrElse(new Pixel((1, 1), imageSize, img,true))
        }

        println("Progress: " + ((t + 1).toDouble/ticks.toDouble * 100.0).toString.take(5) + " %.")
      }
      val instructions = ledPixels.map(_.map(_.rgb))
      var string = "{"
      for (line <- instructions) {
        string = string + "{" + (line.mkString(",")) + "}" + "," 
      }
      string = string.replace('(', '{').replace(')', '}').dropRight(1) + "}"
      println("Conversion done.")
      val fileLocation = readLine("Enter output file name: ")
      this.writeToFile(fileLocation, string)
    }
  }

  def pixelList(img: BufferedImage) = {
    val size = img.getHeight()
    var output = Seq[Pixel]()
    for (x <- 0 until size) {
      for (y <- 0 until size) {
        output = output :+ new Pixel((x, y), size, img,false)
      }
    }
    println(output.map(_.rgb))
    output
  }

  def getImage(location: String): Option[BufferedImage] = {
    var out: Option[BufferedImage] = None
    val trial = Try(ImageIO.read(new File(location)))
    trial match {
      case Success(img) => out = Some(img)
      case Failure(f) => println(f)
    }
    out
  }

  def writeToFile(fileName: String, toBeWritten: String) = this.synchronized {
    try {
      val writer = new FileWriter(fileName, true)
      writer.write(toBeWritten)
      writer.close()
      println("Wrote " + toBeWritten.length() + " characters to file " + fileName)
    } catch {
      case nofile: FileNotFoundException => println("No such file: " + fileName)
    }
  }

  this.convert()
}