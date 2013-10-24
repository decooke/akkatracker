/**
 * D.Cooke  9/13/13 5:02 PM
 */

// Akka project creation notes

// go to File | Project Structure | Libraries
// add following jars to scala-library classes section:
// add akka-actor jar via hidden .ivy directory
// add akka config jar via .ivy but from higher directory (not actor)

// then edit run configuration
// make Main class akka.Main
// make program Argument the top class / entry point of this file
// working directory should be full path to this project

import akka.actor.{Actor, Props}
import collection.mutable.ArrayBuffer
import java.io.{FileOutputStream, FileInputStream}
import javax.swing.JFileChooser
import sun.audio.{AudioStream, AudioPlayer}

object Starter {
  case object Start
  case object Done
}

class Starter extends Actor {

  def receive = {
    case Starter.Start =>
      println("Akka Tracker started...")
      val monitor = context.actorOf(Props[Monitor])
      monitor ! Monitor.Start

      //Thread.sleep(1000)
      val commandReader = context.actorOf(Props[CommandReader])
      commandReader ! CommandReader.Start

    case Starter.Done =>
      context.parent ! Starter.Done
  }
}


object CommandReader {
  case object Start
}

class CommandReader extends Actor {

  def receive = {
    case CommandReader.Start =>
      print("Command >" )
      val line = Console.readLine()
      val audioFilePlayer = context.actorOf(Props[AudioFilePlayer])

      line match {
        case "p" | "play" => {
          context.watch(audioFilePlayer)
          audioFilePlayer ! AudioFilePlayer.Start
          self ! CommandReader.Start
        }

        case "r" | "read" => {
          val wavFileReader = context.actorOf(Props[WavFileReader])
          wavFileReader ! WavFileReader.Start
          self ! CommandReader.Start
        }

        case "t" | "tweak" => {
          val wavFileWriter = context.actorOf(Props[WavFileWriter])
          wavFileWriter ! WavFileWriter.Tweak
          self ! CommandReader.Start
        }

        case "s" | "sine" => {
          val wavFileWriter = context.actorOf(Props[WavFileWriter])
          wavFileWriter ! WavFileWriter.SineTone
          self ! CommandReader.Start
        }

        case "sq" | "square" => {
          val wavFileWriter = context.actorOf(Props[WavFileWriter])
          wavFileWriter ! WavFileWriter.fsSquareWave
          self ! CommandReader.Start
        }

        case "q" | "quit" | "exit" => {;
          context.stop(audioFilePlayer)
          context.parent ! Starter.Done
        }

        case  _ => self ! CommandReader.Start
      }
  }
}

object AudioFilePlayer {
  case object Start
}

class AudioFilePlayer extends Actor {

  def receive = {

    case AudioFilePlayer.Start =>
      val chooser: JFileChooser = new JFileChooser()
      chooser.showOpenDialog(null)
      val soundFile = chooser.getSelectedFile

      try {
        val as: AudioStream = new AudioStream(new FileInputStream(chooser.getName(soundFile)))
        val player: AudioPlayer = AudioPlayer.player
        player.start(as)
      }
      catch {
        case e: Exception => e.printStackTrace()
      }
    case AudioFilePlayer.Done =>



  }
}

object WavFileReader {
  case object Start
  val wavHeaderLength = 44
}

class WavFileReader extends Actor {

  def receive = {

    case WavFileReader.Start =>
      val chooser: JFileChooser = new JFileChooser()
      chooser.showOpenDialog(null)
      val soundFile = chooser.getSelectedFile

      val in = new FileInputStream(soundFile)
      val header = new Array[Byte](WavFileReader.wavHeaderLength)
      val samples = new Array[Byte](soundFile.length.toInt - WavFileReader.wavHeaderLength)
      var sampleValues = new ArrayBuffer[Int]
      var sampleDeltas = new ArrayBuffer[Int]

      in.read(header)
      in.read(samples)
      in.close()

      // .wav file samples here are stored as little endian 2's Complement 16 bit signed Integers
      // converting them to an Int array for easier processing
      for (enum <- samples.grouped(2))
        sampleValues += ((enum(0) & 0xff) | (enum(1) << 8))

      // get average sample absolute value
      val absoluteSampleValues = sampleValues.map(Math.abs(_))
      printf("%s%d\n", "The average sample absolute value is: ", absoluteSampleValues.sum / absoluteSampleValues.length)

      for (i <- 0 until sampleValues.length-1)
        sampleDeltas += Math.abs((sampleValues(i+1) - sampleValues(i)))

      val sampleChange = sampleDeltas.sum / sampleDeltas.length
      printf("%s%d\n", "The average sample change is: ", sampleChange)

   }
}


object WavFileWriter {
  case object Tweak
  case object SineTone
  case object fsSquareWave
  val wavHeaderLength = 44
}

class WavFileWriter extends Actor {

  def receive = {
    case WavFileWriter.Tweak =>
//      val chooser: JFileChooser = new JFileChooser()
//      chooser.showOpenDialog(null)
//      val soundFile = chooser.getSelectedFile
//
//      val in = new FileInputStream(soundFile)
//      val out = new FileOutputStream("tweaked.wav")
//      var header = new Array[Byte](WavFileWriter.wavHeaderLength)
//      val samples = new Array[Byte](soundFile.length.toInt - WavFileWriter.wavHeaderLength)
//
//      in.read(header)
//      in.read(samples)
//      in.close()
//
//      var newSamples = tweakSamples(samples)
//      newSamples = loopSamples(newSamples, 2)
//
//      header = updateHeader(header, newSamples.length)
//      out.write(header)
//      out.write(newSamples)
//      out.close()

    case WavFileWriter.SineTone =>
      val out = new FileOutputStream("mysine.wav")
      val in = new FileInputStream("wavheader.wav")
      var header = new Array[Byte](WavFileWriter.wavHeaderLength)

      in.read(header)
      in.close()

      val sampleValues = makeSineTone(1000, 0.9, 2)
      val newSamples = convertSamples(sampleValues)

      header = updateHeader(header, newSamples.length)
      out.write(header)
      out.write(newSamples)
      out.close()

    case WavFileWriter.fsSquareWave =>
      val out = new FileOutputStream("mysquare.wav")
      val in = new FileInputStream("wavheader.wav")
      var header = new Array[Byte](WavFileWriter.wavHeaderLength)

      in.read(header)
      in.close()

      val firstSamples = makeSineTone(500, 0.6, 1)
      val secondSamples = makeSineTone(1500, 0.2, 1)
      val thirdSamples = makeSineTone(2500, 0.12, 1)
      val fourthSamples = makeSineTone(3500, 0.088, 1)
      val fifthSamples = makeSineTone(4500, 0.055, 1)

      for (i <- 0 until firstSamples.length) {
        firstSamples(i) = firstSamples(i) + secondSamples(i) + thirdSamples(i) + fourthSamples(i) + fifthSamples(i)
      }

      val finalSamples = convertSamples(firstSamples)

      header = updateHeader(header, finalSamples.length)
      out.write(header)
      out.write(finalSamples)
      out.close()
  }

//   def tweakSamples(samples: Array[Byte]): Array[Byte] = {
//
//     var sampleValues = ArrayBuffer[Int]()
//
//      for (enum <- samples.grouped(2))
//       sampleValues += ((enum(0) & 0xff) | (enum(1) << 8))
//
//     // tweak the samples
//     val tweakedSamples = sampleValues.map(_ * 0.5)
//
//     val newSamples = convertSamples(tweakedSamples)
//     newSamples
//   }

  def makeSineTone(frequency: Double, amplitude: Double, duration: Double): ArrayBuffer[Int] = {

    var sampleValues = ArrayBuffer[Int]()

    // a 1000 Hz sine tone at 44100 samples per second has 4410 samples in one tenth of a second
    // and 100 cycles of the sine wave, or 44.1 samples per cycle
    val degreesPerSample = (360 / 44.1) * (frequency / 1000)
    var degrees = 0: Double
    val numSamples = duration * 44100
    val ampLevel = 32768 * amplitude
    var s:Double = 0

    while (s < numSamples) {
      sampleValues += (Math.sin(degrees.toRadians) * ampLevel).toInt
      degrees += degreesPerSample
      if (degrees >= 360)
        degrees -= 360
      s+=1
    }

    sampleValues
  }

  def convertSamples(sampleValues: ArrayBuffer[Int]): Array[Byte] = {
    val newSamples = ArrayBuffer[Byte]()

    for (i <- 0 until sampleValues.length) {
      val newSample = new Array[Byte](2)
      newSample(0) = (sampleValues(i).toInt & 0xff).toByte
      newSample(1) = ((sampleValues(i).toInt >> 8) & 0xff).toByte
      newSamples += newSample(0)
      newSamples += newSample(1)
    }
    newSamples.toArray
  }

  def loopSamples(samples: Array[Byte], numLoops: Int): Array[Byte] = {

    var newSamples = ArrayBuffer[Byte]()

    for (i <- 0 until numLoops) {
      for (j <- 0 until samples.length)
        newSamples += samples(j)
    }
    newSamples.toArray
  }

  def updateHeader(header: Array[Byte], numSamples:Int): Array[Byte] = {

    header(40) = (numSamples & 0x000000ff).toByte
    header(41) = ((numSamples & 0x0000ff00) >> 8).toByte
    header(42) = ((numSamples & 0x00ff0000) >> 16).toByte
    header(43) = ((numSamples & 0xff000000) >> 24).toByte
    header
  }
}

object Monitor {
  case object Start
}

class Monitor extends Actor {

  def receive = {
    case Monitor.Start =>
     // println("Fired up the Monitor Actor")
      // start the listener Actor
      val listener = context.actorOf(Props[Listener])
      listener ! Listener.Start
  }
}

object Listener {
  case object Start
}

class Listener extends Actor {

  def receive = {
    case Listener.Start =>
      //println("Fired up the Listener Actor")
  }
}

class AkkaTracker2 extends Actor {

  override def preStart() {
    // create the top level starter actor
    val starter = context.actorOf(Props[Starter])
    starter ! Starter.Start
  }

  def receive = {
    // when the starter is done, stop this actor and with it the application
    case Starter.Done => context.stop(self)
  }
}






