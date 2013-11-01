/**
 * D.Cooke  9/13/13 5:02 PM
 */


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

        case "q" | "quit" | "exit" => {
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
  case object SineTone
  case object fsSquareWave
  val wavHeaderLength = 44
}

class WavFileWriter extends Actor {

  def receive = {

    case WavFileWriter.SineTone =>
      val out = new FileOutputStream("mysine.wav")
      val headerObj = WaveHeader(WaveHeader.mono)
      var header = headerObj.header.toArray

      val sampleValues = makeSineTone(1000, 0.9, 1, 1)
      val newSamples = convertSamples(sampleValues)

      header = headerObj.updateHeader(header, sampleValues.length)
      out.write(header)
      out.write(newSamples)
      out.close()

    case WavFileWriter.fsSquareWave =>
      val out = new FileOutputStream("mysquare.wav")
      val headerObj = WaveHeader(WaveHeader.stereo)
      var header = headerObj.header.toArray

      val firstSamples = makeSineTone(500, 0.6, 1, 2)
      val secondSamples = makeSineTone(1500, 0.2, 1, 2)
      val thirdSamples = makeSineTone(2500, 0.12, 1, 2)
      val fourthSamples = makeSineTone(3500, 0.088, 1, 2)
      val fifthSamples = makeSineTone(4500, 0.055, 1, 2)

      for (i <- 0 until firstSamples.length) {
        firstSamples(i) = firstSamples(i) + secondSamples(i) + thirdSamples(i) + fourthSamples(i) + fifthSamples(i)
      }

      val finalSamples = convertSamples(firstSamples)

      header = headerObj.updateHeader(header, firstSamples.length)
      out.write(header)
      out.write(finalSamples)
      out.close()
  }

  def makeSineTone(frequency: Double, amplitude: Double, duration: Double, numChannels: Int): ArrayBuffer[Int] = {

    var sampleValues = ArrayBuffer[Int]()
    val bytesPerSample = 2

    // a 1000 Hz mono sine tone at 44100 samples per second has 4410 samples in one tenth of a second
    // and 100 cycles of the sine wave, or 44.1 samples per cycle
    val degreesPerSample = ((360 / 44.1) * (frequency / 1000)) / numChannels
    var degrees = 0: Double
    val numSamples = duration * numChannels * 44100 * bytesPerSample
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
      newSample(0) = (sampleValues(i) & 0xff).toByte
      newSample(1) = ((sampleValues(i) >> 8) & 0xff).toByte
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

object WaveHeader {
  val mono = "0100"
  val stereo = "0200"

  def apply(numChannels: String) = {
    new WaveHeader(numChannels)
  }
}

// adapted from ccrma.stanford.edu/courses/422/projects/WaveFormat/
// & tmyymmt.net/    (hex2bytes)
class WaveHeader(val numChannels:String) {

  var header = new ArrayBuffer[Byte](44)

  val chunkID = "52494646" //"RIFF"
  var chunkSize = "00000000"  // 36 + subChunk2Size
  val format = "57415645"   //"WAVE"
  val subChunk1ID = "666d7420"   //"fmt"
  val subChunk1Size = "10000000" // 16 for PCM
  val audioFormat = "0100" // 1 indicates PCM
  // var numChannels = "0100" // 1 = mono, 2 = stereo
  var sampleRate = "44AC0000" // 44100 Hz default
  var byteRate = "88580100" // sampleRate * numChannels * bitsPerSample/8
  var blockAlign = "0200"  // numChannels * bitsPerSample/8
  var bitsPerSample = "1000" // 8 bits = 8, 16 bits = 16, etc.
  val subChunk2ID = "64617461"   //"data"
  var subChunk2Size = "00000000" //  numSamples * numChannels * bitsPerSample/8 --- number of bytes in the data.

  if (numChannels.matches(WaveHeader.stereo)) {
    byteRate = "10B10200"
    blockAlign = "0400"
  }

  val headerHexString = chunkID + chunkSize + format + subChunk1ID + subChunk1Size + audioFormat + numChannels +
    sampleRate + byteRate + blockAlign + bitsPerSample + subChunk2ID + subChunk2Size

  header ++= hex2bytes(headerHexString)

  def hex2bytes(hex: String): Array[Byte] = {
    hex.sliding(2,2).toArray.map(Integer.parseInt(_, 16).toByte)
  }

  def updateHeader(header: Array[Byte], numSamples:Int): Array[Byte] = {
    // update chunkSize field
    val chunkSize = numSamples + 36
    header(4) = (chunkSize & 0x000000ff).toByte
    header(5) = ((chunkSize & 0x0000ff00) >> 8).toByte
    header(6) = ((chunkSize & 0x00ff0000) >> 16).toByte
    header(7) = ((chunkSize & 0xff000000) >> 24).toByte

    // update subChunk2Size
    header(40) = (numSamples & 0x000000ff).toByte
    header(41) = ((numSamples & 0x0000ff00) >> 8).toByte
    header(42) = ((numSamples & 0x00ff0000) >> 16).toByte
    header(43) = ((numSamples & 0xff000000) >> 24).toByte
    header
  }
}
