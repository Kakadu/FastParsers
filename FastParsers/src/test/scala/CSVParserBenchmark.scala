/**
 * Created by Eric on 05.04.14.
 */

import fastparsers.input.InputWindow
import org.scalameter.api._
import CSVParsers._
import scala.collection.mutable.ListBuffer

import lms._
import InputWindow._

object CSVParserBenchmark extends PerformanceTest {

  lazy val executor = LocalExecutor(
    new Executor.Warmer.Default,
    Aggregator.min,
    new Measurer.Default)
  lazy val reporter = new LoggingReporter
  lazy val persistor = Persistor.None

  val range = Gen.enumeration("size")(10)

  val bigDoubleFileName = "FastParsers/src/test/resources/" + "csvDoubles.txt"
  val bigDoubleFile = scala.io.Source.fromFile(bigDoubleFileName).getLines mkString "\n"
  val bigDoubleFileArray = bigDoubleFile.toCharArray
  val bigDoubleFileSeq = new FastCharSequence(bigDoubleFileArray)

  val bigBoolFileName = "FastParsers/src/test/resources/" + "csvBooleans.txt"
  val bigBoolFile = scala.io.Source.fromFile(bigBoolFileName).getLines mkString "\n"
  val bigBoolFileArray = bigBoolFile.toCharArray
  val bigBoolFileSeq = new FastCharSequence(bigBoolFileArray)

  /*performance of "CSVDoublesParser:Double@FastParsers" in {
    measure method "value" in {
      using(range) in { j =>
        for (i <- 1 to j)
          cvsParser.doubles(bigDoubleFileArray)
      }
    }
  }

  performance of "CSVDoublesParser:Double@LMS" in {
    measure method "value" in {
      using(range) in { j =>
        for (i <- 1 to j)
          LMSCSVDoubleParserGen.apply(bigDoubleFileArray)
      }
    }
  }
  */
  performance of "CSVBooleanParser:Boolean" in {
    measure method "FastParsers" in {
      using(range) in { j =>
        for (i <- 1 to j)
          cvsParser.bools(bigBoolFileArray)
      }
    }
	
	measure method "LMS" in {
      using(range) in { j =>
        for (i <- 1 to j)
          LMSCSVBooleanParseGen.apply(bigBoolFileArray)
      }
    }
	
	measure method "Handwritten" in {
      using(range) in { j =>
        for (i <- 1 to j)
          CSVBoolHandWritten.apply(bigBoolFileArray)
      }
    }
  }
  
}