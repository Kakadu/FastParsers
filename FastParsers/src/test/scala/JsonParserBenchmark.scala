/**
 * Created by Eric on 05.04.14.
 */

import fastparsers.input.InputWindow
import org.scalameter.CurveData
import org.scalameter.api._
import JsonParsers._
import scala.collection.mutable.ListBuffer
import org.scalameter.api._
import org.scalameter.reporting.DsvReporter
import scala.collection.mutable.ArrayBuffer
import scala.reflect.runtime.universe._

import lms._
import InputWindow._

object JsonParserBenchmark extends PerformanceTest {

  lazy val executor = LocalExecutor(
    new Executor.Warmer.Default,
    Aggregator.min,
    new Measurer.Default)
  lazy val reporter = Reporter.Composite(LoggingReporter(), CsvReporter(',') )
  lazy val persistor = Persistor.None

  val range = Gen.enumeration("size")(10)

  val files = (1 to 5).foldLeft(new ListBuffer[Array[Char]]){ (acc,i) =>
    val filename = "FastParsers/src/test/resources/json" + i
    val data = scala.io.Source.fromFile(filename).getLines mkString "\n"
    acc.append(data.toCharArray)
    acc
  }.toList

  performance of "JsonParser_on_small_inputs" in {
    measure method "FastParsers" in {
      using(range) in { j =>
        for (i <- 1 to j; m <- files)
          JSonImplBoxed.jsonparser.value(m)
      }
    }

    // measure method "LMS (gen2)" in {
    //   using(range) in { j =>
    //     for (i <- 1 to j; m <- files)
    //       LMSJsonParserGen2.apply(m)
    //   }
    // }

    measure method "Combinators" in {
      using(range) in { j =>
        for (i <- 1 to j; m <- files)
          JSON.parse(JSON.value,new FastCharSequence(m))
      }
    }
  }


  val bigFileName = "FastParsers/src/test/resources/" + "json.big1"
  val bigFile = scala.io.Source.fromFile(bigFileName).getLines mkString "\n"
  val bigFileArray = bigFile.toCharArray
  val bigFileSeq = new FastCharSequence(bigFileArray)

  // val vbigFileName = "FastParsers/src/test/resources/" + "json.vbig"
  // val vbigFile = scala.io.Source.fromFile(vbigFileName).getLines mkString "\n"
  // val vbigFileArray = vbigFile.toCharArray
  // val vbigFileSeq = new FastCharSequence(vbigFileArray)

  /*
    performance of "JsonParser_on_big_inputs" in {
      measure method "FastParsers" in {
        using(range) in { j =>
          for (i <- 1 to j)
            JSonImplBoxed.jsonparser.value(bigFileArray)
        }
      }

  //    measure method "LMS (gen2)" in {
  //      using(range) in { j =>
  //        for (i <- 1 to j)
  //          LMSJsonParserGen2.apply(bigFileArray)
  //      }
  //    }

      measure method "Combinators" in {
        using(range) in { j =>
          for (i <- 1 to j)
            JSON.parse(JSON.value, bigFileSeq)
        }
      }

    }*/


  /*performance of "JsonParser on a very big input" in {
    measure method "FastParsers" in {
      using(range) in { j =>
        for (i <- 1 to j)
          JSonImpl2.jsonparser.value(vbigFileArray)
      }
    }

    measure method "Combinators" in {
      using(range) in { j =>
        for (i <- 1 to j)
          JSON.parse(JSON.value,vbigFileSeq)
      }
    }

    measure method "LMS (gen2)" in {
      using(range) in { j =>
        for (i <- 1 to j)
          LMSJsonParserGen2.apply(vbigFileArray)
      }
    }
  }*/

/*
  performance of "Different JSonParser implementations" in {
    measure method "FastParsers" in {
      using(range) in { j =>
        for (i <- 1 to j)
          JSonImpl2.jsonparser.value(bigFileArray)
      }
    }*/

    /*measure method "FastParsers Boxed" in {
      using(range) in { j =>
        for (i <- 1 to j)
          JSonImplBoxed.jsonparser.value(vbigFileArray)
      }
    }

    measure method "FastParsers no inline" in {
      using(range) in { j =>
        for (i <- 1 to j)
          JSonImpl3.jsonparser.value(vbigFileArray)
      }
    }
    measure method "FastParsers no inline with errors reporting" in {
      using(range) in { j =>
        for (i <- 1 to j)
          JSonImpl4.jsonparser.value(vbigFileArray)
      }
    }

    measure method "FastParsers no inline with errors reporting and ignore results" in {
      using(range) in { j =>
        for (i <- 1 to j)
          JSonImpl5.jsonparser.value(vbigFileArray)
      }
    }*/

    /*measure method "FastParsers InputWindow to String" in {
      using(range) in { j =>
        for (i <- 1 to j)
          JSonImpl6.jsonparser.value(vbigFileArray)
      }
    }*/

    /*measure method "FastParsers on string input" in {
      using(range) in { j =>
        for (i <- 1 to j)
          JSonImpl1.jsonparser.value(bigFile)
      }
    }*/

    /*measure method "FastParsers on bigFileSeq input" in {
      using(range) in { j =>
        //for (i <- 1 to j)
          JSON.parse(JSON.value,bigFileSeq)
      }
    }

    measure method "FastParsers on bigFile input" in {
      using(range) in { j =>
        //for (i <- 1 to j)
          JSON.parse(JSON.value,bigFile)
      }
    }

  }
     */
}

// https://github.com/amirsh/benchmarking-scala-manifests/blob/master/TestMan.scala
case class CsvReporter(delimiter: Char) extends Reporter {
  import org.scalameter._
  import java.io._
  import java.util.Date
  import java.util.TimeZone
  import java.text.SimpleDateFormat
  import utils.Tree
  val sep = File.separator
  val resultDir = "csv_results"

  def report(result: CurveData, persistor: Persistor) {
    println("simple report: " + result.context.scope)
    new File(s"$resultDir").mkdir()
    val filename = s"$resultDir$sep${result.context.scope}.csv"

     var writer: PrintWriter = null
     try {
        writer = new PrintWriter(new FileWriter(filename, false))
        // writer = System.out
        result.measurements foreach { (m: Measurement) =>
          val nanos = m.data.complete map { x => x*1000 }
          writer.println( nanos.mkString(",") )
        }
      } finally {
        if (writer != null) writer.close()
      }


  }
  def report(result: Tree[CurveData], persistor: Persistor) = {
    true
    /*
    println("report: ")
    val currentDate = new Date
    val resultdir = "csv_results"

    def getHeadCurveData(tree: Tree[CurveData]): CurveData = {
      tree.items.headOption.getOrElse({
        tree.children.filter(t => getHeadCurveData(t) != null).headOption.map(getHeadCurveData).getOrElse(null)
      }
      )
    }
    val headCurveData = getHeadCurveData(result)
    new File(s"$resultdir").mkdir()
    val filename = s"$resultdir$sep${headCurveData.context.scope}.csv"
    def print() {
      var writer: PrintWriter = null
      try {
        writer = new PrintWriter(new FileWriter(filename, false))
        // writer = System.out
        writeData(writer)
      } finally {
        if (writer != null) writer.close()
      }
    }
    def writeData(pw: PrintWriter) {
      var tabular = new ArrayBuffer[List[Any]]
      def header(cd: CurveData) = {
        // "Method" + delimiter + cd.measurements.map(_.params.axisData.head._2).mkString(delimiter.toString)
        tabular += List("Method") ++ cd.measurements.map(_.params.axisData.head._2)
      }
      def row(cd: CurveData) = {
        // cd.context.curve + delimiter + cd.measurements.map(m => m.value).mkString(delimiter.toString)
        tabular += List(cd.context.curve) ++ cd.measurements.map(_.value)
      }
      // def header(cd: CurveData) = {
      // val m = cd.measurements.head
      // "Method" + delimiter + m.params.axisData.head._1 + delimiter + s"Time (${m.units})"
      // }
      //
      // def row(cd: CurveData) = {
      // cd.measurements.map(m => cd.context.curve + delimiter + m.params.axisData.head._2 + delimiter + m.value).mkString("\n")
      // }
      // pw.println(header(headCurveData))
      // result foreach { cd =>
      // pw.println(row(cd))
      //
      // }
      header(headCurveData)
      result foreach row
      tabular.toList.transpose foreach { line =>
        pw.println(line.mkString(delimiter.toString))
      }
    }
    print()
    true
    */
  }
}
