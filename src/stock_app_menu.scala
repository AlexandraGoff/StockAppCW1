package example

import scala.collection.immutable.Nil.{flatMap, head, headOption}
import scala.collection.immutable.{ListMap, SortedMap}
import scala.io.Source
import scala.sys.SystemProperties.headless.key

object StockCalculator extends App {
  //read stock data from file
  val mapdata = readFile("data.txt")

  //Reads Data File From Data.txt
  def readFile(filename: String): Map[String, Int] = {
    // create buffer to build up map as we read each line
    var mapBuffer: Map[String, Int] = Map()
    try {
      for (line <- Source.fromFile(filename).getLines()) {
        // for each line
        val splitline = line.split(",").map(_.trim).toList // split line at , and convert to List

        // add element to map buffer
        // splitline is line from file as List, e.g. List(Bayern Munich, 24)
        // use head as key
        // tail is a list, but need just the first (only in this case) element, so use head of tail and convert to int
        mapBuffer = mapBuffer ++ Map(splitline.head -> splitline.tail.head.toInt)

      }
    } catch {
      case ex: Exception => println("Sorry, an exception happened.")
    }
    mapBuffer
  }

  def currentStockPrice(lmap: Map[String, List[Int]]) {
    val mp = ListMap(lmap.toSeq.sortWith(_._1 < _._1): _*)
    mp foreach { case (key, value) => println("The current stock price of " + key + " is : " + value.last) }
  }

  def maxMinPrices(lmap: Map[String, List[Int]]) {
    val mp = ListMap(lmap.toSeq.sortWith(_._1 < _._1): _*)
    mp foreach { case (key, value) => println(key + " : " + "Highest price : " + value.max + " Lowest Price : " + value.min) }
  }

  def findMedian(lmap: Map[String, List[Int]]): Unit = {
    def median(lmap: Map[String, List[Int]]) {
      val mp = ListMap(lmap.toSeq.sortWith(_._1 < _._1): _*)
      mp foreach {
        case (key, value)
        => if (value.sorted.size % 2 == 0) {
          println(value.sorted)
          val (up, down) = value.sorted.splitAt(value.size / 2)
          println(((up.last + down.head) / 2).toDouble)
        }
        else {
          println(key + " => " + value.sorted.drop(value.length / 2).head)
        }
      }
    }
    median(lmap)
  }


  /*def getSymbolForWeek(lmap: Map[String, Int]): Unit = {
   val mp = ListMap(lmap.toSeq.sortWith(_._1 < _._1): _*)
   var xy = SortedMap[String, Int]()
    mp foreach {
      case (key, value)
      => xy = SortedMap(key -> (value.drop(23).last - value.drop(23).head)) ++ xy
    }
    println(xy.maxBy(stock => stock._2))
  }

  getSymbolForWeek(mapdata)*/

  /*def getInputUser(): Unit = {
    println("Select the first stock : ")
    val x = scala.io.StdIn.readLine().toUpperCase()
    println("Second stock :")
    val y = scala.io.StdIn.readLine().toUpperCase()
    return x
  }*/

  def compareMeanValues(lmap: Map[String,Int]) = {
    val mp = ListMap(lmap.toSeq.sortWith(_._1 < _._1): _*)
    println("Select the first stock : ")
    val x = scala.io.StdIn.readLine().toUpperCase()
    println("Second stock :")
    val y = scala.io.StdIn.readLine().toUpperCase()
    println("The average price of " + x + " is " + getAverage(mp.get(x)) + " and " + y + " is " + getAverage(mp(y)))

  }
  compareMeanValues(mapdata)
    def getAverage(list: List[Int]): Double = {
      var average = list.foldLeft(0.0)(_ + _) / list.foldLeft(0)((r, c) => r + 1)
      average = scala.BigDecimal(average).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
      return average
    }

    // Let the user input their own portfolio and add shared held.
    def createPortfolio() = {
      var xy = SortedMap[String, Int]()
      println("Enter the stock symbol :")
      val stockName = scala.io.StdIn.readLine().toUpperCase()
      println("Enter the number of shares : ")
      val shares = scala.io.StdIn.readInt()
      val valList = mapdata(stockName)
      println(valList)
      val currentValue = shares * valList.last
      println(currentValue)
      xy = SortedMap(stockName -> currentValue) ++ xy
    }
  }

