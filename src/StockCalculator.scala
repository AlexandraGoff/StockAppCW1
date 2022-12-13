import scala.collection.immutable.{ListMap, SortedMap}
import scala.io.Source
import scala.util.control.Breaks.break

object StockCalculator extends App {
  // Get map data from data.txt file
  val mapdata = readFile("src/data.txt")
  printMenu()
  runMenu()

  // MENU FUNCTIONS ----------------------------------------------------------------------------------------------------
  //Prints the menu
  def printMenu() {
    println(
      """Please select one of the following options:
        --------------------------------------------------------------
        | 1. Get current stock prices                                |
        | -----------------------------------------------------------|
        | 2. Get the highest and lowest prices for each stock        |
        | -----------------------------------------------------------|
        | 3. Get the median price for each stock                     |
        | -----------------------------------------------------------|
        | 4. Find the stock which has risen most over the last week  |
        | -----------------------------------------------------------|
        | 5. Compare the average values over the period of two stocks|
        | -----------------------------------------------------------|
        | 6. Input a portfolio                                       |
        |------------------------------------------------------------|
        | Type 'exit' to quit.
        --------------------------------------------------------------""")
  }
// Allows the user to select a menu option
  def runMenu() {
    try {
      Iterator.continually(io.StdIn.readLine().toLowerCase())
        .takeWhile(_ != "exit")
        .foreach {
          case "1" => handleOne()
          case "2" => handleTwo()
          case "3" => handleThree()
          case "4" => handleFour()
          case "5" => handleFive()
          case "6" => handleSix
        }
    } catch {
      case e: Exception
      => println("That command isn't recognized. Please try again.")
        runMenu()
    }
  }

  // MENU HANDLERS
  def handleOne() {
    currentStockPrice(mapdata)
  }

  def handleTwo() {
    maxMinPrices(mapdata)
  }

  def handleThree() {
    findMedian(mapdata)
  }

  def handleFour() {
    getSymbolForWeek(mapdata)
  }

  def handleFive() {
    compareMeanValues(mapdata)
  }

  def handleSix {
    val pf = createPortfolio()
    // val pfMap = for {
    // x <- pf.keys
    // y <- pf.get(x)
    // } yield(x + " -> " + ((mapdata.get(x)) * (y)))
    // print(pf)
    //Doesnt work but shows the logic
    break()
  }

  // ANALYSIS & UTILITY FUNCTIONS ---------------------------------------------------------------------------------------
  //Read the data.txt file and transform it into a map
  def readFile(filename: String): Map[String, List[Int]] = {
    var mapBuffer: Map[String, List[Int]] = Map()
    try {
      for (line <- Source.fromFile(filename).getLines()) {
        val splitline = line.split(",").map(_.trim).toList
        val tailList = for {x <- splitline.tail} yield (x.toInt)
        mapBuffer = Map(splitline.head -> tailList) ++ mapBuffer
        mapBuffer = ListMap(mapBuffer.toSeq.sortWith(_._1 < _._1): _*)
      }
    } catch {
      case ex: Exception => println("Sorry, an exception happened.")
    }
    mapBuffer
  }
// Find the current stock prices
  def currentStockPrice(mp: Map[String, List[Int]]) {
    mp foreach { case (key, value) => println("The current stock price of " + key + " is : " + value.last) }
  }
//Find the lowest and highest values for each stock
  def maxMinPrices(mp: Map[String, List[Int]]) {
    mp foreach { case (key, value) => println(key + " : " + "Highest price : " + value.max + " Lowest Price : " + value.min) }
  }
//Find the Median value for all stocks
  def findMedian(mp: Map[String, List[Int]]): Unit = {
    def median(mp: Map[String, List[Int]]) {
      mp foreach {
        case (key, value)
        => if (value.sorted.size % 2 == 0) {
          val (up, down) = value.sorted.splitAt(value.size / 2)
          println(key + " " + ((up.last + down.head) / 2).toDouble)
        }
        else {
          println(key + " => " + value.sorted.drop(value.length / 2).head)
        }
      }
    }
    median(mp)
  }
//Get symbol for the week with the highest rise
  def getSymbolForWeek(lmap: Map[String, List[Int]]): Unit = {
    var xy = SortedMap[String, Int]()
    lmap foreach {
      case (key, value)
      => xy = SortedMap(key -> (value.drop(23).last - value.drop(23).head)) ++ xy
    }
    println(xy.maxBy(stock => stock._2))
  }

  //Compare two average values selected by the user
  def compareMeanValues(lmap: Map[String, List[Int]]) = {
    println("Select the first stock : ")
    val x = scala.io.StdIn.readLine().toUpperCase()
    println("Second stock :")
    val y = scala.io.StdIn.readLine().toUpperCase()
    println("The average price of " + x + " is " + getAverage(lmap(x)) + " and " + y + " is " + getAverage(lmap(y)))

  }

  // Calculate average value
  def getAverage(list: List[Int]): Double = {
    var average = list.foldLeft(0.0)(_ + _) / list.foldLeft(0)((r, c) => r + 1)
    average = scala.BigDecimal(average).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
    return average
  }

  // Let the user input their own portfolio and add shared held.

  def createPortfolio(): Map[String, Int] = {
    println("Please enter stock and values separated by `=`, each pair on its own line. Press ^D when done.")
    val map = Source
      .fromInputStream(System.in)
      .getLines.map(_.split("="))
      .map { case Array(a, b) => a -> b.toInt }
      .toMap
    map
  }
}
