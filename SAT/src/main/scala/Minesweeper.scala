object Minesweeper extends App {
  // Function to get the neighbors of a position
  def getNeighbors(grid: Array[Array[Int]], i: Int, j: Int): List[Int] = {
    val directions = List((-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1))
    directions
      .map { case (di, dj) => (i + di, j + dj) } // We add each value to our i and j
      .filter { case (ni, nj) => ni >= 0 && ni < n && nj >= 0 && nj < m } // We keep those who are in the rank
      .map { case (ni, nj) => grid(ni)(nj)} // We retrieve the literal
  }

  val e = new ScalAT("Minesweeper")

  // Pàgina per obtenir més buscamines -_- : https://www.janko.at/Raetsel/Minesweeper/index.htm
  // En el cas que hi hagi el nombre de mínes, s'indica a la part superior

  // Read Values
  val source = scala.io.Source.fromFile("testingFiles/Minesweeper/nr90")
  val lines = source.getLines().toList
  source.close()

  //Given values
  val Array(n, m, mines) = lines.head.split(" ").map(_.toInt)
  val entranceGrid = lines.tail.map(_.split(" "))

  //Auxiliar variable
  var emptyPositions: Array[Int] = Array()

  //Literals
  val grid = e.newVar2DArray(n,m)

  //Constrains
  // 1: For each position fo the grid, we look for those who have values and those who don't
  for(i <- 0 until n; j <- 0 until m){
    val tile = entranceGrid(i)(j)
    if(tile != "-"){
      //If the position has a value, there is no way a mine could be there
      e.addClause(-grid(i)(j) :: List())
      //We get the K neighbors of the tile grid(i)(j)
      val neighbors = getNeighbors(grid,i,j)
      //We know K mines have to be in those neighbors
      e.addEK(neighbors,tile.toInt)
    }
    //In case no value is in the tile, we store that position
    else emptyPositions = emptyPositions :+ grid(i)(j)
  }

  // 2: We restrict the number of mines the grid can have. We will only do so if the user entres the parameter.
  if(mines != -1) {
    e.addEK(emptyPositions.toList,mines)
  }

  //Result
  val result = e.solve()
  println(result)
  if (result.satisfiable) {
    for(i <- 0 until n){
      for(j <- 0 until m){
        val cellValue = if(e.getValue(grid(i)(j))) "o" else "X"
        print(cellValue + " ")
      }
      println()
    }
  }
}