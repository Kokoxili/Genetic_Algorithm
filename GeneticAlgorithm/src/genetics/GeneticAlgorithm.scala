package genetics

object GeneticAlgorithm {


  def generatingNumber(number: Int): List[Double] = {
    val lst: List[Double] = for (i <- (1 to number).toList) yield {
      val elem = scala.util.Random.between(-100, 100).toDouble
      elem
    }
    lst
  }


  def findAnimal (animaLst: List[List[Double]]): List[List[Double]] = {
    val animlNum = animaLst.slice(0, 60)
    animlNum
  }

  def mutAtion(animLst: List[List[Double]]): List[List[Double]] = {
    if (animLst == List()) {
      List()
    }
    else {
      val elem = scala.util.Random
      val newAnimal = for (i <- animLst.apply(0)) yield {
        i + elem.nextFloat()
      }
      val fnalst = List(newAnimal) ++ mutAtion(animLst.drop(1))
      fnalst
    }
  }

  def crossOver(animaLst: List[List[Double]]): List[List[Double]] = {

    if (animaLst.isEmpty) {
      List()
    }
    else {
      val newanimal1 = animaLst.drop(1)
      val newanimal2 = newanimal1.drop(1)

      val offspring: List[Double] = for (i <- (0 to animaLst.apply(0).length - 1).toList) yield {
        (animaLst.apply(0).apply(i) + animaLst.apply(1).apply(i))/2
      }
      val fnalst = List(offspring) ++ crossOver(newanimal2)
      fnalst
    }
  }

  def recuranimal[T](container: List[Double] => T, costFunction: T => Double, geneNumber: Int, animalNum: List[List[Double]], offSpring: Int = 1): List[List[Double]] = {

    val costfunction: List[Double] => Double = (x: List[Double]) => costFunction(container(x))
    val animLst: List[List[Double]] = animalNum.sortBy(costfunction)
    val findanimal: List[List[Double]] = findAnimal(animLst)
    val newanimal: List[List[Double]] = crossOver(findanimal)
    val mutation1: List[List[Double]] = mutAtion(newanimal)

    if (offSpring == 400){
      animalNum
    }

    else {
      val offspring = offSpring + 1

      val animalP1: List[List[Double]] = findanimal ++ newanimal ++ mutation1
      val boanimal = animalNum.length - animalP1.length
      val animalP2: List[List[Double]] = for (i <- (0 until boanimal).toList) yield {
        val geneNum: List[Double] = generatingNumber(geneNumber)
        geneNum
      }
      val animalNumber: List[List[Double]] = animalP1 ++ animalP2
      val finl = recuranimal(container, costFunction, geneNumber, animalNumber, offspring)
      finl
    }
  }


  def geneticAlgorithm[T](incubator: List[Double] => T, costFunction: T => Double, numberOfGenes: Int): T = {
    val animalNumm: List[List[Double]] = for (i <- ((0 until (100)).toList)) yield {
      generatingNumber(numberOfGenes)

    }
    incubator(recuranimal(incubator, costFunction, numberOfGenes, animalNumm)(0))
    // TODO: Application Objective
  }


}

