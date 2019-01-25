import  hw.csv._

sealed trait Gender
case class Male() extends Gender
case class Female() extends Gender

case class SSARow(birthYear: Int, name: String, gender: Gender, count: Int)

case class CDCRow(birthYear: Int, maleLifeExpectancy: Int,
  femaleLifeExpectancy: Int)

object Main {

  def readSSARow(row: List[String]): SSARow = row match{
    case year :: yRest => yRest match{
      case name :: nRest => nRest match{
        case sex :: sRest => sRest match{
          case count :: Nil => {
            if(sex == "F"){
              SSARow(year.toInt, name, Female(), count.toInt)
            }else{
              SSARow(year.toInt, name, Male(), count.toInt)
            }
          }
          case _ => throw new IllegalArgumentException("List not of required format")
        }
        case _ => throw new IllegalArgumentException("List not of required format")
      }
      case _ => throw new IllegalArgumentException("List not of required format")
    }
    case _ => throw new IllegalArgumentException("List not of required format")
  }

  def readCDCRow(row: List[String]): CDCRow = row match{
    case year :: yRest => yRest match{
      case maleEx :: mRest => mRest match{
        case femaleEx :: Nil => CDCRow(year.toInt, maleEx.toInt, femaleEx.toInt)
        case _ => throw new IllegalArgumentException("List not of required format")
      }
      case _ => throw new IllegalArgumentException("List not of required format")
    }
    case _ => throw new IllegalArgumentException("List not of required format")
  }

  def yearIs(rows: List[SSARow], bound: Int): List[SSARow] = rows match{
    case first :: rest => {
      if(first.birthYear == bound){
        first :: yearIs(rest, bound)
      }else{
        yearIs(rest, bound)
      }
    }
    case Nil => Nil
  }

  def yearGT(rows: List[SSARow], bound: Int): List[SSARow] = rows match{
    case first :: rest => {
      if(first.birthYear > bound){
        first :: yearGT(rest, bound)
      }else{
        yearIs(rest, bound)
      }
    }
    case Nil => Nil
  }

  def yearLT(rows: List[SSARow], bound: Int): List[SSARow] = rows match{
    case first :: rest => {
      if(first.birthYear < bound){
        first :: yearLT(rest, bound)
      }else{
        yearIs(rest, bound)
      }
    }
    case Nil => Nil
  }

  def onlyName(rows: List[SSARow], name: String): List[SSARow] = rows match{
    case first :: rest => {
      if(first.name == name){
        first :: onlyName(rest, name)
      }else{
        onlyName(rest, name)
      }
    }
    case Nil => Nil
  }

  def sumName(rows: List[SSARow], name: String): Int = rows match{
    case first :: rest => {
      if(first.name == name){
        first.count + sumName(rest, name)
      }else{
        sumName(rest, name)
      }
    }
    case Nil => 0
  }

  def mpHelper(rows: List[SSARow], curName: String, curMax: Int): (String, Int) = rows match{
    case first :: rest => {
      if(curMax < sumName(rows, first.name)){
        mpHelper(rest, first.name, sumName(rows, first.name))
      }else{
        mpHelper(rest, curName, curMax)
      }
    }
    case Nil => (curName, curMax)
  }

  def mostPopular(rows: List[SSARow]): (String, Int) = rows match{
    case first :: rest => mpHelper(rest, first.name, sumName(rows, first.name))
    case _ => throw new IllegalArgumentException("List not of required format")
  }

  def count(rows: List[SSARow]): Int = rows match{
    case first :: rest => first.count + count(rows)
    case Nil => 0
  }

  def isMale(row: SSARow): Boolean = {
    row.gender == Male()
  }

  def isFemale(row: SSARow): Boolean = {
    row.gender == Female()
  }

  def countGirlsAndBoys(rows: List[SSARow]): (Int, Int) = {
    (count(rows.filter(datum => isMale(datum))), count(rows.filter(datum => isFemale(datum))))
  }

  def genNeuHelper(rows: List[SSARow], mName: String): Boolean = rows match{
    case first :: rest => {
      if((first.name == mName)&&(first.gender == Female())){
        true
      }else{
        genNeuHelper(rest, mName)
      }
    }
    case Nil => false
  }

  def nameExt(rows: List[SSARow]): Set[String] = rows match{
    case first :: rest => Set(first.name) ++ nameExt(rest)
    case Nil => Set("No") - "No"
  }

  def genderNeutralNames(rows: List[SSARow]): Set[String] = {
    nameExt(rows.filter(datum => genNeuHelper(rows, datum.name)))
  }

  def expMaleHelper(rows: List[CDCRow], birthYear: Int): Int = rows match{
    case first :: rest => {
      if(first.birthYear == birthYear){
        first.maleLifeExpectancy
      }else{
        expMaleHelper(rows, birthYear)
      }
    }
    case _ => throw new IllegalArgumentException("List not of required format")
  }

  def expFemaleHelper(rows: List[CDCRow], birthYear: Int): Int = rows match{
    case first :: rest => {
      if(first.birthYear == birthYear){
        first.femaleLifeExpectancy
      }else{
        expFemaleHelper(rows, birthYear)
      }
    }
    case _ => throw new IllegalArgumentException("List not of required format")
  }

  def expectedAlive(gender: Gender, birthYear: Int, currentYear: Int,
    lifeExpectancies: List[CDCRow]): Boolean = {
    if(gender == Male()){
      (currentYear - birthYear) <= expMaleHelper(lifeExpectancies, birthYear)
    }else{
      (currentYear - birthYear) <= expFemaleHelper(lifeExpectancies, birthYear)
    }
  }

  def sumCount(rows: List[SSARow]): Int = rows match{
    case first :: rest => first.count + sumCount(rest)
    case Nil => 0
  }

  def estimatePopulation(rows: List[SSARow], year: Int,
    lifeExpectancies: List[CDCRow]): Int = {
    sumCount(rows.filter(datum => expectedAlive(datum.gender, datum.birthYear, year, lifeExpectancies)))
  }

}