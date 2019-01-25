import hw.json._
import hw.wrangling.WranglingLike

object Wrangling extends WranglingLike {

  val data: List[Json] = JsonHelper.fromFile("yelp.json")

  def key(json: Json, key: String): Option[Json] = json match{
    case JsonDict(map) => {
      if(map.contains(JsonString(key))){
        map.get(JsonString(key))
      }else{
        None
      }
    }
    case _ => None
  }

  def fsHelper(json: Json, state: String): Boolean = json match{
    case JsonDict(map) => key(json, "state") match{
      case Some(JsonString(str)) => str == state
      case _ => false
    }
    case _ => false
  }

  def fromState(data: List[Json], state: String): List[Json] = {
    data.filter(datum => fsHelper(datum, state))
  }

  def rltHelper(json: Json, rating: Double): Boolean = json match{
    case JsonDict(map) => key(json, "rating") match{
      case Some(JsonNumber(num)) => num < rating
      case _ => false
    }
    case _ => false
  }

  def ratingLT(data: List[Json], rating: Double): List[Json] = {
    data.filter(datum => rltHelper(datum, rating))
  }

  def rgtHelper(json: Json, rating: Double): Boolean = json match{
    case JsonDict(map) => key(json, "rating") match{
      case Some(JsonNumber(num)) => num > rating
      case _ => false
    }
    case _ => false
  }

  def ratingGT(data: List[Json], rating: Double): List[Json] = {
    data.filter(datum => rgtHelper(datum, rating))
  }

  def catHelper(json: Json, category: String): Boolean = json match{
    case JsonDict(map) => key(json, "category") match{
      case Some(JsonString(str)) => str == category
      case _ => false
    }
    case _ => false
  }

  def category(data: List[Json], category: String): List[Json] = {
    data.filter(datum => catHelper(datum, category))
  }

  def groupByState(data: List[Json]): Map[String, List[Json]] = {
    data.groupBy(datum => datum match {
    case JsonDict(aMap) => aMap.get(JsonString("state")) match {
      case Some(JsonString(stateCode)) => stateCode
      case _ => "no state"
    }
    case _ => "no state"
  })
  }

  def groupByCategory(data: List[Json]): Map[String, List[Json]] = {
    data.groupBy(datum => datum match{
      case JsonDict(map) => map.get(JsonString("category")) match{
        case Some(JsonString(str)) => str
        case _ => "no category"
      }
      case _ => "no category"
    })
  }

  def betterOf(aPlace: Json, bPlace: Option[Json]): Option[Json] = {
    aPlace match{
      case JsonDict(aMap) => {
        bPlace match{
          case Some(JsonDict(bMap)) => aMap.get(JsonString("rating")) match{
            case Some(JsonNumber(aNum)) => bMap.get(JsonString("rating")) match {
              case Some(JsonNumber(bNum)) => {
                if(bNum > aNum){
                  bPlace
                }else{
                  Some(aPlace)
                }
              }
              case _ => None
            }
            case _ => None
          }
          case _ => Some(aPlace)
        }
      }
      case _ => None
    }
  }

  def bestPlace(data: List[Json]): Option[Json] = data match{
    case json :: rest => betterOf(json, bestPlace(rest))
    case Nil => None
  }

  def ambFilter(json: Json, ambience: String): Boolean = json match{
    case JsonDict(map) => map.get(JsonString("attributes")) match{
      case Some(JsonDict(aMap)) => aMap.get(JsonString("ambience")) match{
        case Some(JsonDict(bMap)) => {
          if(bMap.contains(JsonString(ambience))){
            bMap.get(JsonString(ambience)) match {
              case Some(JsonBool(someBool)) => someBool
              case _ => false
            }
          }else{
            false
          }
        }
        case _ => false
      }
      case _ => false
    }
    case _ => false
  }

  def hasAmbience(data: List[Json], ambience: String): List[Json] = {
    data.filter(datum => ambFilter(datum, ambience))
  }

}
