object HOF {

  def map2[A,B,C](f: (A, B) => C, lst1: List[A], lst2: List[B]): List[C] = {
  	lst1 match{
  		case Nil => Nil
  		case a :: aTail => lst2 match{
  			case Nil => Nil
  			case b :: bTail => f(a,b) :: map2(f, aTail, bTail)
  		}
  	}
  }

  def zip[A,B](lst1: List[A], lst2: List[B]): List[(A, B)] = {
  	lst1 match{
  		case Nil => Nil
  		case a :: aTail => lst2 match{
  			case Nil => Nil
  			case b :: bTail => (a,b) :: zip(aTail, bTail)
  		}
  	}
  }

  def concat[A](listA: List[A], listB: List[A]): List[A] = {
  	listA match{
  		case Nil => listB
  		case head :: tail => head :: concat(tail, listB)
  	}
  }

  def flatten[A](lst: List[List[A]]): List[A] = {
  	lst match{
  		case Nil => Nil
  		case aList :: tList => concat(aList, flatten(tList))
  	}
  }

  def flatten3[A](lst: List[List[List[A]]]): List[A] = {
  	lst match{
  		case Nil => Nil
  		case aList :: tList => concat(flatten(aList), flatten3(tList))
  	}
  }

  def buildList[A](length: Int, f: Int => A): List[A] = {
  	buildHelper(0, length, f)
  }

  def buildHelper[A](ind: Int, length: Int, f: Int => A): List[A] = {
  	if(ind < length){
  		f(ind) :: buildHelper(ind+1, length, f)
  	}else{
  		Nil
  	}
  }

  def mapList[A, B](lst: List[A], f: A => List[B]): List[B] = {
  	lst match{
  		case Nil => Nil
  		case head :: tail => concat(f(head), mapList(tail, f))
  	}
  }

  def filter[A](f: A => Boolean, lst: List[A]): List[A] = {
  	lst match{
  		case Nil => Nil
  		case head :: tail => {
  			if(f(head)){
  				head :: filter(f, tail)
  			}else{
  				filter(f, tail)
  			}
  		}
  	}
  }

  def antifilter[A](f: A => Boolean, lst: List[A]): List[A] = {
  	lst match{
  		case Nil => Nil
  		case head :: tail => {
  			if(f(head)){
  				antifilter(f, tail)
  			}else{
  				head :: antifilter(f, tail)
  			}
  		}
  	}
  }

  def partition[A](f: A => Boolean, lst: List[A]): (List[A], List[A]) = {
  	(filter(f, lst), antifilter(f, lst))
  }

  def merge[A](lessThan: (A, A) => Boolean, alist1: List[A], alist2: List[A]): List[A] = {
  	alist1 match{
  		case Nil => alist2
  		case a :: aTail => {
  			alist2 match{
  				case Nil => alist1
  				case b :: bTail => {
  					if(lessThan(a,b)){
  						a :: merge(lessThan, aTail, alist2)
  					}else{
  						b :: merge(lessThan, alist1, bTail)
  					}
  				}
  			}
  		}
  	}
  }

  def sort[A](lessThan: (A, A) => Boolean, alist: List[A]): List[A] = {
  	 alist match{
  	 	case Nil => Nil
  	 	case head :: tail => merge(lessThan, head::Nil, sort(lessThan, tail))
  	 }
  }

}