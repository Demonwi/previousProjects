import hw.streams.Generator

object Main extends hw.streams.SolutionLike { // do not change this line

  // This helper function is very useful
  def cons[A](head: A, tail: =>Generator[A]): Generator[A] = new Generator[A] {
    def next() = (head, tail)
  }

  // The example from the project description
  val ones: Generator[Int] = cons(1, ones)

  def from(x: Int): Generator[Int] = cons(x, from(x + 1))

  def fromDoub(x: Double): Generator[Double] = cons(x, fromDoub(x + 1.0))

  def map[A,B](f: A => B, agen: Generator[A]): Generator[B] = {
    val (head, tail) = agen.next()
    cons(f(head), map[A,B](f, tail))
  }

  val pow: Generator[Int] = powConstruct(1)

  def powConstruct(i: Int): Generator[Int] = {
    cons(i, powConstruct(i*2))
  }

  def nth[A](agen: Generator[A], index: Int): A = {
    val (head, tail) = agen.next()
    if(index == 0){
      head
    }else{
      nth[A](tail, index-1)
    }
  }

  def filter[A](pred: (A) => Boolean, agen: Generator[A]): Generator[A] = {
    val (head, tail) = agen.next()
    if(pred(head)){
      cons(head, filter[A](pred, tail))
    }else{
      filter[A](pred, tail)
    }
  }

  def interleave[A](agen1: Generator[A], agen2: Generator[A]): Generator[A] = {
    val (head1, tail1) = agen1.next()
    val (head2, tail2) = agen2.next()
    cons(head1, cons(head2, interleave(tail1, tail2)))
  }

  def sift(n: Int, agen: Generator[Int]): Generator[Int] = {
    filter(x => !(x%n == 0), agen)
  }

  val prime: Generator[Int] = primeConstruct(from(2))

  def primeConstruct(agen: Generator[Int]): Generator[Int] = {
    val (head, tail) = agen.next()
    cons(head, primeConstruct(sift(head, tail)))
  }

  def total(agen: Generator[Double]): Generator[Double] = {
    totalHelp(0, agen)
  }

  def totalHelp(sum: Double, agen: Generator[Double]): Generator[Double] = {
    val (head, tail) = agen.next()
    val newSum = sum + head
    cons(newSum, totalHelp(newSum, tail))
  } 
}