import org.scalatest.FunSuite

import org.asolimando.hips._

class HIPSTester extends FunSuite {
  test("Bruteforce check computed solutions") {
      for(i <- 1 to 100000){
        if(i % 10000 == 0) println(i)
        val points = (1 to 15).map(_ => HIPS.generatePoint(100,100,10.0))
        HIPS.exec(points)
      }
  }
}
