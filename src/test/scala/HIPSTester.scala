import org.scalatest.FunSuite

import com.github.asolimando.hips._

import java.util.Random

class HIPSTester extends FunSuite {
  test("Bruteforce check computed solutions") {

    val rnd = new Random(2)

    for(i <- 1 to 100000){
      if(i % 10000 == 0) println(i)
      val points = (1 to 15).map(_ => HIPS.generatePoint(100, 100, 10.0, rnd))
      HIPS.exec(points)
    }
  }
}
