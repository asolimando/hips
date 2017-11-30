import org.scalatest.FunSuite

import com.github.asolimando.hips._

import java.util.concurrent.atomic._
import java.util.Random

class HIPSTester extends FunSuite {
  test("Bruteforce check computed solutions") {

    val rnd = new Random(2)
    val c = new AtomicInteger(0)

    (1 to 100000).par.foreach{i =>
      val j = c.getAndIncrement
      if(j % 10000 == 0) println(j)
      val points = (1 to 15).map(_ => HIPS.generatePoint(100, 100, 10.0, rnd))
      HIPS.hipsPnt.exec(points, true)
    }
  }
}
