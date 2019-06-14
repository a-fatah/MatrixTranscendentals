import Transcendentals.sqMatrix
import org.scalatest._

class TranscendentalsTestSuite extends FunSuite {
  test("constructor and equals") {
    assert(sqMatrix(Array(Array(1.0, 0.0), Array(0.0, 1.0))) == sqMatrix.identity(2))
    assert(sqMatrix(Array(Array(1.0,2.0),
      Array(4.0,7.0))) == sqMatrix(Array(Array(1.0,2.0),
      Array(4.0,7.0))))
  }
  test("plus") {
    val m1 = sqMatrix(Array(Array(1.0, 1.0), Array(0.0, 1.0)))
    val m2 = sqMatrix(Array(Array(1.0, 1.0), Array(1.0, 0.0)))
    val zero = sqMatrix(2, (row, col) => 0.0)
    assert(m1 + m2 == m2 + m1)
    assert(m1 + zero == m1)
    assert(m1 == m1 + zero)
    assert(m1 + zero == zero + m1)
    assert(m1 == m1)
    assert(m1 + m2 == sqMatrix(Array(Array(2.0, 2.0), Array(1.0, 1.0))))
  }
  test("times") {
    val m1 = sqMatrix(Array(Array(1.0, 1.0), Array(0.0, 1.0)))
    val m2 = sqMatrix(Array(Array(1.0, 1.0), Array(1.0, 0.0)))
    val zero = sqMatrix(2, (row, col) => 0.0)
    val e = sqMatrix.identity(2)

    assert(m1 * m2 != m2 * m1)
    assert(m1 * zero == zero)
    assert(m2 * zero == zero)
    assert(zero * m1 == zero)
    assert(m1 * e == m1)
    assert(e * m1 == m1)
    assert(e * e == e)
    assert(zero * zero == zero)
    assert(m1 * m2 == sqMatrix(Array(Array(2.0, 1.0), Array(1.0, 0.0))))
  }
  test("find pivot row") {
    val m3 = sqMatrix(Array(Array(1.0, 1.0, -2.0),
      Array(0.0, 1.0, 0.0),
      Array(0.0, 12.0, 1.0)))
    val m2 = sqMatrix(Array(Array(-2.0, 1.0),
      Array(1.0, 0.0)))
    assert(0 == m3.findPivotRow(0))
    assert(2 == m3.findPivotRow(1))
    assert(0 == m2.findPivotRow(0))
    assert(0 == m2.findPivotRow(1))
  }
  test("scale row") {
    val m3 = sqMatrix(Array(Array(1.0, 1.0, -2.0),
      Array(0.0, 1.0, 0.0),
      Array(0.0, 12.0, 1.0)))
    assert(m3.scaleRow(-1.0, 0) == sqMatrix(Array(Array(-1.0, -1.0, 2.0),
      Array(0.0, 1.0, 0.0),
      Array(0.0, 12.0, 1.0))))
    assert(m3.scaleRow(0.0, 1) == sqMatrix(Array(Array(1.0, 1.0, -2.0),
      Array(0.0, 0.0, 0.0),
      Array(0.0, 12.0, 1.0))))
  }
  test("swap rows") {
    val m3 = sqMatrix(Array(Array(1.0, 1.0, -2.0),
      Array(0.0, 1.0, 0.0),
      Array(0.0, 12.0, 1.0)))
    assert(m3.swapRows(0, 1) == sqMatrix(Array(Array(0.0, 1.0, 0.0),
      Array(1.0, 1.0, -2.0),
      Array(0.0, 12.0, 1.0))))
    (0 until 3).foreach { j => {
      (0 until 3).foreach { i => {
        val m = m3.swapRows(i, j)
        if (i == j)
          assert(m == m3)
        else
          assert(m != m3)
        assert(m3 == m.swapRows(i, j))
      }
      }
    }
    }
  }
  test("row operation") {
    val m3 = sqMatrix(Array(Array(1.0, 1.0, -2.0),
      Array(0.0, 1.0, 0.0),
      Array(0.0, 12.0, 1.0)))
    assert(m3.rowOperation(1.0, 0, -1.0, 1) ==
      sqMatrix(Array(Array(1.0, 2.0, -2.0),
        Array(0.0, 1.0, 0.0),
        Array(0.0, 12.0, 1.0))))
    assert(m3.rowOperation(4.0, 1, 1.0, 2) ==
      sqMatrix(Array(Array(1.0, 1.0, -2.0),
        Array(0.0,-2.0,-0.25),
        Array(0.0, 12.0, 1.0))))
  }
  test("norm") {
    val zero = sqMatrix.zero(3)
    assert(zero.norm == 0.0)
    assert(zero.norm == 0.0)
    val id = sqMatrix.identity(4)
    assert(id.norm == 1.0)
  }
  test("dist") {
    val m2 = sqMatrix(Array(Array(1.0, 2.0),
      Array(-1.0, 3.0)))
    val id = sqMatrix.identity(2)
    val zero = sqMatrix.zero(2)
    assert(id.dist(id) == 0.0)
    assert(zero.dist(zero) == 0)
    assert(id.dist(m2) == m2.dist(id))
    assert(id.dist(zero) == zero.dist(id))
    //assert(math.abs(id.dist(zero) - math.sqrt(2.0)) < 0.00001)
    //assert(math.abs(id.dist(m2) - 3.0) < 0.0001)

    for{ // triangle inequality
      a <- (1 to 12 by 3)
      b <- (-2 to 2)
      c <- (-10 to 10 by 5)
      d <- (-7 to -3)
      u = sqMatrix(Array(Array(a,b),
        Array(c,d)))
      v = sqMatrix(Array(Array(a*b,c*d),
        Array(-a*d+b,a+b-c*d)))
    } yield assert(u.dist(u.identity) + u.identity.dist(v) >= u.dist(v))
  }
  test("inverse") {
    // inverse of identity is identity
    assert(sqMatrix.identity(2) == sqMatrix.identity(2).inverse)
    val u = sqMatrix(Array(Array(0.0, 1.0),
      Array(-1.0, 0.0)))
    val v = u * -1
    assert(u.inverse == v)
    assert(v.inverse == u)
    val m2 = sqMatrix(Array(Array(1.0, 2.0),
      Array(-1.0, 3.0)))
    assert(m2.inverse.dist(sqMatrix(Array(Array(3.0 / 5, -2.0 / 5),
      Array(1.0 / 5, 1.0 / 5)))) < 0.0001)
    assert((m2.inverse * m2).dist(sqMatrix.identity(2)) < 0.0001)
  }
  test("upperTriangular"){
    val ut = sqMatrix(Array(Array(1.0,0.0),
      Array(0.0,2.0)))
    assert(ut.upperTriangularize() ==
      ((ut, ut.identity), 1))
  }
  test("diagonalize"){
    val m2 = sqMatrix(Array(Array(1.0,0.0),
      Array(0.0,2.0)))
    assert(m2.diagonalize(m2,m2) == (m2,m2))

    val ut = sqMatrix(Array(Array(1.0,0.0),Array(0.0,2.0)))
    assert(ut.diagonalize(ut,ut.identity) == (ut,ut.identity))
  }
  test("determinant") {
    assert(1.0 == sqMatrix.identity(2).determinant)
    assert(1.0 == sqMatrix.identity(3).determinant)
    assert(1.0 == sqMatrix.identity(4).determinant)
    assert(-1.0 == sqMatrix(Array(Array(0.0, 1.0),
      Array(1.0, 0.0))).determinant)

    assert(2.0 == sqMatrix(Array(Array(1.0, 0.0),
      Array(0.0, 2.0))).determinant )

    assert(4 == sqMatrix(Array(Array(2.0, 0.0),
      Array(0.0, 2.0))).determinant )
    val m2 = sqMatrix(Array(Array(1.0, 2.0),
      Array(-1.0, 3.0)))
    assert(m2.determinant == 5.0)

    val m2i = sqMatrix(Array(Array(3.0/5, -2.0/5),
      Array(1.0/5, 1.0/5)))
    assert(math.abs(m2i.determinant - 0.2) < 0.0001)

    assert(m2.determinant == 1 / m2.inverse.determinant)
  }

  test("pow") {
    (1 to 4).foreach { dim => {
      val zero = sqMatrix.zero(dim)
      val id = sqMatrix.identity(dim)
      assert(zero == zero.pow(1))
      assert(zero == zero.pow(2))
      assert(zero == zero.pow(3))
      assert(id == id.pow(1))
      assert(id == id.pow(2))
      assert(id == id.pow(3))
    }
    }

    val m3 = sqMatrix(Array(Array(1.0, 1.0, -2.0),
      Array(0.0, 1.0, 0.0),
      Array(0.0, 12.0, 1.0)))
    (1 to 10).foreach { exp => {
      val d = m3.pow(exp).dist((1 to exp).foldLeft(sqMatrix.identity(3)) {
        case (m: sqMatrix, _: Int) => m * m3
      })
      assert(d < 0.00001)
    }
    }

    assert(m3.inverse.dist(m3.pow(-1)) < .0001)

    assert(m3.pow(-5).dist(m3.pow(5).inverse ) < .00001 )

  }
  test("cos"){
    val c:sqMatrix = sqMatrix.zero(3).cos()
    assert(sqMatrix.identity(3).dist(c) < .0001)
  }
  test("sin"){
    val s = sqMatrix.zero(3).sin()

    assert(sqMatrix.zero(3).dist(s) < .0001)
  }
  test("pythagoras"){
    val m3 = sqMatrix(Array(Array(1.0, 1.0, -2.0),
      Array(1.0, -2.0, 1.0),
      Array(2.0, -1.0, 1.0)))

    // sin^2 + cos^2 == 1
    val v3 = m3.cos().pow(2)+m3.sin().pow(2)
    assert( v3.identity.dist(v3) < 0.0001)
    assert( v3.dist(v3.identity) < 0.0001)

    val m4 = sqMatrix(Array(Array(1.0, 1.0, -2.0, 2.1),
      Array(-0.5, 1.0, 0.0, -1.0),
      Array(0.0, -2.0, 1.0, 0.0),
      Array(1.0, 2.0, -2.0, 3.0)))
    val v4 = m4.cos().pow(2)+m4.sin().pow(2)
    assert( v4.identity.dist(v4) < 0.0001)
    assert( v4.dist(v4.identity) < 0.0001)
  }
  test("double angle"){
    // sin(2x) = 2sin(x)cos(x)
    val m2 = sqMatrix(Array(Array(1.0,0.8),
      Array(-0.5, 1.5)))
    val m3 = sqMatrix(Array(Array(1.0, 1.0, -2.0),
      Array(1.0, -2.0, 1.0),
      Array(2.0, -1.0, 1.0)))
    val m4 = sqMatrix(Array(Array(1.0, 1.0, -2.0, 2.1),
      Array(-0.5, 1.0, 0.0, -1.0),
      Array(0.0, -2.0, 1.0, 0.0),
      Array(1.0, 2.0, -2.0, 3.0)))
    for{
      m <- List(m2, m3, m4)
    } yield assert( (m*2).sin().dist(m.sin()*m.cos()*2) < 0.001 )

    // cos(2x) = 1 - 2(sin(x)^2)
    for{
      m <- List(m2, m3, m4)
    } yield assert( (m*2).cos.dist( m.identity - m.sin.pow(2)*2) < 0.001)
  }
  test("cos and sin diagonal") {
    import math.Pi
    val m2 = sqMatrix(Array(Array(Pi / 2, 0.0, 0.0),
      Array(0.0, Pi / 2, 0.0),
      Array(0.0, 0.0, Pi / 2)))
    println("cos diag= " + m2.cos())
    assert(m2.cos().dist(m2.zero) < 0.001)

    println("sin diag= "+ (m2*2.0).sin())
  }
  test("sin and cos of sum"){
    // sin(x+y) = sin(x)cos(y)+cos(x)sin(y)
    val m2 = sqMatrix(Array(Array(1.0,0.8),
      Array(-0.5, 1.5)))
    val m3 = sqMatrix(Array(Array(1.0, 1.0, -2.0),
      Array(1.0, -2.0, 1.0),
      Array(2.0, -1.0, 1.0)))
    val m4 = sqMatrix(Array(Array(1.0, 1.0, -2.0, 2.1),
      Array(-0.5, 1.0, 0.0, -1.0),
      Array(0.0, -2.0, 1.0, 0.0),
      Array(1.0, 2.0, -2.0, 3.0)))
    for {
      a <- List(m2, m3, m4)
      b = a.identity - a
      left = (a+b).sin()
      right = (a.sin())*b.cos() + (a.cos())*b.sin()
      dist = left.dist(right)
    }
      yield assert(dist < 0.001)

    /// cos(x+y) = cos(x)cos(y) - sin(x)sin(y)
    for {
      a <- List(m2, m3, m4)
      b = a.identity - a
      left = (a+b).cos()
      right = (a.cos())*b.cos() - (a.sin())*b.sin()
      dist = left.dist(right)
    }
      yield assert(dist < 0.001)
  }
  test("exp"){
    val m3 = sqMatrix(Array(Array(1.0,  1.0, -2.0),
      Array(1.0, -2.0,  1.0),
      Array(2.0, -1.0,  1.0)))
    val e2 = (m3.identity * 2).exp()
    // matrix with e^2 on the diagonal
    assert(math.abs(math.exp(2) - e2(0,0)) < 0.001)
    assert(math.abs(math.exp(2) - e2(1,1)) < 0.001)
    assert(math.abs(math.exp(2) - e2(2,2)) < 0.001)

    //println("double e="+m3.exp(1.0,11))
    //println("double e 2 ="+m3.exp(2.0,11))
    // e^0 = 1
    assert(m3.zero.exp().dist(m3.identity) < 0.00001)

    // e^x * e^-x = e^0 = 1
    assert((m3.exp()*(m3 * -1.0).exp()).dist(m3.identity) < 0.001)

    // exp(x)*exp(1-x) = e = exp(1)
    val e1 = m3.identity.exp()
    val lh = (m3.exp())*((m3.identity - m3).exp())
    assert(lh.dist(e1) < .001)
  }
}
