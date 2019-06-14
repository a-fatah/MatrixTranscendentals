object Transcendentals {

  // Because sqMatrix is a case class, we can construct a new sqMatrix instance,
  //   by specifying a dimension, dim, and a function mapping (Int,Int) to Double.
  case class sqMatrix(dim: Int, tabulate: (Int, Int) => Double) {
    val arr: Array[Double] = Array.tabulate(dim * dim)((i: Int) => tabulate(i / dim, i % dim))

    override def toString: String = {
      (0 until dim).map { row => {
        val x = (0 until dim).map { col => this (row, col).toString }
        x.mkString("[", ",", "]")
      }
      }.mkString("[", ",", "]")
    }

    // the Double in (row,col) of an sqMatrix can be accessed by
    //    m(row,col), assuming 0 <= row < dim, and 0 <= col < dim, this condition is unchecked.
    def apply(row: Int, col: Int): Double = arr(row * dim + col)

    override def equals(that: Any): Boolean = {
      // two sqMatrix instances are considered == if the underlying arrays have the same elements
      // in the corresponding locations.
      that match {
        case that: sqMatrix => (this.dim == that.dim) && (this.arr sameElements that.arr)
        case _ => false
      }
    }

    def +(that: sqMatrix): sqMatrix = {
      assert(dim == that.dim)
      sqMatrix(dim, (row, col) => this (row, col) + that(row, col))
    }

    def -(that: sqMatrix): sqMatrix = {
      assert(dim == that.dim)
      sqMatrix(dim, (row, col) => this (row, col) - that(row, col))
    }

    def dist(that: sqMatrix): Double = {
      (this - that).norm
    }

    lazy val norm = {
      // the L-infinity norm
      arr.fold(0.0)((max,x)=> math.max(math.abs(x),max))
    }

    def *(that: sqMatrix): sqMatrix = {
      assert(dim == that.dim)
      sqMatrix(dim, (row, col) => {
        (0 until dim).foldLeft(0)((sum: Double, k: Int) => sum + ??? * ???)
      })
    }

    def *(that: Integer): sqMatrix = {
      sqMatrix(dim, (row, col) => that * this(row, col))
    }

    def *(that: Double): sqMatrix = {
      sqMatrix(dim, (row, col) => that * this(row, col))
    }

    def /(that: sqMatrix): sqMatrix = {
      this * that.inverse
    }

    // are all the entries below the main diagonal equal to 0.0?
    lazy val isUpperTriangular: Boolean = {
      (1 until dim).forall(row => {
        (0 until row).forall(col => {
          0.0 == this (row, col)
        })
      })
    }
    // matrix with row and column swapped.
    lazy val transpose = sqMatrix(dim, (col, row) => this (row, col))

    // are all the entries above the main diagonal equal to 0.0?
    lazy val isLowerTriangular: Boolean = transpose.isUpperTriangular

    lazy val (inverse: sqMatrix, determinant: Double) = {
      // The method we use for finding the inverse of `this`
      // is to perform row operations to reduce transform it to the identity,
      // and perform the same operations on the identity which transforms it
      // to the inverse.   The determinant is computed along the way,
      // start with 1.0; whenever rows swapped (because of pivoting), multiply
      // by -1; whenever a row is scaled by a factor alpha, multiply by 1/alpha.
      // Hereby, the 1.0 is transformed into the determinant.
      val ((m1, m2), sign) = upperTriangularize()
      val (m1diag, m2residue) = diagonalize(m1, m2)

      // now m1diag is both upper and lower triangular, thus diagonal
      // assert(m1diag.isLowerTriangular)
      // assert(m1diag.isUpperTriangular)
      // divide every row of m2residue by the diagonal element of m1diag for that row
      (0 until dim).foldLeft(m2residue, sign.toDouble)((md, j) => {
        val (m: sqMatrix, d: Double) = md
        (m.scaleRow(1.0 / m1diag(j, j), j), d * m1diag(j, j))
      })
    }

    // The upperTriangularize function starts with the original matrix `this`
    // and the identity matrix.
    // We perform row operations on this to transform it to the identity,
    // and we perform the same operations on the identity, thus transforming it to
    // the inverse of this.
    def upperTriangularize(): ((sqMatrix, sqMatrix), Int) = {
      var countSwaps = 1
      (
        (0 until dim - 1).foldLeft((this, sqMatrix.identity(dim))) {
          case ((m1, m2), col) => {
            val pivot = m1.findPivotRow(col)
            if (pivot != col) countSwaps *= -1
            (col + 1 until dim).foldLeft(swapRows((m1, m2), col, pivot)) {
              // put 0 into m1(row,col)
              case ((m1, m2), _) if m1(col, col) == 0.0 => (m1, m2)
              case ((m1, m2), row) if m1(row, col) == 0.0 => (m1, m2)
              case ((m1, m2), row) if m1(row, col) != 0.0 => rowOperation((m1, m2), m1(row, col), row, m1(col, col), col)
            }
          }
        },
        countSwaps)
    }

    // starting with two matrices, which were returned from upperTriangularize,
    // the first of which is now an upper triangular matrix, and the second is
    // the result of performing the same operations on the identity matrix.
    // Now transform the former to lower triangular retaining upper triangularity
    // thus transforming it to diagonal
    def diagonalize(m1: sqMatrix, m2: sqMatrix): (sqMatrix, sqMatrix) = {
      (dim - 1 until 0 by -1).foldLeft((m1, m2)) {
        (m1m2, col: Int) =>
          (col - 1 to 0 by -1).foldLeft(m1m2) {
            case ((m1, m2), _) if m1(col, col) == 0.0 => m1m2
            case ((m1, m2), row) if m1(row, col) == 0.0 => m1m2
            case ((m1, m2), row) => rowOperation((m1, m2), m1(col, col), row, m1(row, col), col)
          }
      }
    }

    // which row has the maximum element (in terms of absolute value) in the col'th column?
    def findPivotRow(col: Int): Int = {
      import math.abs
      (0 until dim).foldLeft(0)((maxRow, row) => {
        if (abs(this (row, col)) > abs(this (maxRow, col)))
          row
        else
          maxRow
      })
    }

    // produce a new sqMatrix which has row scaled by alpha.
    def scaleRow(alpha: Double, row: Int): sqMatrix = {
      sqMatrix(dim, (j, col) => {
        if (j == row)
          this (row, col) * alpha
        else
          this (j, col)
      })
    }

    // produce new sqMatrix which swaps row1 with row2.
    def swapRows(row1: Int, row2: Int): sqMatrix = {
      if (row1 == row2)
        this
      else
        sqMatrix(dim, (row, col) => {
          val r = if (row == row1)
            row2
          else if (row == row2)
            row1
          else
            row
          this (r, col)
        })
    }

    // given a tuple of two sqMatrix objects, non-destructively swap row1 with row2 in
    // both and return the new tuple.
    def swapRows(pair: (sqMatrix, sqMatrix), row1: Int, row2: Int): (sqMatrix, sqMatrix) = {
      val (m1, m2) = pair
      (m1.swapRows(row1, row2), m2.swapRows(row1, row2))
    }

    // We would like to calculate alpha*row1 - beta*row2 -> row2
    //   but this changes the determinant.  So instead we calculate the following:
    // row1 - (beta/alpha) * row2 -> row1
    //   which leaves the determinant unchanged.
    def rowOperation(alpha: Double, row1: Int, beta: Double, row2: Int): sqMatrix = {
      val ratio = beta/alpha
      sqMatrix(dim, (row, col) => {
        if (row == row1)
          this (row1, col) - ratio * this (row2, col)
        else
          this (row, col)
      })
    }

    // Perform the same row operations on two given sqMatrix instances.
    def rowOperation(pair: (sqMatrix, sqMatrix), alpha: Double, row1: Int, beta: Double, row2: Int): (sqMatrix, sqMatrix) = {
      val (m1, m2) = pair
      (m1.rowOperation(alpha, row1, beta, row2), m2.rowOperation(alpha, row1, beta, row2))
    }

    // compute the n'th power of an sqMatrix,
    // using a divide and conquer strategy.
    //  If n is zero, then m^0 = the identity
    //  If n is one, then m^1 = m
    //  If n is negative, then m^n = the inverse raised to the power -n
    //  If n is even then m^n = (m^(n/2))^2 = (m^(n/2))*(m^(n/2))
    //  If n is odd, then calculate m to the power one less than n, then multiply the result by m.
    def pow(exponent: Int): sqMatrix = {
      exponent match {
        case 0 => identity
        case 1 => this
        case i if i < 0 => pow(-i).inverse
        case i if i % 2 == 1 => { // odd
          ??? * pow(???)
        }
        case i => { //even
          val m = pow(???)
          m * m
        }
      }
    }

    lazy val zero: sqMatrix = sqMatrix.zero(dim)
    lazy val identity: sqMatrix = sqMatrix.identity(dim)
    lazy val thisSqr: sqMatrix = this * this

    def sumTuple(pair: (sqMatrix, sqMatrix)): sqMatrix = {
      val (m1, m2) = pair
      m1 + m2
    }

    // nTerms is the number of terms used in the Taylor expansions
    //   for exp, sin, and cos
    val nTerms = 12

    //           1    M^1   M^2   M^3
    // exp(M) = --- + --- + --- + --- + ...
    //           0!    1!    2!    3!
    def exp(): sqMatrix = sumTuple((???).foldLeft((???,???)) {
      case ((sum, term), n) => (??? + ???, ???)
    })

    //           1    M^2   M^4   M^6
    // cos(M) = --- - --- + --- - --- + ...
    //           0!    2!    4!    6!
    def cos(): sqMatrix = sumTuple((???).foldLeft((???,???)) {
      case ((sum, term), n) => ???
    })

    //          M^1   M^3   M^5   M^7
    // sin(M) = --- - --- + --- - --- + ...
    //           1!    3!    5!    7!
    def sin(): sqMatrix = sumTuple((???).foldLeft((???, ???)) {
      case ((sum, term), n) => ???
    })

    //            sin(M)
    // tan(M) == --------
    //            cos(M)
    def tan(): sqMatrix = sin() * cos().inverse

  }

  object sqMatrix {
    // we can construct an sqMatrix, by providing an Array of Arrays of Double
    def apply(entries: Array[Array[Double]]): sqMatrix = {
      // don't allow arrays of different size in the same sqMatrix,
      //  and force the length of the outer array = length of inner arrays
      //  I.e., the matrix is square.
      assert(entries.forall{a => entries.length == a.length})
      sqMatrix(entries.length, (row, col) => entries(row)(col))
    }

    // sqMatrix with 1.0 on the main diagonal, and 0.0 elsewhere
    def identity(dim: Int): sqMatrix = {
      sqMatrix(dim, (row, col) => if (row == col) 1.0 else 0.0)
    }

    // sqMatrix having all 0.0 as entries.
    def zero(dim: Int): sqMatrix = {
      sqMatrix(dim, (row, col) => 0.0)
    }
  }

  def main(argv: Array[String]): Unit = {

  }
}
