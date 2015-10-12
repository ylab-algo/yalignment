package ylab.alignment.simple

import org.scalatest.{FlatSpec, Matchers}
import ylab.alignment.logic.simple.GlobalAlignment
import ylab.alignment.{Horizontal, Vertical}

/**
 * User: pavel
 * Date: 11.10.15
 * Time: 11:44
 */
class GlobalAlignmentTest extends FlatSpec with Matchers {
  private val dummySub = (c : Char, d : Char) => if (c == d) 1 else -1

  "Global alignment" should "align equal strings" in {
    val ga = new GlobalAlignment[Char](10, 10, dummySub, -1)
    val s = "Hello!"

    ga.push(s)(Horizontal)
    ga.push(s)(Vertical)

    val result = ga.traceback()
    result.score should be (s.length)
    result.horizontal should contain theSameElementsInOrderAs result.vertical
  }

  it should "align with mismatches" in {
    val ga = new GlobalAlignment[Char](10, 10, dummySub, -1)

    ga.push("ACGT")(Horizontal)
    ga.push("AGGT")(Vertical)

    val result = ga.traceback()
    result.score should be (2)
    result.horizontal.size should be (4)
    result.vertical.size should be (4)
  }

  it should "align with indels" in {
    val ga = new GlobalAlignment[Char](10, 10, dummySub, -1)

    ga.push("ACGT")(Horizontal)
    ga.push("AGT")(Vertical)

    val result = ga.traceback()
    result.score should be (2)
    result.horizontal.size should be (4)
    result.vertical.size should be (4)
    result.vertical(1) should be (None)
  }

  it should "work fine with pop() and push()" in {
    val ga = new GlobalAlignment[Char](10, 10, dummySub, -1)

    ga.push("ACGT")(Horizontal)
    ga.push("AGT")(Vertical)

    val result1 = ga.traceback()

    ga.pop()(Horizontal)
    ga.push('A')(Horizontal)

    val result2 = ga.traceback()

    result2.score should be (0)
    result2.horizontal.size should be (result1.horizontal.size)
    result2.vertical.size should be (result1.vertical.size)
    result2.vertical(1) should be (result1.vertical(1))
  }

  it should "work very fast with long sequences" in {
    val ga = new GlobalAlignment[Char](1000, 1000, dummySub, -1)

    val a = "GACGGTTAGGTTCGCACAAGGCGTTTTTGCTATCGGCGTATCGGTCCAGCGAGACGGACGCTTGCTGCCGAGCCCTGACGAACTAAACACTAGAACGAGCAGACAGACAAACTTGTCATTTAGGGGCATAAGCATGCAGGGCCGTGTGCAAAATGCTCTTCTTCACCGGGCCGCTGCCGCCCGAGATCGACTGACTTGGGCCGGGTAAGCGGCGCTGTATGTTAAGTGGCGAGAGATCCCGGAGTTTACCCCTGAGCTCTTTTTGTACGGTTCCAAAAATTCCCTTCTCCGGGTTCGTCGTCACCGTCGCTATCCGTGGTTTTCGTACTAGTACGGGTATCCCCGCTTGACCGCATGGCTAAGAGTTCCGCGGTTTACGTATGTTTGAAATTTAATTGTTTATAAGGCGTAACCTAGAAAGGTTGAATTCCCTGCCCGTGTTGGTATACCGTTACCGAAAATCTATGTCGCCGCGAACGTAGTAAGTCATGCTCCTGAGTCCTTTTGGACAACTATAGTGTGGATTGTACTGTGATTCTTTAGACATTTACCATACACGGCCAGTTACCGTGAATACGAATACATCAAGTGCAGATGACGGAAATAAGCGAGAGTTTTCTTTATGCTTCGGCCAGCTGTAAATTTGTTTTCCTATATGATCCCCGAGATAGTCGGCTTGAACGAACTGGTGCCATGTGACACTCTGGCACCGATCGGTGTTGGGCGCAGCGAATTTTTTCCTTACAGATATAGCGAGCCACACGGTTGTTACGGAGTCATAGTCCCAGCCCCGGACCGTAAGTGTCTTAAAGCGTCCTGATTCGCACTTCGATTGTTGAAATGGGACGATCCGTCCCTTCAGCGCAACCGACATGCAACGGCCC"
    val b = "GACGGTCACGATTCACATCGCGTCTTATTGAGCGTTAGCGTATTCGTTCCAACGCGTTACGACGCCGTTAGCATAGCCGTAGCCCTGACACACTAAAAAGTTACAACAATCAGACGACTTAACCTATCATTTAGGCCAGAAAGTGTGAGGCCTGATTGTCAGAAAGCTTCTCTCACCGGGCCGCTCCGCCGGAGGTCACGTATGGGCGGGCTAACCGGCGCTTTTGTTAAAGCTTCAGCATCCCGAATTGTTACCCCGGTCGCTCTTTTTGTACGGGTTCCAAAACCTATGTTCGAGTTTGCTCGTCACGGGCCTATCGTGGTTTCTGTACGAGGTAAGTGCATTCCCGCGTTGACGGCTCGGGAAGAGTTCCAGCTGGATTTACGCCTGTCTGAATCTATAGTGTTTATGATACTAAGCACTAGCATAAGGTTGAATTTCCCCTTGCCCCGTGGTTGTGTACCGTTGCCGTAGAATTCAATGCGAATCGCGAACCTGTATAGGTTCATGCTCTGAATATCCCTGTTGGACGAATTATTTGTGTGGATTGTAATGTTGATCCTCGAGTATACATTTACCCATACAGCGCTCATTACTCGTTATGCAAATCATCAAGAAGCTGATGGGAACTGAAGGAGAGATCTCCTTGCTGTACGCCAGCTGTACAACTATTATTGCTTACATGAAACCCTTGATAGTCAGTTGACCCGAACTGGTGCCATGTATACCTTAGCCCGGCCGGGTTGGGGCGCACGCGAATTTTTCTTACCTGCGATCAGCAAACCCACATGTCCTCACGATCAGTAGGCCCAGCCCCGCGACCGTAGCGTCTATAGCGTCGTGATCCGCACTTTGTTGTCTAAGACCCTGGGCAGGTTCGTGCCCTTGCGCACTACATGCACGGCCC"

    var sum = 0
    var tm = System.currentTimeMillis()
    (0 until 50).foreach {
      case i =>
        ga.push(a)(Horizontal)
        ga.push(b)(Vertical)

        val result = ga.traceback()

        ga.pop(a.length)(Horizontal)
        ga.pop(b.length)(Vertical)

        sum += result.score
    }
    tm = System.currentTimeMillis() - tm
    println(s"Time spent: ${1.0 * tm / 1000}s")
  }
}
