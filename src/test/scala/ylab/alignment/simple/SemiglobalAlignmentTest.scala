package ylab.alignment.simple

import org.scalatest.{Matchers, FlatSpec}
import ylab.alignment.logic.simple.SemiglobalAlignment
import ylab.alignment.{Vertical, Horizontal}

/**
 * User: pavel
 * Date: 12.10.15
 * Time: 17:11
 */
class SemiglobalAlignmentTest extends FlatSpec with Matchers {
  private val dummySub = (c : Char, d : Char) => if (c == d) 1 else -1

  "Semiglobal alignment" should "align equal strings" in {
    val sa = new SemiglobalAlignment[Char](10, 10, dummySub, -1)
    val s = "Hello!"

    sa.push(s)(Horizontal)
    sa.push(s)(Vertical)

    val result = sa.traceback()
    result.score should be (s.length)
    result.horizontal should contain theSameElementsInOrderAs result.vertical
  }

  it should "align with mismatches" in {
    val sa = new SemiglobalAlignment[Char](10, 10, dummySub, -1)

    sa.push("ACGT")(Horizontal)
    sa.push("AGGT")(Vertical)

    val result = sa.traceback()
    result.score should be (2)
    result.horizontal.size should be (4)
    result.vertical.size should be (4)
  }

  it should "align with indels" in {
    val sa = new SemiglobalAlignment[Char](10, 10, dummySub, -1)

    sa.push("ACGT")(Horizontal)
    sa.push("AGT")(Vertical)

    val result = sa.traceback()
    result.score should be (2)
    result.horizontal.size should be (4)
    result.vertical.size should be (4)
    result.vertical(1) should be (None)
  }

  it should "align with lead/end indels" in {
    val sa = new SemiglobalAlignment[Char](10, 10, dummySub, -1)

    sa.push("CGTA")(Horizontal)
    sa.push("AAAAGT")(Vertical)

    val result = sa.traceback()
    result.score should be (1)
    result.horizontal.size should be (7)
    result.vertical.size should be (7)
  }

  it should "work very fast with long sequences" in {
    val ga = new SemiglobalAlignment[Char](1000, 1000, dummySub, -1)

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

