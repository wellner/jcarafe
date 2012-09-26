package org.mitre.jcarafe.maxent

import org.mitre.jcarafe.util.Options
import org.mitre.jcarafe.crf.AbstractInstance

class SparseToDenseMapper(opts: Options) {
  def mapIt() = {
    val sGen = new MaxEntTrainingSeqGen(opts)
    val input: Seq[AbstractInstance] = sGen.createSeqsFromFiles(0).iseq
    input foreach {ai =>
      val fvs = ai.getCompactVec
      var set = Set[Int]()
    fvs foreach {f => set += f.fid}
    print(ai.label)		
    for (i <- 0 until (sGen.getNumberOfFeatures / sGen.getNumberOfStates)) {
      if (set.contains(i)) {
	print(" 1")
      } else print(" 0")
    }
    print("\n")		
  }
  }
}

object ConvertSparseToDense {
  def main (argv: Array[String]) : Unit = {
    val opts = new Options(argv)
    val mapper = new SparseToDenseMapper(opts)
    mapper.mapIt()
  }

}
