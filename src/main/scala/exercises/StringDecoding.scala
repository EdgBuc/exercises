package exercises

object StringDecoding extends App {

  var result = 0;
  var given = "CRnCaSiRnBSiRnFArTiBPTiTiBFArPBCaSiThSiRnTiBPBPMgArCaSiRnTiMgArCaSiThCaSiRnFArRnSiRnFArTiTiBFArCaCaSiRnSiThCaCaSiRnMgArFYSiRnFYCaFArSiThCaSiThPBPTiMgArCaPRnSiAlArPBCaCaSiRnFYSiThCaRnFArArCaCaSiRnPBSiRnFArMgYCaCaCaCaSiThCaCaSiAlArCaCaSiRnPBSiAlArBCaCaCaCaSiThCaPBSiThPBPBCaSiRnFYFArSiThCaSiRnFArBCaCaSiRnFYFArSiThCaPBSiThCaSiRnPMgArRnFArPTiBCaPRnFArCaCaCaCaSiRnCaCaSiRnFYFArFArBCaSiThFArThSiThSiRnTiRnPMgArFArCaSiThCaPBCaSiRnBFArCaCaPRnCaCaPMgArSiRnFYFArCaSiThRnPBPMgAr"

  val rules = Map(
    "ThF" -> "Al",
    "ThRnFAr" -> "Al",
    "BCa" -> "B",
    "TiB" -> "B",
    "TiRnFAr" -> "B",
    "CaCa" -> "Ca",
    "PB" -> "Ca",
    "PRnFAr" -> "Ca",
    "SiRnFYFAr" -> "Ca",
    "SiRnMgAr" -> "Ca",
    "SiTh" -> "Ca",
    "CaF" -> "F",
    "PMg" -> "F",
    "SiAl" -> "F",
    "CRnAlAr" -> "H",
    "CRnFYFYFAr" -> "H",
    "CRnFYMgAr" -> "H",
    "CRnMgYFAr" -> "H",
    "HCa" -> "H",
    "NRnFYFAr" -> "H",
    "NRnMgAr" -> "H",
    "NTh" -> "H",
    "OB" -> "H",
    "ORnFAr" -> "H",
    "BF" -> "Mg",
    "TiMg" -> "Mg",
    "CRnFAr" -> "N",
    "HSi" -> "N",
    "CRnFYFAr" -> "O",
    "CRnMgAr" -> "O",
    "HP" -> "O",
    "NRnFAr" -> "O",
    "OTi" -> "O",
    "CaP" -> "P",
    "PTi" -> "P",
    "SiRnFAr" -> "P",
    "CaSi" -> "Si",
    "ThCa" -> "Th",
    "BP" -> "Ti",
    "TiTi" -> "Ti",
    "HF" -> "e",
    "NAl" -> "e",
    "OMg" -> "e",
  )

  private val fromLongestKeys: List[String] =
    rules.keys.toList.sortBy(key => rules.count(kv => kv._2.equals(rules(key))))

  println(fromLongestKeys.head)
  decode(fromLongestKeys.head, fromLongestKeys.tail)
  println(result)

  def decode(key: String, otherKeys: List[String]): Unit = {
    val occurencies = countSubstring(given, key)
    if (occurencies > 0) {
      result += occurencies;
      given = given.replace(key, rules(key))
      decode(fromLongestKeys.head, fromLongestKeys.tail)
    }

    if (!given.equals("e") && otherKeys.nonEmpty) {
      decode(otherKeys.head, otherKeys.tail)
    }
  }

  println(given)


  def countSubstring(str: String, sub: String): Int =
    str.sliding(sub.length).count(_ == sub)

}
