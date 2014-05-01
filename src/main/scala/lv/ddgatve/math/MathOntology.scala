package lv.ddgatve.math

object MathOntology {

  val countryMap = Map("lv" -> "Latvia")

  val yearMap = Map(
    "openmo38" -> 2011,
    "openmo39" -> 2012,
    "openmo40" -> 2013,
    "openmo41" -> 2014, 
    "sol61" -> 2011)

  val olympiadMap = Map(
    "openmo38" -> "LV Open Math Olympiad",
    "openmo39" -> "LV Open Math Olympiad",
    "openmo40" -> "LV Open Math Olympiad",
    "openmo41" -> "LV Open Math Olympiad",
    "sol61" -> "LV Prep Olympiad in Math")

  val languageMap = Map("lv" -> "Latvian", "en" -> "English")

  val schoolAgeMap = Map("lv1" -> 8, "lv2" -> 9, "lv7" -> 10, "lv8" -> 11,
    "lv5" -> 12, "lv6" -> 13, "lv7" -> 14, "lv8" -> 15,
    "lv9" -> 16, "lv10" -> 17, "lv11" -> 18, "lv12" -> 19)

  val indexMap = Map(
    "en" -> List(("2013", "", "", "1 of 40", "amo40-list-en.html")),
    "lv" -> List(
      ("2011", "1 of 40", "sol61-list.html", "5 of 40", "amo38-list.html"),
      ("2012", "", "", "9 of 40", "amo39-list.html"),
      ("2013", "", "", "23 of 40", "amo40-list.html"),
      ("2014", "", "", "2 of 40", "amo41-list.html")))

  val abbrevMap = Map(
    "en" -> Map("SOL" -> "PrepO", "NOL" -> "RegO", "VOL" -> "StateO", "AMO" -> "OpenMO"),
    "lv" -> Map("SOL" -> "SOL", "NOL" -> "NOL", "VOL" -> "VOL", "AMO" -> "AMO"))

}


