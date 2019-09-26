object StringUtils {
	def string2Chars(str: String): List[Char] = { 
			str.toList
	}

	def getFrequencies(chars: List[Char]): List[(Char, Int)] = {
			def incr(acc:Map[Char, Int], c:Char) = {
					val count = (acc get c).getOrElse(0) + 1
							acc + ((c, count))
			}
			(Map[Char,Int]() /: chars)(incr).iterator.toList
	}
}