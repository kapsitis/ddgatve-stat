object Main {

	def main(args: Array[String]): Unit = {

			val t1 = HuffmanCode.getSampleTree();
			val enc1 = HuffmanCode.encode(t1)(StringUtils.string2Chars("abd"));
			println(enc1);
			println(HuffmanCode.quickEncode(t1)(StringUtils.string2Chars("abd")))
			println(HuffmanCode.decodedSecret);
	}
}
