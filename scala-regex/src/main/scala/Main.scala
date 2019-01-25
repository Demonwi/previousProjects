import scala.util.matching.Regex

object Regexes extends hw.regex.RegexLike {
	def notAlphanumeric : Regex = {
		"[^A-Za-z0123456789 ]*".r
	}
	def time : Regex = {
		"(0[0123456789]|1[0123456789]\\d|2[0123]):[012345]\\d".r
	}
	def phone : Regex = {
		"\\(\\d{3}\\)\\s\\d{3}-\\d{4}".r
	}
	def zip : Regex = {
		"\\d{5}|\\d{5}-\\d{4}".r
	}
	def comment : Regex = {
		"\\/\\*(.|\\n)*\\*\\/".r
	}
	def numberPhrase : Regex = {
		"\\btwenty\\b(-one|-two|-three|-four|-five|-six|-seven|-eight|-nine)|\\bthirty\\b(-one|-two|-three|-four|-five|-six|-seven|-eight|-nine)|\\bforty\\b(-one|-two|-three|-four|-five|-six|-seven|-eight|-nine)|\\bfifty\\b(-one|-two|-three|-four|-five|-six|-seven|-eight|-nine)|\\bsixty\\b(-one|-two|-three|-four|-five|-six|-seven|-eight|-nine)|\\bseventy\\b(-one|-two|-three|-four|-five|-six|-seven|-eight|-nine)|\\beighty\\b(-one|-two|-three|-four|-five|-six|-seven|-eight|-nine)|\\bninety\\b(-one|-two|-three|-four|-five|-six|-seven|-eight|-nine)".r
	}
	def roman : Regex = {
		"(XL|X{0,3})(IX|IV|V?I{0,3})".r
	}
	def date : Regex = {
		"((((19|20)(([02468][048])|([13579][26]))-((((0[1-9])|11|12)-((0[1-9])|(1[0-9])|(2[0-9])))|(((01|03|05|07|08|10|12)-31)|((01|03|04|05|06|07|08|09|10|11|12)-(29|30))))))|(\\d{4})-((((0[1-9])|11|12)-((0[1-9])|(1[0-9])|(2[0-8])))|(((01|03|05|07|08|10|12)-31)|((01|03|04|05|06|07|08|09|10|11|12)-(29|30)))))".r
	}
	def evenParity : Regex = {
		"(([02468]*[13579]){2})*[02468]*|(([02468]*[13579]){2})".r
	}

	def score(a:Int, b:Int): Int = {
		if(a == b){
			0
		}else{
			1
		}
	}
	def iter(a:Int, b:Int, c:Int, d:Int): Int = {
		if(a == 2){
			0
		}else{
			print(a)
			print(b)
			print(c)
			print(d)
			print(score(a, b))
			print(score(a, c))
			print(score(a, d))
			print(score(b, c))
			print(score(b, d))
			print(score(c, d))
			println()
			var nd = d + 1
			var nc = c
			var nb = b
			var na = a
			if(nd == 2){
				nd = 0
				nc += 1
			}
			if(nc == 2){
				nc = 0
				nb += 1
			}
			if(nb == 2){
				nb = 0
				na += 1
			}
		}
	}
}