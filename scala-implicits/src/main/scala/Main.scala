import java.nio.file._
import java.time.LocalDate
object PathImplicits {
	implicit class RichPath(apath: Path){
		def /(s: String): Path = apath.resolve(s)
		def /(bpath: Path): Path = apath.resolve(bpath)
		def write(s: String): Path = {
			val Filler = Files.createFile(apath)
			Files.write(apath, s.getBytes())
		}
		def read(): String = {
			new String(Files.readAllBytes(apath))
		}
		def append(s: String): Path = {
			if(Files.exists(apath)){
				Files.write(apath, (read() + s).getBytes())
			}else{
				write(s)
			}
		}
	}
	implicit class StringPath(root: String){
		def /(s: String) = Paths.get(root, s)
	}
}

object TimeImplicits {
	implicit class RichDate(aDate: LocalDate){
		def +(bDate: LocalDate): LocalDate = {
			val dayAdded = aDate.plusDays(bDate.getDayOfMonth())
			val monthAdded = dayAdded.plusMonths(bDate.getMonthValue() - 1)
			monthAdded.plusYears(bDate.getYear())
		}
	}
	implicit class RichDay(day: Int){
		def day(): LocalDate = {
			val zeroDate = LocalDate.of(-1, 12, 31)
			zeroDate.plusDays(day)
		}
		def month(): LocalDate = {
			val zeroDate = LocalDate.of(-1, 12, 31)
			zeroDate.plusMonths(day)
		}
		def year(): LocalDate = {
			val zeroDate = LocalDate.of(-1, 12, 31)
			zeroDate.plusYears(day)
		}
		def jan(): LocalDate = {
			LocalDate.of(LocalDate.now().getYear(), day, 1)
		}
		def jan(year: Int): LocalDate = {
			LocalDate.of(year, day, 1)
		}
		def feb(): LocalDate = {
			LocalDate.of(LocalDate.now().getYear(), day, 2)
		}
		def feb(year: Int): LocalDate = {
			LocalDate.of(year, day, 2)
		}
		def mar(): LocalDate = {
			LocalDate.of(LocalDate.now().getYear(), day, 3)
		}
		def mar(year: Int): LocalDate = {
			LocalDate.of(year, day, 3)
		}
		def apr(): LocalDate = {
			LocalDate.of(LocalDate.now().getYear(), day, 4)
		}
		def apr(year: Int): LocalDate = {
			LocalDate.of(year, day, 4)
		}
		def may(): LocalDate = {
			LocalDate.of(LocalDate.now().getYear(), day, 5)
		}
		def may(year: Int): LocalDate = {
			LocalDate.of(year, day, 5)
		}
		def jun(): LocalDate = {
			LocalDate.of(LocalDate.now().getYear(), day, 6)
		}
		def jun(year: Int): LocalDate = {
			LocalDate.of(year, day, 6)
		}
		def jul(): LocalDate = {
			LocalDate.of(LocalDate.now().getYear(), day, 7)
		}
		def jul(year: Int): LocalDate = {
			LocalDate.of(year, day, 7)
		}
		def aug(): LocalDate = {
			LocalDate.of(LocalDate.now().getYear(), day, 8)
		}
		def aug(year: Int): LocalDate = {
			LocalDate.of(year, day, 8)
		}
		def sep(): LocalDate = {
			LocalDate.of(LocalDate.now().getYear(), day, 9)
		}
		def sep(year: Int): LocalDate = {
			LocalDate.of(year, day, 9)
		}
		def oct(): LocalDate = {
			LocalDate.of(LocalDate.now().getYear(), day, 10)
		}
		def oct(year: Int): LocalDate = {
			LocalDate.of(year, day, 10)
		}
		def nov(): LocalDate = {
			LocalDate.of(LocalDate.now().getYear(), day, 11)
		}
		def nov(year: Int): LocalDate = {
			LocalDate.of(year, day, 11)
		}
		def dec(): LocalDate = {
			LocalDate.of(LocalDate.now().getYear(), day, 12)
		}
		def dec(year: Int): LocalDate = {
			LocalDate.of(year, day, 12)
		}
	}
}