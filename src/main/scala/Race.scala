import scala.util.Random
trait Runner {
  val name: String
  val morale: String
  val ListMorale = List("Low", "Middle", "Hight")
  val ListPreference = List("Short", "Middle", "Long")
  def Run (distance: Int): Int
}
class Runner_1 extends Runner {
  val name = "Sergey"
  val morale = ListMorale(Random.nextInt(ListMorale.size))
  val preference = ListPreference(Random.nextInt(ListPreference.size))
  def Run (distance: Int): Int = distance match {
    case distance if (distance >= 60 && distance <= 400) => morale match {
      case "Low" => if (preference == "Short") Random.between(600, 651) + Random.between(16, 67) else Random.between(600, 651) - Random.between(16, 67)
      case "Middle" => if (preference == "Short") Random.between(651, 701) + Random.between(16, 67) else Random.between(651, 701) - Random.between(16, 67)
      case "Hight" => if (preference == "Short") Random.between(701, 751) + Random.between(16, 67) else Random.between(701, 751) - Random.between(16, 67)
    }
    case distance if (distance > 400 && distance <= 3000) => morale match {
      case "Low" => if (preference == "Middle") Random.between(300, 351) + Random.between(16, 67) else Random.between(300, 351) - Random.between(16, 67)
      case "Middle" => if (preference == "Middle") Random.between(351, 401) + Random.between(16, 67) else Random.between(351, 401) - Random.between(16, 67)
      case "Hight" => if (preference == "Middle") Random.between(401, 451) + Random.between(16, 67) else Random.between(401, 451) - Random.between(16, 67)
    }
    case distance if (distance > 3000 && distance <= 30000) => morale match {
      case "Low" => if (preference == "Long") Random.between(200, 251) + Random.between(16, 67) else Random.between(200, 251) - Random.between(16, 67)
      case "Middle" => if (preference == "Long") Random.between(251, 301) + Random.between(16, 67) else Random.between(251, 301) - Random.between(16, 67)
      case "Hight" => if (preference == "Long") Random.between(301, 351) + Random.between(16, 67) else Random.between(301, 351) - Random.between(16, 67)
    }
  }
}
class Runner_2 extends Runner_1 {
  override val name: String = "Vladislav"
}
class Runner_3 extends Runner_1 {
  override val name: String = "Yaroslav"
}
class Runner_4 extends Runner_1 {
  override val name: String = "Ivan"
}
object Main extends App {
  val distance = scala.io.StdIn.readLine("Введите дистанцию в метрах: ").toInt
  val sergey = new Runner_1
  val vladislav = new Runner_2
  val yaroslav = new Runner_3
  val ivan = new Runner_4
  def Race (distance: Int, speed: Int): Double = {
   distance.toDouble / (speed.toDouble / 60)
  }
  def PrintRunner (name: String, time: Int): Unit ={
    print(name)
    print(" -> ")
    print(Race(distance, time).toInt / 3600)
    print(":")
    print(Race(distance, time).toInt / 60)
    print(":")
    print((Race(distance, time).toInt % 60).toString.take(2))
    print(":")
    println(((Race(distance, time) % (Race(distance, time).toInt)) * 10).toString.take(1))
  }
  val speedRunner_1 = sergey.Run(distance)
  val speedRunner_2 = vladislav.Run(distance)
  val speedRunner_3 = yaroslav.Run(distance)
  val speedRunner_4 = ivan.Run(distance)
  PrintRunner(sergey.name, speedRunner_1)
  PrintRunner(vladislav.name, speedRunner_2)
  PrintRunner(yaroslav.name, speedRunner_3)
  PrintRunner(ivan.name, speedRunner_4)
 }