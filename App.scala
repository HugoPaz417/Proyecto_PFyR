package ec.edu.utpl.computacion.pfr.pi
package proyectoIntegrador

import cats.effect.IO
import doobie._
import doobie.implicits._
import com.github.tototoshi.csv._
import org.nspl._
import org.nspl.awtrenderer._
import org.nspl.data.HistogramData

import java.io.File

implicit object CustomFormat extends DefaultCSVFormat {
  override val delimiter: Char = ';'
}

object App {
  @main
  def pintegra(): Unit = {

    val pathDataFile = "C://Users//Asus Rog//Desktop//polaar//dsPartidosYGoles.csv"
    val pathDataFileInfoJugadores = "C://Users//Asus Rog//Desktop//polaar//3erCiclo//ProgramacionFuncional_y_Reactiva//Proyecto//dsAlineacionesXTorneo (1).csv"

    val reader = CSVReader.open(new File(pathDataFile))
    val readerInfo = CSVReader.open(new File(pathDataFileInfoJugadores))
    //val contentFile: List[List[String]] = reader.all()
    val contentFile: List[Map[String, String]] = reader.allWithHeaders()
    println(contentFile.take(2))
    println(s"Filas: ${contentFile.length} y columnas: ${contentFile(1).keys.size}")
    val contentFileInfo: List[Map[String, String]] = readerInfo.allWithHeaders()

    //Promedio edad arqueros
    val promEdadArqueros = contentFileInfo
      .filter(row =>
        row("players_goal_keeper").contains("1") &&
          row("players_birth_date").contains("-"))
      .map(row => 2024 - row("players_birth_date").split("-")(0).toInt)
    val calcEdadArqueros = promEdadArqueros.sum / promEdadArqueros.length.toDouble
    println(s"Promedio edad de arqueros: $calcEdadArqueros")

    //El Mediocampista mas viejo
    val medioCampistaViejo = contentFileInfo
      .filter(row =>
        row("players_midfielder").contains("1") &&
          row("players_birth_date").contains("-"))
      .map(row => 2024 - row("players_birth_date").split("-")(0).toInt).max
    println(s"MedioCampista viejo: $medioCampistaViejo")

    //Defensa mas joven
    val defensaMasJoven = contentFileInfo
      .filter(row =>
        row("players_defender").contains("1") &&
          row("players_birth_date").contains("-"))
      .map(row => 2024 - row("players_birth_date").split("-")(0).toInt).min
    println(s"Promedio edad defensas: $defensaMasJoven")

    //Delanteros
    val edadesDelanteros = contentFileInfo
      .filter(row => row("players_birth_date").contains("-"))
      .map(row => 2024 - row("players_birth_date").split("-")(0).toInt)

    val modaEdadDelanteros = edadesDelanteros.groupBy(identity).map { case (edad, lista) => (edad, lista.size) }.maxBy(_._2)._1
    println(s"La moda de las edades de los jugadores en general es de: $modaEdadDelanteros")

    //Histogramas
    def charting(data: List[Map[String, String]]): Unit = {
      val listNroShirt: List[Double] = data
        .filter(row => row("squads_position_name") == "forward")
        .map(row => row("").toDouble)

      val histForwardShirtNumer = xyplot(HistogramData(listNroShirt, 10) -> bar())(
        par
          .xlab("Shirt number")
          .ylab("freq.")
          .main("Forward shirt number")
      )
      pngToFile(new File("C://Users//Asus Rog//Desktop//polaar//3erCiclo//ProgramacionFuncional_y_Reactiva//Proyecto//image2proyecto.png"), histForwardShirtNumer.build, 1000)
      renderToByteArray(histForwardShirtNumer.build, 2000)
    }

    def charting2(data: List[Map[String, String]]): Unit = {
      val goalsPerMatch: List[Double] = data
        .filter(row => row("tournaments_year").toInt >= 2002)
        .map(row => {
          val homeScore = row("matches_home_team_score").toDouble
          val awayScore = row("matches_away_team_score").toDouble
          homeScore + awayScore
        })

      val histGoalsPerMatch = xyplot(HistogramData(goalsPerMatch, 10) -> bar())(
        par
          .xlab("Numero de goles")
          .ylab("Frecuencia")
          .main("Goles por partido, de 2002 en adelante")
      )

      pngToFile(new File("C://Users//Asus Rog//Desktop//polaar//3erCiclo//ProgramacionFuncional_y_Reactiva//Proyecto//imageproyecto.png"), histGoalsPerMatch.build, 1000)
      renderToByteArray(histGoalsPerMatch.build, 2000)
    }
    charting(contentFileInfo)
    charting2(contentFile)
    reader.close()
}}