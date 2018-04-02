import java.io.{File, PrintWriter}

import scala.io.Source


import java.nio.file.{Paths, Files}
import java.nio.file.{Paths, Files}

object HelloWordApp extends App {
  var RESOURCES = "./src/main/resources/"

  println("Hello world!")


//  fetch_file("test.txt")
//  fetch_file("train.txt")

  def fetch_file(file_name: String): String = {

    var tufts_formatted_file_name: String = write_tufts_in_file_name(file_name)

    if (file_exists(tufts_formatted_file_name)) { // No need for formatting, file is ready from previous runs
      return tufts_formatted_file_name
    } else {
      return convert_penn_to_tufts_format(file_name) // convert and return the file
    }
  }

  private def write_tufts_in_file_name(file_name: String) = {
    var file_tokens = file_name.split("\\.")

    var tufts_formatted_file_name = file_tokens(0) + "_tufts." + file_tokens(1)
    tufts_formatted_file_name
  }

  def file_exists(file_name: String) : Boolean ={
    return Files.exists(Paths.get(RESOURCES + file_name))
  }

  def convert_penn_to_tufts_tag(penn_tag: String) : String = {
    if (penn_tag.startsWith("N")) return "NOUN"
    else if (penn_tag.startsWith("V")) return "VERB"
    else return "OTHER"
  }

  def convert_penn_to_tufts_format(file_name: String): String ={
    val original_file = new File(getClass.getClassLoader.getResource(file_name).getPath)
    val original_lines = Source.fromFile(original_file).getLines()

    val tufts_formatted_file_path_name = RESOURCES + write_tufts_in_file_name(file_name)

    print(tufts_formatted_file_path_name)


    val writer = new PrintWriter(new File(tufts_formatted_file_path_name))

    for (line <- original_lines) {
      var converted_line = ""
      if (!line.equals("")) {
        val term_and_tag = line.split(" ")

        println(term_and_tag.length)
        println(term_and_tag)

        val term = term_and_tag(0)
        val tag = convert_penn_to_tufts_tag(term_and_tag(1))

        converted_line = term + " " + tag
      }

      writer.write(converted_line + "\n")

      println(line)

    }
    writer.close()

    return tufts_formatted_file_path_name
  }



}