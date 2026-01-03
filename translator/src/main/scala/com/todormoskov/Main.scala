package com.todormoskov

import com.todormoskov.Lexer.{Scanner, Tokenizer}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.annotation.tailrec
import scala.jdk.CollectionConverters.*
import scala.sys.exit

object Main {
  private val BEGIN_TRANSLATION = "\\* BEGIN TRANSLATION"
  private val END_TRANSLATION = "\\* END TRANSLATION"

  def main(args: Array[String]): Unit = {
    val parsedArgs = parseArgs(Map(), args.toList)
    val validatedArgs = validateArgs(parsedArgs)

    val inputLines = Files.readAllLines(validatedArgs.input, StandardCharsets.UTF_8).asScala
    if (!inputLines.exists(_.contains("--EPC"))) {
      println(s"EPC spec not found in file ${parsedArgs("input")}!")
      exit(1)
    }
    // Determine the start and end lines of the comment, containing the EPC spec.
    val epcStartLine = inputLines.indexWhere(_.contains("--EPC"))
    val epcEndLine = inputLines.zipWithIndex.indexWhere((line, idx) => line.endsWith("*)") && idx > epcStartLine)
    // And the start and end lines of its translation.
    val beginTranslationLine = inputLines.zipWithIndex.indexWhere((line, idx) => line.startsWith(BEGIN_TRANSLATION) && idx > epcEndLine)
    val endTranslationLine = inputLines.zipWithIndex.indexWhere((line, idx) => line.startsWith(END_TRANSLATION) && idx > beginTranslationLine)

    var specBeforeBeginTranslation = if beginTranslationLine != -1 && endTranslationLine != -1
      then inputLines.take(beginTranslationLine).mkString(System.lineSeparator())
      else inputLines.take(epcEndLine + 1).mkString(System.lineSeparator())
    var specAfterEndTranslation = if beginTranslationLine != -1 && endTranslationLine != -1
      then inputLines.drop(endTranslationLine + 1).mkString(System.lineSeparator())
      else inputLines.drop(epcEndLine + 1).mkString(System.lineSeparator())
    // Replace non-breaking spaces in the spec, because they are not handled by the parser.
    specBeforeBeginTranslation = specBeforeBeginTranslation.replace('\u00A0', ' ').replace('\u2007', ' ').replace('\u202F', ' ')
    specAfterEndTranslation = specAfterEndTranslation.replace('\u00A0', ' ').replace('\u2007', ' ').replace('\u202F', ' ')

    val scanner = Scanner(specBeforeBeginTranslation + System.lineSeparator() + specAfterEndTranslation)
    val tokenizer = Tokenizer(scanner)
    val parser = Parser(tokenizer)
    val translationResult = for {
      parsedModule <- parser.parseModule()
      epcCtx <- Analyzer.analyze(parsedModule)
    } yield EPCToTLA(epcCtx).translateSpec()

    if (translationResult.isLeft) {
      println(translationResult.left.toOption.get)
      exit(1)
    }
    val tlaTranslation = translationResult.toOption.get.getSpec
    val outputSpec = specBeforeBeginTranslation + System.lineSeparator()
      + BEGIN_TRANSLATION + System.lineSeparator()
      + tlaTranslation + System.lineSeparator()
      + END_TRANSLATION + System.lineSeparator()
      + specAfterEndTranslation + System.lineSeparator()

    Files.write(validatedArgs.input, outputSpec.getBytes(StandardCharsets.UTF_8))
  }

  private val usage = """
               Usage: epc2tla [--input <path-to-epc-spec.tla>]
               """

  private case class Arguments(input: Path)

  @tailrec
  private def parseArgs(accumulator: Map[String, String], list: List[String]): Map[String, String] = {
    list match {
      case Nil => accumulator
      case "--input" :: value :: tail =>
        parseArgs(accumulator ++ Map("input" -> value), tail)
      case "-i" :: value :: tail =>
        parseArgs(accumulator ++ Map("input" -> value), tail)
      case unknown :: _ =>
        println("Unknown option " + unknown)
        exit(1)
    }
  }

  private def validateArgs(args: Map[String, String]): Arguments = {
    // Input is a required argument!
    if (!args.contains("input")) {
      print(usage)
      exit(1)
    }

    val inputStr: String = args("input")
    // The input file should have a .tla extension.
    if (!inputStr.endsWith(".tla")) {
      println(s"File extension not allowed: $inputStr")
      exit(1)
    }

    // The file should exist
    val inputPath = Path.of(inputStr)
    if (!Files.exists(inputPath)) {
      println(s"File not found: $inputStr")
      exit(1)
    }
    Arguments(inputPath)
  }
}