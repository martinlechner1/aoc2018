package aoc

import atto._, Atto._
import cats.implicits._

import scala.io.Source

object DayEight {

  case class Header(children: Int, metadata: Int)
  case class Body(childNodes: List[Int], metadataEntries: List[Int])

  private def runParser[A](parser: Parser[A], input: String): A =
    parser.parse(input).done.option.get

  private def parseHeader(): Parser[Header] =
    (token(int), token(int)).mapN(Header.apply)

  private def parseBody(header: Header, rootParser: Parser[Int]): Parser[Body] =
    (manyN(header.children, rootParser), manyN(header.metadata, token(int)))
      .mapN(Body.apply)

  private def parse(resultTransformer: Body => Int): Parser[Int] =
    for {
      header <- parseHeader()
      body <- parseBody(header, parse(resultTransformer))
    } yield resultTransformer(body)

  private def resultTransformerA(body: Body): Int =
    body.childNodes.sum + body.metadataEntries.sum

  private def resultTransformerB(body: Body): Int =
    if (body.childNodes.isEmpty) {
      body.metadataEntries.sum
    } else {
      body.metadataEntries
        .flatMap(
          idx =>
            body.childNodes.zipWithIndex
              .find(_._2 == idx - 1)
              .map(_._1))
        .sum
    }

  def run() = {
    val input = Source.fromResource("input08").mkString.trim()
    println(runParser(parse(resultTransformerA), input))
    println(runParser(parse(resultTransformerB), input))
  }

}
