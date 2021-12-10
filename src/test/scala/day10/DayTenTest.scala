package day10

import day10.DayTen.{getAutocompleteScore, getErrorScore, getMiddleValue, parseLine}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class DayTenTest extends AnyFunSpec {

  describe("DayTenTest") {

    val input = Seq("[({(<(())[]>[[{[]{<()<>>",
      "[(()[<>])]({[<{<<[]>>(",
      "{([(<{}[<>[]}>{[]{[(<()>",
      "(((({<>}<{<{<>}{[]{[]{}",
      "[[<[([]))<([[{}[[()]]]",
      "[{[{({}]{}}([{[{{{}}([]",
      "{<[[]]>}<{[{[{[]{()[[[]",
      "[<(<(<(<{}))><([]([]()",
      "<{([([[(<>()){}]>(<<{{",
      "<{([{{}}[<[[[<>{}]]]>[]]")

    it("should find first invalid chars") {
      input.flatMap(line => parseLine(line, Seq()).toOption) shouldBe Seq('}', ')', ']', ')', '>')
    }

    it("should calculate error scores") {
      input.flatMap(line => getErrorScore(line)).sum shouldBe 26397
    }

    it("should calculate autocomplete scores") {
      input.flatMap(line => getAutocompleteScore(line)) shouldBe Seq(288957, 5566, 1480781, 995444, 294)
    }

    it("should calculate final autocomplete score") {
      getMiddleValue(input.flatMap(getAutocompleteScore)) shouldBe Some(288957)
    }
  }
}
