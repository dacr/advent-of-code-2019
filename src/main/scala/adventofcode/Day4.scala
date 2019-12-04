package adventofcode

import better.files._

import scala.math._

object Day4 {

  object Part1 {

    def passwordFromStringToIntArray(password:String):Array[Int] = {
      password.toArray.map(ch => ch.toInt - 48)
    }

    def checkValididy(password:Array[Int]):Boolean = {
      def slides = password.sliding(2)
      val twoAdjacentDigitAreTheSame = slides.exists{case Array(a,b) => a==b}
      val digitsNeverDecrease = slides.forall{case Array(a,b) => b >= a}
      val passwordLengthConstraint = password.length==6
      twoAdjacentDigitAreTheSame && digitsNeverDecrease && passwordLengthConstraint
    }
    def checkStringValidity(password:String):Boolean = {
      checkValididy(passwordFromStringToIntArray(password))
    }

    def validPasswordCount(fromPassword:String, toPassword:String):Int = {
      val begin = fromPassword.toInt
      val end = toPassword.toInt

      begin.to(end).count(pass => checkStringValidity(pass.toString))
    }
  }

  // ========================================================================================

  object Part2 {
    def passwordFromStringToIntArray(password:String):Array[Int] = {
      password.toArray.map(ch => ch.toInt - 48)
    }

    def checkValididy(password:Array[Int]):Boolean = {
      def passwordLengthConstraint: Boolean = password.length==6
      def digitsNeverDecrease: Boolean = password.sliding(2).forall{case Array(a,b) => b >= a}
      def noMoreThanTwoAdjacentDigitAreTheSame:Boolean = {
        password.groupBy(v => v).values.exists(_.size == 2)
      }

      passwordLengthConstraint && digitsNeverDecrease &&  noMoreThanTwoAdjacentDigitAreTheSame
    }
    def checkStringValidity(password:String):Boolean = {
      checkValididy(passwordFromStringToIntArray(password))
    }

    def validPasswordCount(fromPassword:String, toPassword:String):Int = {
      val begin = fromPassword.toInt
      val end = toPassword.toInt
      begin.to(end).count(pass => checkStringValidity(pass.toString))
    }
  }
}
