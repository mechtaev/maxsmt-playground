package edu.nus.maxsmtplay

import z3.scala._
import java.io._

trait Printer {
  this: Z3 =>

  def printConstraints(label: String, astList: List[Z3AST]) = {
    println(label + ": ")
    astList.map(println)
  }

  def writeToFile(file: String, content: String) {
    val pw = new java.io.PrintWriter(new File(file))
    try pw.write(content)
    finally pw.close()
  }

  def writeLog(name: String, content: String) {
    val file = new File(".").getAbsolutePath() + "/log/" + name + ".log"
    writeToFile(file, content)
  }
}
