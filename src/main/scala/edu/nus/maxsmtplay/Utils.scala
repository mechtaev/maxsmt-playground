package edu.nus.maxsmtplay

object BooleanImplication {
 
  implicit def extendedBoolean(a: Boolean) = new {
    def implies(b: => Boolean) = {
      !a || b
    }
  }

}

object UniqueName {

  var id = 0

  def withPrefix(prefix: String): String = {
    id += 1
    prefix + "!" + id
  }

}
