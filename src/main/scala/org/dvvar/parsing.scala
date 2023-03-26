package org.dvvar

def parse(raw: String): Graph =
  Graph(raw.split(", ").toList.map(parseOne))
  
def parse(raw: Seq[String]): Graph =
  Graph(raw.map(parseOne))  

private def parseOne(raw: String): DirectedEdge =
  raw.toList match {
    case from :: to :: lengthStr =>
      val length = lengthStr.mkString("").toIntOption.getOrElse(throw new RuntimeException(s"Invalid vertex: $raw"))
      DirectedEdge.create(from, to, length)
    case _ => throw new RuntimeException(s"Invalid vertex: $raw")
  }

