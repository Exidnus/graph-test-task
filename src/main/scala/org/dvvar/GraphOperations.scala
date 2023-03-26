package org.dvvar

object GraphOperations {
  def distance(route: RouteWithLength): Int = route.map(_.length).sum

  def route(graph: Graph, needRoute: Route): Option[RouteWithLength] = {
    @scala.annotation.tailrec
    def route(needRoute: Route, acc: RouteWithLength): RouteWithLength =
      needRoute match {
        case from :: to :: other =>
          graph.edgeFor(from, to) match {
            case Some(edge) => route(to +: other, edge +: acc)
            case None => Seq()
          }
        case _ => acc
      }

    val result = route(needRoute, Seq())
    if result.isEmpty then None else Some(result)
  }

  def circlesWithMaximumStops(graph: Graph, startEnd: Vertex, maximum: Int): Seq[RouteWithLength] = {
    def current(route: RouteWithLength): Vertex = route.lastOption.map(_.to).getOrElse(startEnd)

    def traverse(step: Int, acc: RouteWithLength): Seq[RouteWithLength] =
      if (step > maximum) List()
      else {
        graph
          .possibleRoutesFor(current(acc))
          .flatMap { edge =>
            if (edge.to == startEnd) List(acc :+ edge)
            else traverse(step + 1, acc :+ edge)
          }
      }

    traverse(1, Seq())
  }

  def circleWithExactStops(graph: Graph, start: Vertex, end: Vertex, number: Int): Seq[RouteWithLength] = {
    def current(route: RouteWithLength): Vertex = route.lastOption.map(_.to).getOrElse(start)

    def traverse(step: Int, acc: RouteWithLength): Seq[RouteWithLength] =
      if (step > number) List()
      else {
        graph
          .possibleRoutesFor(current(acc))
          .flatMap { edge =>
            if (edge.to == end && step == number) List(acc :+ edge)
            else traverse(step + 1, acc :+ edge)
          }
      }

    traverse(1, Seq())
  }

  private def allPossibleRoutesWithoutCycles(graph: Graph, start: Vertex, end: Vertex): Seq[RouteWithLength] = {
    def current(route: RouteWithLength): Vertex = route.lastOption.map(_.to).getOrElse(start)

    def traverse(acc: RouteWithLength, visited: Set[Vertex]): Seq[RouteWithLength] =
      graph
        .possibleRoutesFor(current(acc))
        .filterNot(e => visited(e.to))
        .flatMap { edge =>
          if (edge.to == end) Seq(acc :+ edge)
          else traverse(acc :+ edge, visited + edge.to)
        }

    traverse(Seq(), Set(start))
  }

  def lengthOfTheShortestPath(graph: Graph, start: Vertex, end: Vertex): Option[Int] =
    allPossibleRoutesWithoutCycles(graph, start, end)
      .map(_.totalLength)
      .sorted
      .headOption

  private def allPossibleSingleCycles(graph: Graph, start: Vertex): Seq[RouteWithLength] = {
    def current(route: RouteWithLength): Vertex = route.lastOption.map(_.to).getOrElse(start)

    def traverse(acc: RouteWithLength, visited: Set[Vertex]): Seq[RouteWithLength] = {
      graph
        .possibleRoutesFor(current(acc))
        .filterNot(e => visited(e.to))
        .flatMap { edge =>
          if (edge.to == start) Seq(acc :+ edge)
          else traverse(acc :+ edge, visited + edge.to)
        }
    }

    traverse(Seq(), Set())
  }

  def lengthOfTheShortestSingleCycle(graph: Graph, start: Vertex): Option[Int] =
    allPossibleSingleCycles(graph, start)
      .map(_.totalLength)
      .sorted
      .headOption

  def numberOfSingleCyclesWithLengthNoMoreThan(graph: Graph, start: Vertex, noMoreThan: Int): Int = {
    def current(route: RouteWithLength): Vertex = route.lastOption.map(_.to).getOrElse(start)

    def traverse(acc: RouteWithLength, totalLength: Int): Seq[RouteWithLength] =
      if (totalLength >= noMoreThan) Seq()
      else {
        graph
          .possibleRoutesFor(current(acc))
          .flatMap { edge =>
            val next = acc :+ edge
            if (edge.to == start && totalLength + edge.length < noMoreThan) {
              next +: traverse(next, totalLength + edge.length)
            } else traverse(next, totalLength + edge.length)
          }
      }

    traverse(Seq(), 0).size
  }

  def routeWithoutLength(route: RouteWithLength): Route =
    route match {
      case head :: tail => head.from :: head.to :: tail.map(_.to)
      case Nil => Nil
    }
}
