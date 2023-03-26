package org.dvvar

opaque type Vertex = Char
type RouteWithLength = Seq[DirectedEdge]
type Route = Seq[Vertex]

class Graph(possibleRoutes: Map[Vertex, Seq[DirectedEdge]], edges: Map[(Vertex, Vertex), DirectedEdge]) {

  def edgeFor(from: Vertex, to: Vertex): Option[DirectedEdge] = edges.get((from, to))

  def possibleRoutesFor(vertex: Vertex): Seq[DirectedEdge] = possibleRoutes.getOrElse(vertex, List())
}

object Graph {
  def apply(edges: Seq[DirectedEdge]): Graph =
    new Graph(
      edges.groupBy(_.from),
      edges.map { case de@DirectedEdge(from, to, _) => ((from, to), de) }.toMap
    )
}

extension (s: String)
  def asRoute: Route = s.toList.filterNot(_ == '-').map(_.asVertex)

extension (c: Char)
  def asVertex: Vertex = c

case class DirectedEdge(from: Vertex, to: Vertex, length: Int)

object DirectedEdge {
  def create(from: Char, to: Char, length: Int): DirectedEdge = DirectedEdge(from.asVertex, to.asVertex, length)
}

extension (r: RouteWithLength)
  def totalLength: Int = r.map(_.length).sum

