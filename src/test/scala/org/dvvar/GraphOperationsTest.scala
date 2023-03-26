package org.dvvar

import org.scalatest.{Assertions, Inside}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GraphOperationsTest extends AnyFlatSpec with Inside with Matchers {

  private val input = "AB5, BC4, CD8, DC8, DE6, AD5, CE2, EB3, AE7"
  private val graph = parse(input)

  "Distance of A-B-C in 'AB5, BC4, CD8, DC8, DE6, AD5, CE2, EB3, AE7'" should "be 9" in  {
    inside(GraphOperations.route(graph, "ABC".asRoute)) {
      case Some(route) => GraphOperations.distance(route) shouldBe 9
    }
  }

  "Distance of A-D in 'AB5, BC4, CD8, DC8, DE6, AD5, CE2, EB3, AE7'" should "be 5" in {
    inside(GraphOperations.route(graph, "AD".asRoute)) {
      case Some(route) => GraphOperations.distance(route) shouldBe 5
    }
  }

  "Distance of A-D-C in 'AB5, BC4, CD8, DC8, DE6, AD5, CE2, EB3, AE7'" should "be 5" in {
    inside(GraphOperations.route(graph, "ADC".asRoute)) {
      case Some(route) => GraphOperations.distance(route) shouldBe 13
    }
  }

  "Distance of A-E-B-C-D in 'AB5, BC4, CD8, DC8, DE6, AD5, CE2, EB3, AE7'" should "be 5" in {
    inside(GraphOperations.route(graph, "AEBCD".asRoute)) {
      case Some(route) => GraphOperations.distance(route) shouldBe 22
    }
  }

  "Route of A-E-D in 'AB5, BC4, CD8, DC8, DE6, AD5, CE2, EB3, AE7'" should "not be found" in {
    GraphOperations.route(graph, "AED".asRoute).isEmpty shouldBe true
  }

  "For circle with maximum 3 stops from vertex C in graph 'AB5, BC4, CD8, DC8, DE6, AD5, CE2, EB3, AE7'" should "be two routes: C-D-C, C-E-B-C" in {
    GraphOperations
      .circlesWithMaximumStops(graph, 'C'.asVertex, maximum = 3)
      .map(GraphOperations.routeWithoutLength)
      .toSet shouldBe Set("CDC".asRoute, "CEBC".asRoute)
  }

  "For circle with maximum 2 stops from vertex C in graph 'AB5, BC4, CD8, DC8, DE6, AD5, CE2, EB3, AE7'" should "be one route: C-D-C" in {
    GraphOperations
      .circlesWithMaximumStops(graph, 'C'.asVertex, maximum = 2)
      .map(GraphOperations.routeWithoutLength)
      .toSet shouldBe Set("CDC".asRoute)
  }

  "For circle with maximum 10 stops from vertex A in graph 'AB5, BC4, CD8, DC8, DE6, AD5, CE2, EB3, AE7'" should "be no routes" in {
    GraphOperations
      .circlesWithMaximumStops(graph, 'A'.asVertex, maximum = 3)
      .map(GraphOperations.routeWithoutLength)
      .toSet shouldBe Set()
  }

  "For route with with 4 stops from A to C in graph 'AB5, BC4, CD8, DC8, DE6, AD5, CE2, EB3, AE7'" should "be three variants: A-B-C-D-C, A-D-C-D-C, A-D-E-B-C" in {
    GraphOperations
      .circleWithExactStops(graph, 'A'.asVertex, 'C'.asVertex, number = 4)
      .map(GraphOperations.routeWithoutLength)
      .toSet shouldBe Set("ABCDC".asRoute, "ADCDC".asRoute, "ADEBC".asRoute)
  }

  "For route with 1 stop from D to C in graph 'AB5, BC4, CD8, DC8, DE6, AD5, CE2, EB3, AE7'" should "be one variant: D-C" in {
    GraphOperations
      .circleWithExactStops(graph, 'D'.asVertex, 'C'.asVertex, number = 1)
      .map(GraphOperations.routeWithoutLength)
      .toSet shouldBe Set("DC".asRoute)
  }

  "Length of the shortest route from A to C in graph 'AB5, BC4, CD8, DC8, DE6, AD5, CE2, EB3, AE7'" should "9" in {
    inside(GraphOperations.lengthOfTheShortestPath(graph, 'A'.asVertex, 'C'.asVertex)) {
      case Some(length) => length shouldBe 9
    }
  }

  "Length of the shortest route from A to B in graph 'AB5, BC4, CD8, DC8, DE6, AD5, CE2, EB3, AE7'" should "5" in {
    inside(GraphOperations.lengthOfTheShortestPath(graph, 'A'.asVertex, 'B'.asVertex)) {
      case Some(length) => length shouldBe 5
    }
  }

  "Length of the shortest route from C to A in graph 'AB5, BC4, CD8, DC8, DE6, AD5, CE2, EB3, AE7'" should "be absent (no route)" in {
    inside(GraphOperations.lengthOfTheShortestPath(graph, 'C'.asVertex, 'A'.asVertex)) {
      case None => Assertions.succeed
    }
  }

  "Length of the shortest single cycle from B to B in graph 'AB5, BC4, CD8, DC8, DE6, AD5, CE2, EB3, AE7'" should "be 9" in {
    inside(GraphOperations.lengthOfTheShortestSingleCycle(graph, 'B'.asVertex)) {
      case Some(length) => length shouldBe 9
    }
  }

  "Number of cycles from C in graph 'AB5, BC4, CD8, DC8, DE6, AD5, CE2, EB3, AE7'" should "be 7" in {
    GraphOperations.numberOfSingleCyclesWithLengthNoMoreThan(graph, 'C'.asVertex, noMoreThan = 30) shouldBe 7
  }
}
