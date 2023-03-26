Instruction :
Please solve this problem in scala,
Please provide the whole InteliJ Solution inclusive of Unit Test.

Please provide the exercise, as if you provide a piece of code for Review, with target of having it merged in master.



Please provide a reproducible way to evaluate the solution for this input : “AB5, BC4, CD8, DC8, DE6, AD5, CE2, EB3, AE7”




“/*

* The local commuter railroad services a number of towns in LtqLand.

*Because of monetary concerns, all of the tracks are 'one-way.'

* That is, a route from “StructuringTown” to “BoTown” does not imply the existence of a route from “BoTown” to “StructuringTown”.

* In fact, even if both of these routes do happen to exist, they are distinct and are not necessarily the same distance!

*

* The purpose of this problem is to help the railroad provide its customers with information about the routes.

* In particular, you will compute the distance along a certain route, the number of different routes between two towns, and the shortest route between two towns.

*

*

* Input:

* A directed graph where a node represents a town and an edge represents a route between two towns. The weighting of the edge represents the distance between the two towns.

* A given route will never appear more than once, and for a given route, the starting and ending town will not be the same town.

* è AB5 = a route From town “A” To town “B” that has a distance of “5”.

* This Input is the Current road MAP of LtqLand : “AB5, BC4, CD8, DC8, DE6, AD5, CE2, EB3, AE7”
  *Please consider it as a configuration of your system.

*

* Output:

* For test input 1 through 5, if no such route exists, output 'NO SUCH ROUTE'. Otherwise, follow the route as given; do not make any extra stops! For example, the first problem means to start at city A, then travel directly to city B (a distance of 5), then directly to city C (a distance of 4).

*

* 1. The distance of the route A-B-C.

* 2. The distance of the route A-D.

* 3. The distance of the route A-D-C.

* 4. The distance of the route A-E-B-C-D.

* 5. The distance of the route A-E-D.

* 6. The number of trips starting at C and ending at C with a maximum of 3 stops. In the sample data below, there are two such trips: C-D-C (2 stops). and C-E-B-C (3 stops).

* 7. The number of trips starting at A and ending at C with exactly 4 stops. In the sample data below, there are three such trips: A to C (via B,C,D); A to C (via D,C,D); and A to C (via D,E,B).

* 8. The length of the shortest route (in terms of distance to travel) from A to C.

* 9. The length of the shortest route (in terms of distance to travel) from B to B.

* 10. The number of different routes from C to C with a distance of less than 30. In the sample data, the trips are: CDC, CEBC, CEBCDC, CDCEBC, CDEBC, CEBCEBC, CEBCEBCEBC.


You can run `sbt test` and check test results. Also you can add your own tests. 