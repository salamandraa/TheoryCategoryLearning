name := "TheoryCategoryLearning"

version := "1.0"

scalaVersion := "2.13.3"


libraryDependencies ++= Seq(
 "org.scalactic" %% "scalactic" % "3.2.0",
   "org.scalatest" %% "scalatest" % "3.2.0" % "test"
)
resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"