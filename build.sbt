name := "TheoryCategoryLearning"

version := "1.0"

scalaVersion := "2.13.3"

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.3" cross CrossVersion.full)

libraryDependencies ++= Seq(
 "org.scalactic" %% "scalactic" % "3.2.0",
   "org.scalatest" %% "scalatest" % "3.2.0" % "test"
)
resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"