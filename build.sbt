name := "TheoryCategoryLearning"

version := "1.0"

scalaVersion := "2.13.3"

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.3" cross CrossVersion.full)

libraryDependencies ++= Seq(
  "org.scalactic" %% "scalactic" % "3.2.0",
  "org.scalatest" %% "scalatest" % "3.2.0" % "test",
  "org.typelevel" %% "cats-core" % "2.2.0",
  "org.typelevel" %% "cats-effect" % "2.2.0"
  // https://mvnrepository.com/artifact/org.scalaz/scalaz-core
  //  "org.scalaz" %% "scalaz-core" % "7.4.0-M6"
)
resolvers += "Artima Maven Repository" at "https://repo.artima.com/releases"