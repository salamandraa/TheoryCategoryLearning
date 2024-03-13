name := "TheoryCategoryLearning"

version := "1.0"

scalaVersion := "2.13.3"

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.13.3" cross CrossVersion.full)

libraryDependencies ++= Seq(
  "org.scalactic" %% "scalactic" % "3.2.18",
  "org.scalatest" %% "scalatest" % "3.2.18" % "test",
  "org.typelevel" %% "cats-core" % "2.10.0",
  "org.typelevel" %% "cats-effect" % "2.2.0",
  // https://mvnrepository.com/artifact/org.scalaz/scalaz-core
  //  "org.scalaz" %% "scalaz-core" % "7.4.0-M6"

 "com.github.julien-truffaut" %% "monocle-core" % "3.0.0-M6" % "test",
 "com.github.julien-truffaut" %% "monocle-macro" % "3.0.0-M6"  % "test"

)
resolvers += "Artima Maven Repository" at "https://repo.artima.com/releases"