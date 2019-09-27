name := "hangman-scala"

version := "0.1"

scalaVersion := "2.12.6"

resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"

scalacOptions ++= Seq(
      "-deprecation"
    , "-unchecked"
    , "-encoding", "UTF-8"
    , "-Xlint"
    , "-Xverify"
    , "-feature"
    , "-Ypartial-unification"
    , "-Xlint:-unused"
    , "-language:_"
)

libraryDependencies ++= Seq( "org.scalactic" %% "scalactic" % "3.0.5",
                             "org.scalatest" %% "scalatest" % "3.0.5" % "test",
                             "org.typelevel" %% "cats-effect" % "2.0.0",
                             "org.typelevel" %% "cats-core" % "2.0.0",
                             "com.typesafe" % "config" % "1.3.4")
