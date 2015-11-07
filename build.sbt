name := "Appathon_desktop"

version := "1.0"

resolvers += "scalaz-bintray" at "https://dl.bintray.com/scalaz/releases"

lazy val `appathon_desktop` = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(evolutions, cache, ws, specs2 % Test)


libraryDependencies ++= Seq(
  "me.figo" % "sdk" % "1.2.5",
  "com.typesafe.slick" %% "slick" % "3.0.0",
  "com.typesafe.play" %% "play-slick" % "1.0.0",
  "com.typesafe.play" %% "play-slick-evolutions" % "1.0.0",
  "com.typesafe.akka" %% "akka-actor" % "2.3.12",
  "com.typesafe.akka" %% "akka-slf4j" % "2.3.12",
  "com.zaxxer" % "HikariCP" % "2.3.7",
  "com.h2database" % "h2" % "1.4.187"
)

unmanagedResourceDirectories in Test <+= baseDirectory(_ / "target/web/public/test")