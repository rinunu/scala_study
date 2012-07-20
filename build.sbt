name := "scala_study"

version := "1.0"

scalaVersion := "2.9.1"

resolvers ++= Seq(
        "Twitter" at "http://maven.twttr.com")

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2" % "1.11" % "test",
  "org.scalatest" %% "scalatest" % "1.8" % "test",
  "com.twitter" %% "finagle-http" % "4.0.2",
  "org.scalaquery" %% "scalaquery" % "0.10.0-M1",
  "org.clapper" %% "grizzled-slf4j" % "0.6.9",
  "ch.qos.logback" % "logback-classic" % "1.0.0",
  "com.rabbitmq" % "amqp-client" % "2.8.4",
  "mysql" % "mysql-connector-java" % "5.1.20"
  )



