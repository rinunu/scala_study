name := "scala_study"

version := "1.0"

scalaVersion := "2.9.1"

resolvers ++= Seq(
        "Twitter" at "http://maven.twttr.com")

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2" % "1.11" % "test",
  "org.scalatest" %% "scalatest" % "1.8" % "test",
  "com.rabbitmq" % "amqp-client" % "2.7.1",
  "com.twitter" % "finagle-http" % "1.9.10",
  "org.scalaquery" %% "scalaquery" % "0.10.0-M1",
  "com.twitter" %% "twitter_future-http" % "3.0.0",
  "mysql" % "mysql-connector-java" % "5.1.20"
  )



