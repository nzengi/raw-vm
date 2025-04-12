name := "zk-dag-pos-blockchain"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.13.8"

libraryDependencies ++= Seq(
  // Akka (aktör modeli ve ağ iletişimi için)
  "com.typesafe.akka" %% "akka-actor-typed" % "2.6.20",
  "com.typesafe.akka" %% "akka-stream" % "2.6.20",
  "com.typesafe.akka" %% "akka-http" % "10.2.10",
  
  // JSON işleme
  "io.circe" %% "circe-core" % "0.14.2",
  "io.circe" %% "circe-generic" % "0.14.2",
  "io.circe" %% "circe-parser" % "0.14.2",
  
  // Kriptografi kütüphaneleri
  "org.bouncycastle" % "bcprov-jdk15on" % "1.70",
  "org.bouncycastle" % "bcpkix-jdk15on" % "1.70",
  
  // Loglama
  "ch.qos.logback" % "logback-classic" % "1.4.5",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5",
  
  // Test
  "org.scalatest" %% "scalatest" % "3.2.15" % Test
)

// Assembly ayarları (çalıştırılabilir JAR için)
assembly / assemblyJarName := "zk-dag-pos-blockchain.jar"
assembly / mainClass := Some("core.Main")

// Derleyici ayarları
scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Xlint"
)

// Paketleme ayarları
Compile / packageBin / mappings += {
  val confFile = (Compile / resourceDirectory).value / "application.conf"
  confFile -> "application.conf"
}

// Kaynak klasörleri
Compile / unmanagedSourceDirectories += baseDirectory.value / "core"
Compile / unmanagedSourceDirectories += baseDirectory.value / "provi" 