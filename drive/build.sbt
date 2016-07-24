lazy val root = (project in file("."))
  .enablePlugins(PlayScala)
  .enablePlugins(SbtWeb)
  .settings(
    name := """../drive""",
    version := "0.1",
    scalaVersion := "2.11.8",
    resolvers ++= Seq(
      "sonatype releases" at "http://oss.sonatype.org/content/repositories/releases",
      "scalaz-bintray" at "https://dl.bintray.com/scalaz/releases"
    ),
    // https://github.com/sbt/sbt/issues/2217
    fullResolvers ~= { _.filterNot(_.name == "jcenter") },
    libraryDependencies ++= Seq(
      "org.scalikejdbc"      %% "scalikejdbc"                   % scalikejdbcVersion,
      "org.scalikejdbc"      %% "scalikejdbc-config"            % scalikejdbcVersion,
      "org.scalikejdbc"      %% "scalikejdbc-play-initializer"  % scalikejdbcPlayVersion,
      "org.scalikejdbc"      %% "scalikejdbc-play-fixture"      % scalikejdbcPlayVersion,
      "com.h2database"       %  "h2"                            % h2Version,
      "org.json4s"           %% "json4s-ext"                    % "3.3.+",
      "com.github.tototoshi" %% "play-json4s-native"            % "0.5.+",
      "org.flywaydb"         %% "flyway-play"                   % "3.0.+",
      "org.scalikejdbc"      %% "scalikejdbc-test"              % scalikejdbcVersion  % "test",
      specs2 % "test"
    ),
    initialCommands := """
      import scalikejdbc._, config._
      import models._, utils._
      DBs.setupAll
      DBInitializer.run()
      implicit val autoSession = AutoSession
      val (p, c, s, ps) = (Programmer.syntax("p"), Company.syntax("c"), Skill.syntax("s"), ProgrammerSkill.syntax("ps"))
    """,
    routesGenerator := InjectedRoutesGenerator,
    scalikejdbcSettings // http://scalikejdbc.org/documentation/setup.html
  ).settings(scalariformSettings)

lazy val scalikejdbcVersion = scalikejdbc.ScalikejdbcBuildInfo.version
lazy val scalikejdbcPlayVersion = "2.5.+"
lazy val h2Version = "1.4.+"
