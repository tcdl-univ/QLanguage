resolvers += "Uqbar Central" at "http://uqbar-wiki.org/mvn/releases"
resolvers += Classpaths.sbtPluginReleases

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.0.4")

addSbtPlugin("org.uqbar" % "sbt-flexible-dependencies" % "latest.integration")

addSbtPlugin("com.github.gseitz" % "sbt-release" % "0.8.3")

addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "2.5.0")