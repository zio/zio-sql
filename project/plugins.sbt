addSbtPlugin("org.scala-js"                      % "sbt-scalajs"               % "1.7.0")
addSbtPlugin("org.portable-scala"                % "sbt-scalajs-crossproject"  % "1.2.0")
addSbtPlugin("org.scalameta"                     % "sbt-scalafmt"              % "2.5.0")
addSbtPlugin("pl.project13.scala"                % "sbt-jmh"                   % "0.4.3")
addSbtPlugin("com.eed3si9n"                      % "sbt-buildinfo"             % "0.11.0")
addSbtPlugin("org.scoverage"                     % "sbt-scoverage"             % "2.0.6")
addSbtPlugin("org.scalameta"                     % "sbt-mdoc"                  % "2.2.22")
addSbtPlugin("ch.epfl.scala"                     % "sbt-bloop"                 % "1.5.3")
addSbtPlugin("com.eed3si9n"                      % "sbt-unidoc"                % "0.4.3")
addSbtPlugin("com.github.sbt"                    % "sbt-ci-release"            % "1.5.9")
addSbtPlugin("com.github.cb372"                  % "sbt-explicit-dependencies" % "0.2.16")
addSbtPlugin("com.thoughtworks.sbt-api-mappings" % "sbt-api-mappings"          % "3.0.2")
addSbtPlugin("ch.epfl.scala"                     % "sbt-scalafix"              % "0.10.1")
addSbtPlugin("io.github.davidgregory084"         % "sbt-tpolecat"              % "0.4.1")
addSbtPlugin("dev.zio"                           % "zio-sbt-website"           % "0.0.0+84-6fd7d64e-SNAPSHOT")

resolvers += Resolver.sonatypeRepo("public")
