lazy val stack = (project.in(file("stack"))).
  settings(
    name := "stack",
    version := "1.0",
    scalaVersion := "2.11.4"
  )


lazy val calculator = (project.in(file("calculator"))).
  settings(
    name := "calculator",
    version := "1.0",
    scalaVersion := "2.11.4"
  )

