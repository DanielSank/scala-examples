# scala-examples
Little examples of how to use the Scala language

# Setup

* Install and set up eclipse
  * Download eclipse from https://eclipse.org/downloads/.
  I stick it in `~/lib` such that I have an executable `~/lib/eclipse/eclipse`.
  * Add the scala plugin to eclipse.
    * Go to `Help > Install New Software...`.
    In the Available Software dialog, click on the Available Software Sites link and add the software update site URL given below for each plugin.
    Then back in the Available Software dialog you can select the update site, select the plugin code, and install.
    * Scala eclipse plugin: http://download.scala-ide.org/sdk/lithium/e44/scala211/stable/site.
    You’ll definitely want the “Scala IDE for Eclipse” feature and probably the “Scala IDE plugins (incubation)” feature.
    The others are not needed.
* Install sbt. Just download it and put it in `~/bin` (and make sure `~/bin` is on your PATH).
* Clone this repo (e.g. into `~/src/scala-examples`).
* Navigate to the cloned repo and then in the terminal type `$ sbt eclipse`. This generates eclipse project files.
* Open eclipse, go to `File > Import`, select `Existing Projects into Workspace`, and then select the directory containing the project, i.e. the directory into which you cloned this repo.

# Run

In sbt, just type `> run` and then when you see a menu of options, pick which example to run.
