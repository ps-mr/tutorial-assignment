[optimus]: https://github.com/vagm/Optimus
[b39a]: https://github.com/vagm/Optimus/tree/b39a8b0398bf4f9154f128a268f2fd7ad35f98e9
[jin]: http://lpsolve.sourceforge.net/5.5/Java.htm


# tutorial-assignment
Tutorial assignment for the lecture "Informatik 1", winter semester 2015/16, University of Tübingen


### Installation

1. Clone [Optimus][optimus]. Tested with version [b39a8b0398bf4f9154f128a268f2fd7ad35f98e9][b39a].

         git clone https://github.com/vagm/Optimus
         ### optional: git checkout b39a8b0398bf4f9154f128a268f2fd7ad35f98e9


3. The `oJalgo` solver fails our integer programming instance about 2014's students
   with dummy tutors. The `lp_solve` instance doesn't work in a 64 bit Mac. So,
   install the proprietory solver `gurobi`. Request an academic license. Save the
   license in the default location (_important_!). Find out the installation
   directory through paths to the example files mentioned in the getting-started
   documentation. Locate the folder `lib` containing `gurobi.jar`. Make a symbolic
   link of this folder into the `Optimus` directory.

2. Build optimus and publish locally.

         cd Optimus
         ln -s <gurobi-lib-dir-from-previous-step> .
         sbt compile
         sbt publishLocal


3. This repository can now be built with `sbt` as usual.
