## Automated distribution of quantum circuits

This repository was originally used to develop the MSc by Research thesis of Pablo Andres Martinez, which can be found at [https://project-archive.inf.ed.ac.uk/msc/2018-outstanding.html]. A preprint of the paper version of this work can be found at [https://arxiv.org/abs/1811.10972]

The repository contains all the source code of the tool implemented in this project. This tool accepts as input any quantum circuit written in Quipper (https://www.mathstat.dal.ca/~selinger/quipper/) and outputs a circuit distributed across K quantum processing units, where K is chosen by the user.

# Third party software

This project uses Quipper 0.8 (https://www.mathstat.dal.ca/~selinger/quipper/), a quantum programming language. Quipper must be installed in the computer, and the source code must be compiled with command "quipper Distributer.hs Examples.hs config.hs".

This project uses KaHyPar, a hypergraph patitioner available at GitHub (https://github.com/SebastianSchlag/kahypar). Binaries on this repository will probably not work in your computer, you should follow KaHyPar's GitHub page instructions to compile KaHyPar and use that binary instead. As an alternative, you can use PaToH, which can be downloaded from [https://www.cc.gatech.edu/~umit/PaToH/].

