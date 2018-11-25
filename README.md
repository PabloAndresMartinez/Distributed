## Automated distribution of quantum circuits

This repository was originally used to develop the MSc by Research thesis of Pablo Andres Martinez, which can be found at [https://project-archive.inf.ed.ac.uk/msc/2018-outstanding.html]. We intend to publish the results of this work, and a preprint will be posted in arXiv soon.

The repository contains, inside folder Code/ all the source code of the tool implemented in this project. This tool accepts as input any quantum circuit written in Quipper (https://www.mathstat.dal.ca/~selinger/quipper/) and outputs a circuit distributed across K quantum processing units, where K is chosen by the user. The rest of the folders contain figures, data and the text of the Master's thesis.

# Third party software

This project uses Quipper 0.8 (https://www.mathstat.dal.ca/~selinger/quipper/), a quantum programming language. Quipper must be installed in the computer, and the source code must be compiled with command "quipper Distributer.hs Examples.hs config.hs".

This project uses KaHyPar, a hypergraph patitioner available at GitHub (https://github.com/SebastianSchlag/kahypar). Binaries on this repository will probably not work in your computer, you should follow KaHyPar's GitHub page instructions to compile KaHyPar and use that binary instead.

