# Automated distribution of quantum circuits

This repository was originally used to develop the MSc by Research thesis of Pablo Andres Martinez, which can be found at [https://project-archive.inf.ed.ac.uk/msc/2018-outstanding.html]. A preprint of the paper version of this work can be found at [https://arxiv.org/abs/1811.10972]

The repository contains all the source code of the tool implemented in this project. This tool accepts as input any quantum circuit written in Quipper (https://www.mathstat.dal.ca/~selinger/quipper/) and outputs a circuit distributed across K quantum processing units, where K is chosen by the user.

## Third party software

This project uses Quipper 0.9 [https://www.mathstat.dal.ca/~selinger/quipper/], a quantum programming language. Quipper must be installed in the computer, and the source code must be compiled with command "quipper *.hs".

This project uses KaHyPar, which can be downloaded and built from [https://github.com/SebastianSchlag/kahypar]. As an alternative, you can use PaToH (use option `-fp` when running `./Main`), available at [https://www.cc.gatech.edu/~umit/PaToH/].

## How to use

**Virtual machine:** We have released a stable version of our software, with all dependencies installed within a virtual machine. Download it from [http://homepages.inf.ed.ac.uk/s1775899/docs/Stable.ova], then boot it using some VM software (e.g. VirtualBox, VMWare...) and find further instructions within the README file in the machine's Desktop.

**Important:** You will need the executable for KaHyPar in the same directory where `Main` is. Download it from [https://github.com/SebastianSchlag/kahypar] and build it.

Run the script `compile`, this should create the executable for `Main`. Use `./Main --h` to see the list of options. The program must receive as input a circuit in Quipper's ASCII representation (use Quipper's `print_generic ASCII`, for more information read [https://www.mathstat.dal.ca/~selinger/quipper/doc/Quipper-Internal-Printing.html]). If your circuit is in a file, for instance, file `qft20` provided in this repository, you may run:

`cat circuits/qft20 | ./Main -k=3 -s=7 -o=preview`

The executable `Examples` may be used to generate the ASCII representation of some selected circuits, for instance:

`./Examples qft 15 | ./Main -k=4 -s=4 -o=preview`

`./Examples bfWalk | ./Main -k=6 -s=20 -o=preview`

`./Examples withToffolis | ./Main -cc -k=2 -s=2 -o=preview`

A list of the available circuits from `Examples` can be found in the main function from `Examples.hs`.


