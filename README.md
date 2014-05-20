`ibd_particle`
=============

`ibd_particle` is a program for sampling identity by descent (IBD) among individuals in a population from genetic marker data.  While it is not a part of the 
 [MORGAN][morg] software suite, it was developed in the same research group and as a result uses similar configuration, input, and output file formats. In many cases this documentation will refer to MORGAN formats and documentation.


Running 
-------

The program is invoked as 

     ibd_particle [parameter file]

All configuration for an analysis is specified in in the parameter file.

It is recommended to enable multicore processing with

     ibd_particle [parameter file] +RTS -N

Output
-----------------
A single [MORGAN][morg] format IBD graph file containing realizations of IBD on the input chromosomes.

Configuration
-----------------

Parameters for a run of ibd\_particle are given as statements in a text file.  There is no formal syntax for the various options.

* `input marker data file input.markers`   
Specifies the file of SNP data to use in the analysis.  This file comes in [MORGAN][morg] marker format; see that documentation or the example for more information.


* `select population kinship 0.05`  
Sets the model's level of population kinship, or the probability of IBD between two alleles.


* `set kinship change rate 0.1`  
A scaling parameter for the change rate of the hidden Markov process.


* `set genotyping error rate 0.01`  
The model probability that a given allele is observed with error, in which case it is assumed to be drawn from the population allele distribution.

* `set particles 1000`
The number of particles used by the particle Gibbs sampler.

* `set iterations 1000`
The length of the particle Gibbs Markov chain to be sampled.

* `set seed 12345`   
The seed for the pseudorandom number generator.

Download 
-----------------

### Binary
A 64-bit Linux binary is available [here](http://www.stat.washington.edu/~cglazner/ibd_particle). It was built on Ubuntu 12.04 LTS.  You must have `gsl` and `lapack` installed to run it; on Ubuntu, this is

       apt get install libgsl0ldbl
       apt get install liblapack3gf

### Build from source
Building requires the [Haskell Platform](http://www.haskell.org/platform/).  Building has only been tested on Ubuntu 12.04 LTS, but the following steps should be cross-platform. There are other ways to install a cabal package, but this has worked best for me. Testing performed with GHC 7.4.1.

1. Ensure that you have the libraries `gsl` and `lapack` installed.  On Ubuntu this can be done with

         apt-get install libgsl0-dev
         apt-get install liblapack-dev

2. If it is not already installed, install `cabal-dev`

         cabal install cabal-dev

3. Clone the github repository, and move into the new directory
         
         git clone https://github.com/cglazner/ibd_particle

4. Build the cabal package, installing dependencies locally

         ~/.cabal/bin/cabal-dev install 

5. The executable can be found at `./cabal-dev/bin/ibd_particle`


[morg]: http://www.stat.washington.edu/thompson/Genepi/MORGAN/Morgan.shtml "linku"
