ExportR
=======

Small application for exporting data from the ProjMan database

Install instructions
---------------------
This requires that `rJava` is installed and configured. On Ubuntu this is easiest to do via the system packages:

```
sudo apt-get install r-cran-rjava
sudo R CMD javareconf
```

Then we need to install the other dependency packages

```
R -e 'install.packages(c("RJDBC", "devtools"))'
R -e 'devtools::install_github("imanuelcostigan/RSQLServer", ref="8bccb4b6921e85ea929f811967b40ec2e4ab37b4")'
R -e 'devtools::install_github("Molmed/projmanr", ref="0.1")' 
```