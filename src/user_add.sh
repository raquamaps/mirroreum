#!/bin/bash
# execute this script as super user! 
# (ie chmod +x and sudo ./scriptname.sh)
sudo useradd -m -s /bin/bash --skel /home/rstudio/skeleton -d /home/rstudio/researchers/markus.skyttner -p `openssl passwd -1 q7E5xavg` markus.skyttner
sudo useradd -m -s /bin/bash --skel /home/rstudio/skeleton -d /home/rstudio/researchers/sven.kullander -p `openssl passwd -1 NbwGJyKL` sven.kullander
