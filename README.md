# mirroreum

A platform for authoring and publishing Reproducible Open Research, for example using R packages produced within EUBON

# Introduction

This is a collection of micro-services running together using `docker` and `docker-compose`.

* Authoring tool: web-enabled RStudio provides rich analytical functionality

* Application server: shiny provides interactive web applications from R packages

* Web server: for static documents or static reports, nginx is used to publish these

# Using

There is a Makefile available:

```console
# to start
make 

# to remove and clean out
make clean
```

