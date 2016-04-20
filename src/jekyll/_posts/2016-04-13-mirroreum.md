---
layout: post
title:  "Mirroreum - Reproducible Open Research for EUBON"
date:   2016-04-12 21:46:04
---

{% maincolumn 'assets/img/mirroreum.png' 'ROR - Reproducible Open Research' %}

{% newthought 'Mirroreum is a platform' %} for Reproducible Open Research within EUBON and includes various products and software tools produced in the European Biodiversity Observation Network. 

# Architecture

It provides a platform for authoring and sharing reproducible open research analytics, tools and results and builds on a stack of technologies utilizing *docker* and *docker-compose* for integrating microserver architectural software components.

# Authoring

For web-based authoring of open science work, *ROpenSci* is used, extended with EUBON contributions with *RStudio* for web-based scientific analysis.

<http://wrangler.local>

# Sharing

For web-friendly sharing of research results, *Jekyll, nginx and shiny* are used. 

<http://portal.local>