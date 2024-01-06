FROM rocker/shiny:4.2.1
RUN install2.r rsconnect shiny dplyr tidyverse shinyWidgets
WORKDIR /home/shinytweet
COPY pitchTempoSHinyDemo.R
COPY pbp.db pbp.db
COPY deploy.R deploy.R
CMD Rscript deploy.R
