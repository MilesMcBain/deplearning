library(datapasta)
library("tidyr")
library(rstudioapi)
library(A3)
library("abc")
library(narnia)
library(visdat)
library(dplyr)
library(switchr)
library("rtimicropem")
library(notARealPackage)
require("packagemetrics")
p_load(xml2, jsonlite)
p_load("devtools")

require(packup)

doc <- rstudioapi::getActiveDocumentContext()$contents
depl_check_run()
