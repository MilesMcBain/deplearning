library(datapasta)
library("tidyr")
library(rstudioapi)
library(A3)
library("abc")

p_load(xml2, jsonlite)
p_load("devtools")

require(packup)

doc <- rstudioapi::getActiveDocumentContext()$contents

