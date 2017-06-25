library(datapasta)
library("tidyr")
library(rstudioapi)

p_load(xml2, josnlite)
p_load("devtools")

require(packup)

test_doc <- rstudioapi::getActiveDocumentContext()
doc <- test_doc$contents
