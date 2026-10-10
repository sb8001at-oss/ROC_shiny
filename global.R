library(shiny)
library(bslib)
library(ggplot2)
library(patchwork)
library(pROC)
library(showtext)

font_paths("./")
font_add("noto", "NotoSansJP-VariableFont_wght.ttf")
showtext_auto()

df_prepare <- function(posi, nega){
  l <- length(posi)
  data.frame(
    outcome = rep(c("good", "poor"), c(l, l)),
    detection = c(posi, nega)
  )
}

auc_f <- function(d){
  v1 <- head(d$true_positive, -1)
  v2 <- d$true_positive |> _[-1]
  sum((v1 + v2) * d$true_negative |> diff() / 2)/10000
}

vec_to_percent <- function(vec){
  min_v <- min(vec)
  max_v <- max(vec)
  (vec - min_v) / (max_v - min_v) * 100
}

