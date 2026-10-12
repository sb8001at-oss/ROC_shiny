function(input, output, session) {
  
  posi <- reactive(rnorm(input$data_n, input$positive_mean, input$positive_sd))
  nega <- reactive(rnorm(input$data_n, input$negative_mean, input$negative_sd))
  
  data_df <- reactive(df_prepare(posi(), nega()))
  
  output$threshold_input <- 
    renderUI(
      sliderInput("threshold", "陰性/陽性の閾値", min = data_df()$detection |> min() |> round(1), max = data_df()$detection |> max() |> round(1), value = data_df()$detection |> median(), step = 0.1)
    )
  
  output$data_densityplot <- renderPlot({
    ggplot(data_df(), aes(x = detection, fill = outcome)) +
      geom_density(alpha = 0.5) +
      labs(x = "検査値", y = "頻度", color = NULL, caption = "赤は陽性、青は陰性の場合の検査値の分布") +
      theme(
        axis.text.x = element_text(size = 15), 
        axis.text.y = element_text(size = 15), 
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        legend.position = "bottom",
        legend.text = element_text(size = 15),
        plot.caption = element_text(size = 13))
  })
  
  roc_d <- reactive({
    thre_f <- function(x){
      if(input$positive_mean > input$negative_mean){
        judge <- ifelse(data_df()$detection > x, "positive", "negative")
      } else {
        judge <- ifelse(data_df()$detection < x, "positive", "negative")
      }
      
      true_negative <- sum(data_df()$outcome == "poor" & judge == "negative")/nrow(data_df())
      true_positive <- sum(data_df()$outcome == "good" & judge == "positive")/nrow(data_df())
      c(threshold = x, true_negative = true_negative, true_positive = true_positive)
    }
    
    thre_vec <- seq(min(data_df()$detection) |> round(1), max(data_df()$detection) |> round(1), by = 0.1)
    
    roc_temp <- mapply(thre_f, thre_vec) |> t() |> as.data.frame()
    
    roc_temp$true_negative <- roc_temp$true_negative |> vec_to_percent()
    roc_temp$true_positive <- roc_temp$true_positive |> vec_to_percent()
    
    roc_temp
  })
  
  output$roc_df <- renderTable(roc_d())
  
  output$roc_plot <- renderPlot({
    data_thre <- roc_d()[roc_d()$threshold |> round(1) == input$threshold |> round(1), ]
    
    youden <- roc_d()$true_negative + roc_d()$true_positive - 100
    max_point <- which(youden == max(youden))
    
    max_point_roc <- roc_d()[max_point, ]
    
    auc <- ifelse(input$positive_mean > input$negative_mean, auc_f(roc_d()), -auc_f(roc_d()))
    
    added_text <- 
      data.frame(
        x = 12.5,
        y = 12.5,
        text = 
          paste0(
            "AUC: ", auc |> round(3), "\n", 
            "閾値: ", max_point_roc$threshold, "\n", 
            "感度: ", max_point_roc$true_positive |> round(1), "\n", 
            "特異度: ", max_point_roc$true_negative |> round(1))
      )

    
    ggplot() +
      geom_path(data = roc_d(), mapping = aes(x = true_negative, y = true_positive), linewidth = 1)+
      geom_point(data = data_thre, mapping = aes(x = true_negative, y = true_positive), size = 10, color = "#00BFC4")+
      geom_point(data = max_point_roc, mapping = aes(x = true_negative, y = true_positive), size = 5, color = "deeppink4")+
      geom_label(data = added_text, mapping = aes(x = x, y = y, label = text), size = 8)+
      scale_x_reverse() +
      labs(x = "特異度（Specificity）", y = "感度（Sensitivity）", color = NULL, caption = "赤丸はYoudenインデックス最大の点、青丸は左の閾値に対応する値") +
      theme(
        axis.text.x = element_text(size = 15), 
        axis.text.y = element_text(size = 15), 
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        legend.position = "bottom",
        legend.text = element_text(size = 15),
        plot.caption = element_text(size = 13))
  })
  
  output$data_thre_plot <- renderPlot({
    d_good <- data_df()[data_df()$outcome == "good", ]
    dens_good <- density(d_good$detection)
    d_g <- data.frame(x = dens_good$x, y = dens_good$y)
    
    d_poor <- data_df()[data_df()$outcome == "poor", ]
    dens_poor <- density(d_poor$detection)
    d_p <- data.frame(x = dens_poor$x, y = dens_poor$y)
    
    if(input$positive_mean < input$negative_mean){
      d_g_below <- d_g[d_g$x < input$threshold, ]
      d_g_above <- d_g[d_g$x > input$threshold, ]
      d_p_below <- d_p[d_p$x < input$threshold, ]
      d_p_above <- d_p[d_p$x > input$threshold, ]
    } else {
      d_g_below <- d_g[d_g$x > input$threshold, ]
      d_g_above <- d_g[d_g$x < input$threshold, ]
      d_p_below <- d_p[d_p$x > input$threshold, ]
      d_p_above <- d_p[d_p$x < input$threshold, ]      
    }
    
    min_x <- c(d_p$x, d_g$x) |> min()
    max_x <- c(d_p$x, d_g$x) |> max()
    max_y <- c(d_p$y, d_g$y) |> max()

    
    p1 <- ggplot()+
      geom_area(aes(x = x, y = y), color = "#F8766D", fill = "white", data = d_g_above)+
      geom_area(aes(x = x, y = y), color = "#F8766D", fill = "#F8766D", data = d_g_below)+
      xlim(c(min_x, max_x))+
      ylim(c(0, max_y)) +
      labs(x = "検査値（陽性のとき）", y = "頻度", color = NULL, caption = "赤いエリアの面積は感度、白いエリアの面積はfalse-positiveの確率、縦線の位置は左の閾値で変化する") +
      theme(
        axis.text.x = element_text(size = 15), 
        axis.text.y = element_text(size = 15), 
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        legend.position = "bottom",
        legend.text = element_text(size = 15),
        plot.caption = element_text(size = 13))
    
    p2 <- ggplot()+
      geom_area(aes(x = x, y = y), color = "#00BFC4", fill = "white", data = d_p_below)+
      geom_area(aes(x = x, y = y), color = "#00BFC4", fill = "#00BFC4", data = d_p_above)+
      xlim(c(min_x, max_x))+
      ylim(c(0, max_y)) +
      labs(x = "検査値（陰性のとき）", y = "頻度", color = NULL, caption = "青いエリアの面積は特異度、白いエリアの面積はfalse-negativeの確率") +
      theme(
        axis.text.x = element_text(size = 15), 
        axis.text.y = element_text(size = 15), 
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        legend.position = "bottom",
        legend.text = element_text(size = 15),
        plot.caption = element_text(size = 13))
    
    p1 / p2
  })
  
  output$pROC_plot1 <- renderPlot({
    plot.roc(data_df()$outcome, data_df()$detection)
  })
  
  output$pROC_plot2 <- renderPlot({
    rocobj <- plot.roc(data_df()$outcome, data_df()$detection, ci = TRUE)
    ciobj <- ci.se(rocobj, specificities=seq(0, 1, 0.01))
    plot(ciobj, type = "shape", col = "#00BFC4")
  })  
  
  output$pROC_plot3 <- renderPlot({
    rocobj <- plot.roc(data_df()$outcome, data_df()$detection)
    lines(smooth(rocobj), col = "#00BFC4")
  })
  
  output$pROC_plot4 <- renderPlot({
    plot.roc(data_df()$outcome, data_df()$detection, ci=TRUE, of="thresholds", thresholds="best", print.thres="best")
  })
}