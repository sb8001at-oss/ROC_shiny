ui <- page_sidebar(
  title = "ROCシミュレーション",
  theme = bs_theme(bootswatch = "united"),
  
  sidebar = sidebar(
    sliderInput("positive_mean", "陽性-平均値", min = 0, max = 100, value = 15, step = 1),
    sliderInput("positive_sd", "陽性-標準偏差", min = 0.1, max = 100, value = 5, step = 0.25),
    sliderInput("negative_mean", "陰性-平均値", min = 0, max = 100, value = 5, step = 1),
    sliderInput("negative_sd", "陰性-標準偏差", min = 0.1, max = 100, value = 5, step = 0.25),
    sliderInput("data_n", "各群のデータ数", min = 10, max = 200, value = 50, step = 10),
    uiOutput("threshold_input")
  ),
  navset_card_underline(
    nav_panel("データの分布", card(card_header("分布（確率密度）"), plotOutput("data_densityplot"))),
    nav_panel("データ：陽性と陰性", card(card_header("分布と陽性・陰性の割合"), plotOutput("data_thre_plot"))),
    nav_panel("ROCの表", card(card_header("ROCの閾値と感度・特異度"), tableOutput("roc_df"))),
    nav_panel("ROC（グラフ）", card(card_header("ROC"), plotOutput("roc_plot"))),
    nav_panel(
      "pROCパッケージ", 
      accordion(
        multiple = FALSE,
        height = "750px",
        accordion_panel(
          "グラフ1",
          layout_columns(
            card(card_header("ROCプロット"), plotOutput("pROC_plot1"), height = "600px"),
            card(card_header("ROC（95%信頼区間）"), plotOutput("pROC_plot2"), height = "600px")
          )
        ),
        accordion_panel(
          "グラフ2",
          layout_columns(
            card(card_header("ROC（スムージング）"), plotOutput("pROC_plot3"), height = "600px"),
            card(card_header("ROC（閾値：信頼区間）"), plotOutput("pROC_plot4"), height = "600px")
          )
        )
      )
    )
  )
)
  