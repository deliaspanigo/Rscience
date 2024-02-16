library(plotly)

fig <- plot_ly()
# Add traces
fig <- fig %>%
  add_trace(data = minibase_mod,
            showlegend = TRUE,
    #data = violinplot_data,
    type = "violin",
    points = "all",
    y = ~VR,
    x = ~FACTOR,
    split = ~FACTOR,
    name = "Violinplot",
    color = ~FACTOR,
    colors = df_plot003_table$color

  )

ay <- list(
  tickfont = list(color = "red"),
  overlaying = "y",
  side = "right",
  title = "<b>secondary</b> yaxis title")






# fig <- fig %>% add_trace(x = ~2:4, y = ~4:6, name = "yaxis 2 data",
#                          yaxis = "y2", mode = "lines+markers", type = "scatter")


fig <- add_trace(p = fig, type = "box",
                 #name = df_plot003_table$level,
                 x = df_plot003_table$level ,
                 #fillcolor = I(df_plot003_table$color),
                 #color = df_plot003_table$level,
                 #colors = df_plot003_table$color,
                 #split = df_plot003_table$lvl,
                 split = df_plot003_table$level,
                 lowerfence = df_plot003_table$min,
                 q1 = df_plot003_table$Q1,
                 median = df_plot003_table$median,
                 q3 = df_plot003_table$Q3,
                 upperfence = df_plot003_table$max,
                 boxmean = TRUE, boxpoints = FALSE
                 #marker = list(colors = df_plot003_table$color)
)

# Set figure title, x and y-axes titles
fig <- fig %>% layout(
  title = "Double Y Axis Example", yaxis2 = ay,
  xaxis = list(title="xaxis title "),
  yaxis = list(title="<b>primary</b> yaxis title")
)#%>%
  # layout(plot_bgcolor='#e5ecf6',
  #        xaxis = list(
  #          zerolinecolor = '#ffff',
  #          zerolinewidth = 2,
  #          gridcolor = 'ffff'),
  #        yaxis = list(
  #          zerolinecolor = '#ffff',
  #          zerolinewidth = 2,
  #          gridcolor = 'ffff')
  # )

# # # Without zerolines...
fig  <- plotly::layout(p = fig ,
                                xaxis = list(zeroline = FALSE),
                                yaxis = list(zeroline = FALSE))

fig
