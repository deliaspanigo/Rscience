# New plotly...
fig002 <- plot_ly(data = df_plot003_table)

# Boxplot and info...
fig002 <- add_trace(p = fig002, type = "box",
                           x = ~level ,
                           color = ~level, colors = ~color,
                           lowerfence = ~min, q1 = ~Q1, median = ~median,
                           q3 = ~Q3, upperfence = ~max,
                           boxmean = TRUE, boxpoints = FALSE,
                    boxwidth = 0.5
)

# # # Title and settings...
fig002 <- plotly::layout(p = fig002,
                                title = "Plot 004 - Boxplot with means",
                                font = list(size = 20),
                                margin = list(t = 100))


# # # Without zerolines
fig002 <- plotly::layout(p = fig002,
                                xaxis = list(zeroline = FALSE),
                                yaxis = list(zeroline = FALSE))

#####################################################################


fig001 <- add_trace(p = fig002 ,
          x = minibase_mod$FACTOR,
          y = minibase_mod$VR,
          type = "scatter", mode = "markers",
          #color = minibase_mod$FACTOR,
        #, # color = ~minibase_mod$FACTOR, colors = "red",
          marker = list(symbol = "point",
                        size = 30,
                        opacity = 1,
                        color = minibase_mod$lvl_color))




fig001
#
# # Superponer plot2 sobre plot1
# #subplot(# Obtener los rangos de los ejes X e Y
# range_x <- as.numeric(as.character(levels(minibase_mod$FACTOR)[c(1,3)]))
# range_y <- c(min(minibase_mod$VR), max(minibase_mod$VR))
#
# # Superponer plot2 sobre plot1
# subplot(fig001, fig002, nrows = 2, shareX = TRUE, shareY = TRUE) %>%
#   layout(yaxis = list(range = range_y))
#
#
