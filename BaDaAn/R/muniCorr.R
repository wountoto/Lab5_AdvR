muniCorr <-
function(df){
  df1 <- tidyr::pivot_wider(df,id_cols = c(municipality,period) ,names_from = kpi,values_from=values)
  
  graphics::pairs(df1[,-c(1:4)], gap = .2, col = "darkgreen", pch = 19, lower.panel = NULL)
  base::print(stats::cor(df1[,-c(1:4)]))
}
