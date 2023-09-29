#'@title 
#'
#'@name 
#'
#'@param 
#'
#'
#'@param 
#'
#'
#'@returns 
#'
#'
#'@export
#'
#'@source
#'
#'
#'
muniModel <-
function(y,form,df){
  if(!(y %in% df$kpi || form %in% df$kpi)){
    print("Only the defined for the given municipalities. Also check spelling!")
    stop()
  }
  # Changing the data to wide format 
  df1 <- tidyr::pivot_wider(df,id_cols = c(municipality,period) ,names_from = kpi,values_from=values) 
  
  formula <- formula(paste0(y,'~',paste0(form,collapse = ' + ')))
  
  mod <- lm(formula, data=df1)
  
  base::print(ggplot2::ggplot(data.frame('res'=mod$residuals),ggplot2::aes(x=res) ) + 
                ggplot2::geom_density(fill=8, alpha=0.7) +
                ggplot2::theme_bw()+ ggplot2::ylab('Density') + 
                ggplot2::xlab(paste0('Residuals \n',deparse(formula)))+
                ggplot2::ggtitle('Density plot for the residuals')+
                ggplot2::theme(axis.title.y=ggplot2::element_text(angle=0, vjust=0.5),
                               plot.title = ggplot2::element_text(hjust = 0.5)))
  
  s <- summary(mod) # summary of the model
  
  return(s)
}
