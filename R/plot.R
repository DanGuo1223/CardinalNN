setMethod("plot", signature = c(x = "SummaryDataFrame", y = "missing"),
          function(x, groups = NULL,  xlabel = 'm/z', ylabel = 'Coefficient', 
                   col = c("#FFC20A", "#0C7BDC"))
          {
            
            df <- data.frame('mz' = x@listData$mz, 'coef' = x@listData$coef)
            df$type <- ifelse(df$coef < 0, "neg", "pos")
            df <- df[order(abs(df$coef)), ]
            
            df$mz <- factor(df$mz, levels = df$mz)
            
            ggplot(df, aes(x = mz, y = coef, label = coef)) + 
              geom_bar(stat='identity', aes(fill=type), width=.5)  +
              #geom_text(aes(label = coef), vjust=0, hjust=1) +
              scale_fill_manual(
                labels = c("Supports", "Contradicts"), 
                values = c("pos"=col[1], 'neg' = col[2])) + 
              xlab(xlabel)+
              ylab(ylabel)+
              coord_flip()
            
            
          })
