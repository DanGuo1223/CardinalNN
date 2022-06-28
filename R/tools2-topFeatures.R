setMethod("topFeatures", "MiCNN_classification",
          function(object, train, test, ..., n = 10, method = c('lime', 'shap'), instance_id = 1)
          {
            model_name = modelData(object)$model_name
            train_name = deparse(substitute(train))
            test_name = deparse(substitute(test))
            if (method == 'lime')
            {
              py_string = paste0("from classification import explain\n",
                                 "feature = explain.explain(", model_name,
                                 ", train_data = r.", train_name, ", test_data = r.", test_name, ", num_features = ", 
                                 n, ", instance_id = ", instance_id," )\n",
                                 sep = '')
            }
            
            print(py_string)
            
            py_run_string(py_string)
            out <- py$feature
            out <- DataFrame(mz = c(out[seq(1,nrow(out),by=2),]), coef = as.numeric(c(out[seq(2,nrow(out),by=2),])))
            out <- out[order(abs(out$coef),decreasing = T), ]
            out <- SummaryDataFrame(as.list(out), .rownumbers=TRUE,
                                    .summary="Top-ranked features:")
            return(out)
          })
