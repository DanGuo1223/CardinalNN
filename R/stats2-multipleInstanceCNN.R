setMethod("multipleInstanceCNN",
          signature = c("SparseImagingExperiment", "ANY"),
          function(x, y, model_name = 'bladder_classifier1', groups  = run(x), 
                   method = c('multiple_instance', 'fully_supervised'), batch_size = 32, 
                   optim = 'Adam', lr = 0.00001, num_epochs =10)
          {
            .checkForIncompleteProcessing(x)
            
            groups <- as.factor(rep_len(groups, ncol(x)))
            method <- match.arg(method)
            
            train = data.frame(label= y, sample_id = x$sample, x = coord(x)$x, y = coord(x)$y)
            spec = as.data.frame(as.matrix(t(spectra(x))))
            colnames(spec) <- paste0('mz',mz(x))
            train = cbind(train, spec)
            train <<- train
            
            results = list()
            
            y <- as.factor(y)
            
            if(length(y) != ncol(x)){
              stop("The length of labels does not match the length of samples")   
            }
            
            if (method == 'multiple_instance')
            {
              .message("fitting multiple instance convolutional network...")
              results[[1]] = .miCNN_fit(train, model_name, batch_size, 
                                        optim, lr, num_epochs)
            }
            
            if (method == 'fully_supervised')
            {
              .message("fitting fully supervised convolutional network...")
              results[[1]] = .CNN_fit(train, model_name, batch_size, 
                                      optim, lr, num_epochs)
            }
            
            #results <- do.call('c', results)
            
            out <- .MiCNN_classification(
              imageData=.SimpleImageArrayList_nn(),
              featureData= featureData(x),
              elementMetadata = pixelData(x),
              metadata=list(
                batch_size = batch_size,
                learning_rate = lr,
                optimizer = optim,
                epochs = num_epochs,
                method=method),
              resultData=as(results, "List"),
              modelData=DataFrame(model_name))
            #out = predict(out, newx=x, newy=y)
            elementMetadata(out)$.response <- y
            return(out)
          })

setMethod("predict", "MiCNN_classification",
          function(object, newx, newy = NULL)
          {
            if ( !is(newx, "SparseImagingExperiment") )
              .stop("'newx' must inherit from 'SparseImagingExperiment'")
            .checkForIncompleteProcessing(newx)
            
            test = data.frame(label= newy, sample_id = newx$sample, x = coord(newx)$x, y = coord(newx)$y)
            spec = as.data.frame(t(as.matrix(spectra(newx))))
            colnames(spec) <- paste0('mz',mz(newx))
            test = cbind(test, spec) 
            test <<- test
            if ( !missing(newy) )
              newy <- as.factor(newy)
            
            model_name <- modelData(object)$model_name
            
            .message(paste0("predicting using", model_name))
            
            results <- list()
            results[[1]] = .CNN_predict(test, model_name)
            out <- .MiCNN_classification(
              imageData=.SimpleImageArrayList_nn(),
              featureData=featureData(newx),
              elementMetadata=pixelData(newx),
              metadata=metadata(object),
              resultData=as(results, "List"),
              modelData=modelData(object))
            
            if ( !missing(newy) )
              elementMetadata(out)$.response <- newy
            return(out)
          })


.checkForIncompleteProcessing <- function(object, message.only = FALSE) {
  anyPending <- any(mcols(processingData(object))$pending)
  if ( anyPending && !.Cardinal$processing ) {
    msg <- paste0("object has incomplete processing steps; ",
                  "run process() on it to apply them")
    if ( message.only ) {
      .message("Note: ", msg)
    } else {
      .stop(msg)
    }
  }
}


.CNN_fit <- function(data, model_name, batch_size = 32, 
                     optim = 'Adam', lr = 0.00001, num_epochs =10)
{
  obj_name = deparse(substitute(data))
  py_string = paste0("from classification import *\n",
                     "model_opts = model_opt()\n", 
                     model_name, " = classify(model_opts, data = r.", obj_name, ", batch_size = ", batch_size, 
                     " ,optim = '", optim, "' ,lr = ", lr, " ,num_epochs = ", num_epochs, ")\n",
                     model_name, ".train()\n",
                     "pred_label, pred_prob = ", model_name, ".val()\n",
                     sep = '')
  #py_string =('z = z.cpu().detach().numpy()')
  py_run_string(py_string)
  #py_run_string('z = np.array([x.numpy() for x in z])')
  #py_run_string('x = np.array([xp.numpy() for xp in x])')
  #py_run_string('y = np.array([yp.numpy() for yp in y])')
  return(list(class = as.factor(unlist(py$pred_label)), probability = unlist(py$pred_prob)))
}

.miCNN_fit <- function(data, model_name, batch_size = 32, 
                       optim = 'Adam', lr = 0.00001, num_epochs =10)
{
  obj_name = deparse(substitute(data))
  py_string = paste0("from classification import *\n",
                     "model_opts = model_opt()\n", 
                     model_name, " = classify(model_opts, data = r.", obj_name, ", batch_size = ", batch_size, 
                     " ,optim = '", optim, "' ,lr = ", lr, " ,num_epochs = ", num_epochs, ")\n",
                     model_name, ".mitrain()\n",
                     "pred_label, pred_prob = ", model_name, ".val()\n",
                     sep = '')
  #py_string =('z = z.cpu().detach().numpy()')
  py_run_string(py_string)
  #py_run_string('z = np.array([x.numpy() for x in z])')
  #py_run_string('x = np.array([xp.numpy() for xp in x])')
  #py_run_string('y = np.array([yp.numpy() for yp in y])')
  return(list(class = as.factor(unlist(py$pred_label)), probability = unlist(py$pred_prob)))
}

.CNN_predict <- function(data, model_name)
{
  obj_name = deparse(substitute(data))
  py_string = paste0("from classification import *\n",
                     "new_data = r.", obj_name, "\n",
                     "pred_label, pred_prob = ", model_name, ".val(new_data)\n",
                     sep = '')
  #py_string =('z = z.cpu().detach().numpy()')
  py_run_string(py_string)
  #py_run_string('z = np.array([x.numpy() for x in z])')
  #py_run_string('x = np.array([xp.numpy() for xp in x])')
  #py_run_string('y = np.array([yp.numpy() for yp in y])')
  return(list(class = as.factor(unlist(py$pred_label)), probability = unlist(py$pred_prob)))
}


