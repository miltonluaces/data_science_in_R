# CLASS NN : NEURAL NETWORKS ================================================================================================================================================

library(R6)
library(MiscFunctions)


NN = R6Class(

  classname = 'NN', 
  
  public = list(
    
    # Public Fields ----
    
    connHidden1=NULL,
    connHidden2=NULL,
    connOutput=NULL,
    nInput=0,
    nHidden1=0,
    nHidden2=0,
    nOutput=0,
    n=0.7,
    m=0.5,
    T=.Machine$integer.max,
    actFun='logsig', 
    error=.Machine$integer.max,
 
    # Public Methods ----      
    
    initialize = function(nInput, nHidden1, nHidden2=0, nOutput, connHidden1, connHidden2=NULL, connOutput, n, m,  T=.Machine$integer.max, actFun) {
      if(length(connHidden1) != nHidden1) { print('Error: connHidden1 must have nHidden1 lists'); return() }
      if(length(connHidden2) != nHidden2) { print('Error: connHidden2 must have nHidden2 lists'); return() }
      if(length(connOutput) != nOutput) { print('Error: connOutput must have nOutput lists'); return() }
      self$nInput = nInput
      self$nHidden1 = nHidden1
      self$nHidden2 = nHidden2
      self$nOutput = nOutput
      self$connHidden1 = connHidden1
      self$connHidden2 = connHidden2
      self$connOutput = connOutput
      self$n = n
      self$m = m
      self$T=T
      self$actFun = actFun
      private$lay.hidden1 = private$CreateLayer(nHidden1, nInput, connHidden1)
      nHidden = nHidden1
      if(self$nHidden2 > 0) { private$lay.hidden2 = private$CreateLayer(nHidden2, nHidden1, connHidden2);  nHidden = nHidden2  }
      private$lay.output = private$CreateLayer(nOutput, nHidden, connOutput)
    },
    
    Process = function(input) {
      if(self$nHidden2 > 0) private$Process2Hidd(input) else private$Process1Hidd(input)
    },
    
    Train = function(input, desOutput, epochs=1) {
      if(self$nHidden2 > 0) private$Train2Hidd(input, desOutput, epochs=1) else private$Train1Hidd(input, desOutput, epochs=1)
    },
    
    GetOutput = function() {
      private$LayerAxon(private$lay.output)
    },
    
    CheckError = function(nor, data, desOut, roundDig=2) {
      e=0
      cat(paste('Exp', 'Obt',sep='\t')); cat('\n')
      for(i in 1:length(data)) {  
        y = nor$UnNorm(self$Process(data[[i]])); 
        e = e + abs(y-desOut[i])
        cat(desOut[i], round(y,roundDig), sep='\t'); cat('\n')
      }
      cat('\nError', round(e, roundDig), paste(round((e/sum(desOut))*100, roundDig), '%', sep=''), sep='\t'); cat('\n')
      if(e > self$error) { cat('Stop Trainning!. Error = ', e, ' Prev Error = ', self$error); cat('\n') }
      self$error=e
    },
    
    Summary = function(nn) {
      print(paste('STRUCTURE : ',self$nInput,'-',self$nHidden, '-', self$nOutput, sep=''), quote=FALSE)
      print('', quote=FALSE)
      print('', quote=FALSE); print('Input Layer:', quote=FALSE)
      print(private$lay.input, quote=FALSE)
      print('', quote=FALSE); print('Hidden Layer:', quote=FALSE)
      print(private$lay.hidden, quote=FALSE)
      print('', quote=FALSE); print('Output Layer:', quote=FALSE) 
      print(private$lay.output, quote=FALSE)
      print('', quote=FALSE)
      print('Connections Hidden Layer:', quote=FALSE)
      print(self$connHidden, quote=FALSE)
      print('Connections Output Layer:', quote=FALSE)
      print(self$connOutput, quote=FALSE)
    }
  ),
  
  private = list(

    # Private Fields ----

    lay.input=NULL, 
    lay.hidden1=NULL, 
    lay.hidden2=NULL,
    lay.output=NULL,
    norm.xMin=0, 
    norm.xMax=0,
    norm.yMin=0,
    norm.yMax=0,
    
    # Private Methods ----
   
    Process1Hidd = function(input) {
      for(i in 1:self$nHidden1) { 
        connsFromInput = self$connHidden1[[i]]
        for(j in 1:length(connsFromInput)) { private$lay.hidden1[[i]][connsFromInput[j],'x'] = input[connsFromInput[j]] }
      }
      yHidden1 = private$LayerAxon(private$lay.hidden1)
      
      for(i in 1:self$nOutput) {
        connsFromHidden1 = self$connOutput[[i]]
        for(j in 1:length(connsFromHidden1)) { private$lay.output[[i]][connsFromHidden1[j],'x'] = yHidden1[j] }
      } 
      yOutput = private$LayerAxon(private$lay.output)
      yOutput
    },
    
    Train1Hidd = function(input, desOutput, epochs=1) {
      n0 = self$n
      for(t in 1:epochs) {
        if(t > self$T) self$n = n0 * (self$T/t); #print(paste('n = ', private$nn$n))
        private$Process1Hidd(input)
        for(i in 1:self$nOutput) { 
          deltaOutput = private$GetDeltaOutput(private$lay.output[[i]], desOutput)
          newWeights = private$lay.output[[i]][,'w'] + self$n * deltaOutput * private$lay.output[[i]][,'x'] + self$m * (private$lay.output[[i]][,'w'] - private$lay.output[[i]][,'v'])  #nðx + momentum
          private$lay.output[[i]][,'v'] = private$lay.output[[i]][,'w']
          private$lay.output[[i]][,'w'] = newWeights
        }
        
        for(i in 1:self$nHidden1) { 
          deltaHidden1 = private$GetDeltaHidden(private$lay.hidden1[[i]], deltaOutput)
          newWeights = private$lay.hidden1[[i]][,'w'] + self$n * deltaHidden1 * private$lay.hidden1[[i]][,'x'] + self$m * (private$lay.hidden1[[i]][,'w'] - private$lay.hidden1[[i]][,'v'])  #nðx + momentum
          private$lay.hidden1[[i]][,'v'] = private$lay.hidden1[[i]][,'w']
          private$lay.hidden1[[i]][,'w'] =  newWeights
        }
      }
    },
    
    Process2Hidd = function(input) {
      for(i in 1:self$nHidden1) { 
        connsFromInput = self$connHidden1[[i]]
        for(j in 1:length(connsFromInput)) { private$lay.hidden1[[i]][connsFromInput[j],'x'] = input[connsFromInput[j]] }
      }
      yHidden1 = private$LayerAxon(private$lay.hidden1)
      
      for(i in 1:self$nHidden2) {
        connsFromHidden1 = self$connHidden2[[i]]
        for(j in 1:length(connsFromHidden1)) { private$lay.hidden2[[i]][connsFromHidden1[j],'x'] = yHidden1[j] }
      } 
      yHidden2 = private$LayerAxon(private$lay.hidden2)
      
      for(i in 1:self$nOutput) {
        connsFromHidden2 = self$connOutput[[i]]
        for(j in 1:length(connsFromHidden2)) { private$lay.output[[i]][connsFromHidden2[j],'x'] = yHidden2[j] }
      } 
      yOutput = private$LayerAxon(private$lay.output)
      yOutput
    },
    
    Train2Hidd = function(input, desOutput, epochs=1) {
      n0 = self$n
      for(t in 1:epochs) {
        if(t > self$T) self$n = n0 * (self$T/t); #print(paste('n = ', private$nn$n))
        private$Process2Hidd(input)
        for(i in 1:self$nOutput) { 
          deltaOutput = private$GetDeltaOutput(private$lay.output[[i]], desOutput)
          newWeights = private$lay.output[[i]][,'w'] + self$n * deltaOutput * private$lay.output[[i]][,'x'] + self$m * (private$lay.output[[i]][,'w'] - private$lay.output[[i]][,'v'])  #nðx + momentum
          private$lay.output[[i]][,'v'] = private$lay.output[[i]][,'w']
          private$lay.output[[i]][,'w'] = newWeights
        }
       
        for(i in 1:self$nHidden2) { 
          deltaHidden2 = private$GetDeltaHidden(private$lay.hidden2[[i]], deltaOutput)
          newWeights = private$lay.hidden2[[i]][,'w'] + self$n * deltaHidden2 * private$lay.hidden2[[i]][,'x'] + self$m * (private$lay.hidden2[[i]][,'w'] - private$lay.hidden2[[i]][,'v'])  #nðx + momentum
          private$lay.hidden2[[i]][,'v'] = private$lay.hidden2[[i]][,'w']
          private$lay.hidden2[[i]][,'w'] =  newWeights
        }
        
        for(i in 1:self$nHidden1) { 
          deltaHidden1 = private$GetDeltaHidden(private$lay.hidden1[[i]], deltaOutput) #TODO: en lugar de deltaOutput algun deltaHidden2
          newWeights = private$lay.hidden1[[i]][,'w'] + self$n * deltaHidden1 * private$lay.hidden1[[i]][,'x'] + self$m * (private$lay.hidden1[[i]][,'w'] - private$lay.hidden1[[i]][,'v'])  #nðx + momentum
          private$lay.hidden1[[i]][,'v'] = private$lay.hidden1[[i]][,'w']
          private$lay.hidden1[[i]][,'w'] =  newWeights
        }
      }
    },
    
    CreateLayer = function(nLayer, nPreLayer, connLayer) {
      layer = vector('list', nLayer)
      for(i in 1:nLayer) { 
        layer[[i]] = private$CreateNeuron(nPreLayer, connLayer[[i]])  
      }
      layer
    },
    
    CreateNeuron = function(nPreLayer, conn) {
      neuron = matrix(nrow=nPreLayer+1, ncol=3)
      colnames(neuron) = c('x', 'w', 'v')
      for(i in 1:length(conn)) { neuron[conn[i],'w']=runif(1, 0, 1) }
      neuron[nPreLayer+1,'x'] = 1  #bias
      neuron[nPreLayer+1,'w'] = runif(1, 0, 1) 
      neuron[,'v'] = 0 
      neuron
    },
    
    Axon = function(neuron) {
      net = sum(neuron[,'x'] * neuron[,'w'])
      if(self$actFun=='logsig') { return (1 / (1 + exp(-net))) }
      if(self$actFun=='tansig') { return (tanh(net)) }
    },
    
    LayerAxon = function(layer) {
      output = numeric(length(layer))
      for(i in 1:length(layer)) { output[i] = private$Axon(layer[[i]]) }
      output
    },
    
    GetDeltaOutput = function(neuron, d) {
      y = private$Axon(neuron)
      delta = y * (1-y) * (d-y);
      delta
    },
    
    GetDeltaHidden = function(neuron, deltaOutput) { 
      y = private$Axon(neuron)
      delta = neuron[,'w'] * deltaOutput *  y * (1-y)
      delta
    }

  )
)  

