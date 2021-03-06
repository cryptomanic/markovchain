# check if the sequence holds the Markov property
verifyMarkovProperty<-function(sequence,...) {
  n<-length(sequence)
  u<-unique(sequence)
  stateNames<-u
  nelements<-length(stateNames)
  mat<-zeros(nrow=nelements, ncol=3)
  # SSO: state sequence occurrences
  # TSO: two state occurences
  dimnames(mat)<-list(stateNames, c("SSO", "TSO", "TSO-SSO"))
  SSO<-numeric()
  for(i in 1:nelements) {
    sname<-stateNames[i]
    SSO[sname]<-0
  }
  TSO<-SSO
  out<-list()
  for(present in stateNames) {
    for(future in stateNames) {
      # print(paste0(present,'->',future))
      for(i in 1:nelements) TSO[i]<-SSO[i]<-0
      for(i in 1:(n-1))
      {
        past<-sequence[i]
        if(sequence[i+1] == present) {
          TSO[past] <- TSO[past] + 1
          if((i < n - 1) && (sequence[i+2] == future)) {
            for(s in stateNames) {
              if(s == past) {
                SSO[s] <- SSO[s] + 1
              }
            }
          }
        }
      }
      for(i in 1:(length(SSO))) {
        mat[i,1]<-SSO[i]
        mat[i,2]<-TSO[i]
        mat[i,3]<-TSO[i] - SSO[i]
      }
      # chi-squared test
      table<-as.data.frame(mat[,c(1,3)])
      res<-chisq.test(table,...)
      res<-c(res)
      table<-as.data.frame(mat[,c(1,2)])
      res[["table"]]<-table
      out[[paste0(present,future)]]<-res
    }
  }
  return(out)
}

#the out should be a function with following slots:
#stateTransitionSequenceTable (as in page 25)
#statistic: the chi - square statistic 
#p-value: the p-value of the statistic with appropriate degrees of freedom (see p 25-27 on the calculation)

#also the following should run: data(blanden); myMc<-as(blanden,"markovchain");sequenza<-rmarkovchain(n = 100,myMc)
#verifyMarkovProperty(sequenza)
#http://stats.stackexchange.com/questions/37386/check-memoryless-property-of-a-markov-chain 

# check if sequence is of first order or of second order
assessOrder<-function(sequence) {
  n<-length(sequence)
  states<-unique(sequence)
  nelements<-length(states)
  TStat<-0
  for(present in states) {
    mat<-zeros(nelements)
    dimnames(mat)<-list(states, states)
    for(i in 1:(n - 2)) {
      if(present == sequence[i + 1]) {
        past<-sequence[i]
        future<-sequence[i+2]
        mat[past, future] <- mat[past, future] + 1 
      }
    }
    # chi-squared test
    res<-chisq.test(mat)
    TStat<-TStat+res$statistic
    # out[[present]]<-res
  }
  k<-nelements
  df<-k*(k-1)^2
  pvalue<-1-pchisq(q = TStat, df)
  #returning the output
  cat("The assessOrder test statistic is: ",TStat, " the Chi-Square d.f. are: ",df," the p-value is: ",pvalue,"\n")
  out<-list(statistic=TStat[[1]], p.value=pvalue[[1]])
  return(out)
}

# check if sequence is stationary
assessStationarity<-function(sequence, nblocks) {
  n<-length(sequence)
  blocksize<-n/nblocks
  states<-unique(sequence)
  nstates<-length(states)
  TStat<-0
  for(i in states) {
    mat<-zeros(nblocks,nstates)
    dimnames(mat)<-list(1:nblocks,states)
    for(j in 1:(n-1)) {
      if(sequence[j] == i) {
        b<- ceiling(j / blocksize)
        future<-sequence[j+1]
        mat[b,future]<-mat[b,future]+1
      }
    }
    rowsums<-rowSums(mat)
    indices<-which(rowsums == 0)
    mat<-mat/rowsums
    for(k in indices) mat[k,]<-1/nstates
    # chi-squared test
    res<-chisq.test(mat)
    TStat<-TStat+res$statistic
  }
  k<-nstates
  df<-k*(nblocks - 1) * (k-1)
  pvalue<-1-pchisq(TStat, df)
  #returning the output
  cat("The assessStationarity test statistic is: ",TStat, " the Chi-Square d.f. are: ",df," the p-value is: ",pvalue,"\n")
  out<-list(statistic=TStat[[1]], p.value=pvalue[[1]])
  return(out)
}

# sequence to transition frequencey matrix
.seq2mat<-function(sequence) {
  n<-length(sequence)
  states<-unique(sequence)
  nstates<-length(states)
  mat<-zeros(nstates)
  dimnames(mat)<-list(states, states)
  for(i in 1:(n-1)) {
    from<-sequence[i]
    to<-sequence[i+1]
    mat[from,to]<-mat[from,to]+1
  }
  return (mat)
}

# divergence test for the hypothesized one and an empirical transition matrix from sequence
divergenceTest<-function(sequence, hypothetic) {
  n<-length(sequence)
  empirical<-.seq2mat(sequence)
  M<-nrow(empirical)
  v<-numeric()
  out<-2*n/.phi2(1)
  sum<-0
  c<-0
  for(i in 1:M) {
    sum2<-0
    sum3<-0
    for(j in 1:M) {
      if(hypothetic[i,j]>0) c<-c+1
      sum2<-sum2+hypothetic[i,j]*.phi(empirical[i,j]/hypothetic[i,j])
      if((j > 1) && (sequence[j-1] == i))
        sum3<-sum3 + 1
    }
    v[i]<-sum3
    sum<-v[i]/n*sum2
  }
  TStat<-out*sum
  pvalue<-1-pchisq(TStat,c-M)
  cat("The Divergence test statistic is: ",TStat, " the Chi-Square d.f. are: ",c-M," the p-value is: ",pvalue,"\n")
  out<-list(statistic=TStat, p.value=pvalue)
  return (out)
}

# phi function for divergence test
.phi<-function(x) {
  out<-x*log(x)-x+1
  return(out)
}

# another phi function for divergence test
.phi2<-function(x) {
  out<-1/x
  return(out)
}
