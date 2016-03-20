#include <RcppArmadilloExtensions/sample.h>
// [[Rcpp::depends(RcppArmadillo)]]
#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
CharacterVector markovchainSequenceRcpp(int n, S4 markovchain, CharacterVector t0,
                                       bool include_t0 = false) {
  
  // character vector to store the result
  CharacterVector chain = CharacterVector::create();
  
  // transition mastrix
  NumericMatrix transitionMatrix = markovchain.slot("transitionMatrix");
  
  // possible states
  CharacterVector states = markovchain.slot("states");
    
  // current state
  CharacterVector state = t0;
  
  for(int i = 0;i < n;i++) {
    
    // extracting row probabilties for the given state from transition matrix
    int row_no = 0;
    for(int j = 0;j < states.size();j++) {
      if(states[j] == state[0]) {
        row_no = j;
        break;
      }
    }
  
    NumericVector rowProbs = NumericVector::create();
    for(int j = 0; j < states.size(); j++) {
      rowProbs.push_back(transitionMatrix(row_no, j));
    }
    
    // calculate next state
    CharacterVector outstate = RcppArmadillo::sample(states, 1, false, rowProbs);
    chain.push_back(outstate[0]);  
    state = outstate;
    
  }
  
  if (include_t0)
    chain.push_front(t0[0]);
  
  return chain;
}

bool checkSequenceRcpp(List object) {
  bool out = true;
  int nob = object.size();
  
  // if there is only one markovchain object return true
  if (nob == 1)
    return(true);
  
  S4 ob0, ob1;
  
  for(int i = 1; i < nob;i++) {
    ob0 = S4(object[i-1]);
    ob1 = S4(object[i]);
    
    CharacterVector statesNm1 = ob0.slot("states"); 
    CharacterVector statesN = ob1.slot("states");
    
    CharacterVector intersection = intersect(statesNm1, statesN);
    if(not setequal(intersection, statesNm1)) {
      out = false;
      break;
    }
  }
  return(out);
}

// [[Rcpp::export]]
List markovchainListRcpp(int n, List object) {
  bool verify = checkSequenceRcpp(object);
  
  if (not verify) {
    warning("Warning: some states in the markovchain sequences are not contained in the following states!");
  }
    
  
  NumericVector iteration = NumericVector::create();
  CharacterVector values = CharacterVector::create();
  S4 ob(object[0]);
  
  CharacterVector sampledValues;
  IntegerVector outIter;
  
  for(int i = 0;i < n;i++) {
    sampledValues = markovchainSequenceRcpp(1, object[0], CharacterVector(ob.slot("states")));
    outIter = rep(i+1, sampledValues.size());
    
    if(object.size() > 1) {
      for(int j = 1;j < object.size();j++) {
        CharacterVector newVals = markovchainSequenceRcpp(1, object[j], sampledValues);
        outIter.push_back(i+1);
        sampledValues.push_back(newVals[0]);
      }
    }
    
    for(int k = 0;k < outIter.size();k++) {
      iteration.push_back(outIter[k]);
      values.push_back(sampledValues[k]);
    }
  }
  
  return(List::create(iteration, values));
}