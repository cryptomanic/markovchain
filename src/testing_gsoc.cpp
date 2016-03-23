#include <RcppArmadilloExtensions/sample.h>
// [[Rcpp::depends(RcppArmadillo)]]
#include <Rcpp.h>

using namespace Rcpp;
using namespace RcppArmadillo;

// [[Rcpp::export(.markovchainSequenceRcpp)]]
CharacterVector markovchainSequenceRcpp(int n, S4 markovchain, CharacterVector t0,
                                       bool include_t0 = false) {
  
  // character vector to store the result
  CharacterVector chain(n);
  
  // transition mastrix
  NumericMatrix transitionMatrix = markovchain.slot("transitionMatrix");
  
  // possible states
  CharacterVector states = markovchain.slot("states");
    
  // current state
  CharacterVector state = t0;
  
  NumericVector rowProbs(states.size());
  CharacterVector outstate;
  
  
  
  for(int i = 0;i < n;i++) {
    
    // extracting row probabilties for the given state from transition matrix
    int row_no = 0;
    for(int j = 0;j < states.size();j++) {
      if(states[j] == state[0]) {
        row_no = j;
        break;
      }
    }
  
    for(int j = 0; j < states.size(); j++) {
      rowProbs[j] = transitionMatrix(row_no, j);
    }
    
    // calculate next state
    outstate = sample(states, 1, false, rowProbs);
    chain[i] = outstate[0];  
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
  CharacterVector statesNm1, statesN, intersection;
  
  for(int i = 1; i < nob;i++) {
    ob0 = S4(object[i-1]);
    ob1 = S4(object[i]);
    
    statesNm1 = ob0.slot("states"); 
    statesN = ob1.slot("states");
    
    intersection = intersect(statesNm1, statesN);
    if(not setequal(intersection, statesNm1)) {
      out = false;
      break;
    }
  }
  return(out);
}

// [[Rcpp::export(.markovchainListRcpp)]]
List markovchainListRcpp(int n, List object, bool include_t0 = false) {
  bool verify = checkSequenceRcpp(object);
  
  if (not verify) {
    warning("Warning: some states in the markovchain sequences are not contained in the following states!");
  }
    
  
  NumericVector iteration = NumericVector::create();
  CharacterVector values = CharacterVector::create();
  S4 ob(object[0]);
  
  CharacterVector sampledValues, newVals;
  IntegerVector outIter;
  
  for(int i = 0;i < n;i++) {
    sampledValues = markovchainSequenceRcpp(1, object[0], CharacterVector(ob.slot("states")), include_t0);
    outIter = rep(i+1, sampledValues.size());
    
    if(object.size() > 1) {
      for(int j = 1;j < object.size();j++) {
        newVals = markovchainSequenceRcpp(1, object[j], sampledValues, include_t0);
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
