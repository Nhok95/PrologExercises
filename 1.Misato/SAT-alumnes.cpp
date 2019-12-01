varlit#include <iostream>
#include <stdlib.h>
#include <algorithm>
#include <vector>
#include <time.h>
using namespace std;

#define UNDEF -1
#define TRUE 1
#define FALSE 0

uint numVars;
uint numClauses;
vector<vector<int> > clauses;
vector<int> model;   // for each variable contain its value (UNDEF, TRUE, FALSE)
vector<int> vsids;   // for each variable (literal) contain a counter

vector<int> modelStack;    
uint indexOfNextLitToPropagate;  
uint decisionLevel;  // counts how many decisions we have in the model

vector<vector<vector<int>* > > negative_clauses;
vector<vector<vector<int>* > > positive_clauses;



void readClauses( ){
  char c = cin.get();
  while (c == 'c') {
    while (c != '\n') c = cin.get();
    c = cin.get();
  }  
  string aux;
  cin >> aux >> numVars >> numClauses;
  
  clauses.resize(numClauses);
  model.resize(numVars+1,UNDEF);
  positive_clauses.resize(numVars+1);
  negative_clauses.resize(numVars+1);  
  vsids.resize(numVars+1,0);
  
  // Read clauses
  for (uint i = 0; i < numClauses; ++i) {
    int lit;
    while (cin >> lit and lit != 0) {
      int var = abs(lit);
      vsids[var]++;
      if (lit > 0) positive_clauses[var].push_back(&clauses[i]);
      else negative_clauses[var].push_back(&clauses[i]);
      clauses[i].push_back(lit);
    }
  }    
}

int currentValueInModel(int lit){
  if (lit >= 0) {
      return model[lit];  //return TRUE or FALSE or UNDEF
  }
  else {
    if (model[-lit] == UNDEF) return UNDEF;  //UNDEF == UNDEF
    else return 1 - model[-lit];    // if TRUE return 0 --- if FALSE return 1
  }
}


inline void setLiteralToTrue(int lit){
  modelStack.push_back(lit);
  if (lit > 0) model[lit] = TRUE;
  else model[-lit] = FALSE;
}


bool propagate() {
    int lit = modelStack[indexOfNextLitToPropagate];
    uint var = abs(lit);
    vector<vector<int>* >& clauses_opposite = lit > 0 ?
                            negative_clauses[var] :
                            positive_clauses[var];

    for (uint i = 0; i < clauses_opposite.size(); ++i) {
        vector<int>& clause = *clauses_opposite[i];
        bool someLitTrue = false;
        uint numUndefs = 0;
        int lastLitUndef = 0;

        for (uint j = 0; not someLitTrue and j < clause.size(); ++j) {
            int l = clause[j];
            int val = currentValueInModel(l);
            if (val == TRUE) someLitTrue = true;
            else if (val == UNDEF){ 
              ++numUndefs;
              lastLitUndef = clause[j];
            }
        }

        if (not someLitTrue and numUndefs < 2) {
            ++vsids[var];
            return true;
        }
        else if (not someLitTrue and numUndefs == 1) setLiteralToTrue(lastLitUndef);
        }
    }

    return false;
}

bool propagateGivesConflict ( ) { 
  while ( indexOfNextLitToPropagate < modelStack.size() ) {
    if (propagate()) return true;
    ++indexOfNextLitToPropagate;  
  }
  return false;
}


void backtrack(){
  uint i = modelStack.size() -1;
  int lit = 0;
  while (modelStack[i] != 0){ // 0 is the DL mark
    lit = modelStack[i];
    model[abs(lit)] = UNDEF;
    modelStack.pop_back();
    --i;
  }
  // at this point, lit is the last decision
  modelStack.pop_back(); // remove the DL mark
  --decisionLevel;
  indexOfNextLitToPropagate = modelStack.size();
  setLiteralToTrue(-lit);  // reverse last decision
}

// Heuristic for finding the next decision literal:
int getNextDecisionLiteral(){
  int maxUndefIndex = 0;
  int max = 0;
  for (uint i = 1; i <= numVars; ++i) {}
    if (model[i] == UNDEF and vsids[i] >= max) { 
        max = vsids[i];
        maxUndefIndex = i;
    }   
  }
  if (max == 0) return 0; // reurns 0 when all literals are defined
  else return maxUndefIndex;// returns last max counter UNDEF var, positively
}

void checkmodel(){
  for (uint i = 0; i < numClauses; ++i){
    bool someTrue = false;
    for (uint j = 0; not someTrue and j < clauses[i].size(); ++j)
      someTrue = (currentValueInModel(clauses[i][j]) == TRUE);
    if (not someTrue) {
      cout << "Error in model, clause is not satisfied:";
      for (uint j = 0; j < clauses[i].size(); ++j) cout << clauses[i][j] << " ";
      cout << endl;
      exit(1);
    }
  }  
}

int main(){ 
  readClauses(); // reads  
  indexOfNextLitToPropagate = 0;  
  decisionLevel = 0;
  
  // Take care of initial unit clauses, if any
  for (uint i = 0; i < numClauses; ++i)
    if (clauses[i].size() == 1) {
      int lit = clauses[i][0];
      int val = currentValueInModel(lit);
      if (val == FALSE) {cout << "UNSATISFIABLE" << endl; return 10;}
      else if (val == UNDEF) {
        setLiteralToTrue(lit);
      }
    }
  
  // DPLL algorithm
  while (true) {
    while ( propagateGivesConflict() ) {
      if ( decisionLevel == 0) { cout << "UNSATISFIABLE" << endl; return 10; }
      backtrack();
    }
    
    int decisionLit = getNextDecisionLiteral();
    if (decisionLit == 0) { checkmodel(); cout << "SATISFIABLE" << endl; return 20; }
    modelStack.push_back(0);  // push mark indicating new DL
    
    ++indexOfNextLitToPropagate;
    ++decisionLevel;
    
    setLiteralToTrue(decisionLit);    // now push decisionLit on top of the mark
  }
}  
