#include <iostream>
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

vector<int> modelStack;     //registro de decisiones??
uint indexOfNextLitToPropagate;  
uint decisionLevel;  // counts how many decisions we have in the model

vector<vector<vector<int>* > > negative_clauses;
vector<vector<vector<int>* > > positive_clauses;



void readClauses( ){
  // Skip comments
  char c = cin.get();
  while (c == 'c') {
    while (c != '\n') c = cin.get();
    c = cin.get();
  }  
  // Read "cnf numVars numClauses"
  string aux;
  cin >> aux >> numVars >> numClauses;
  
  clauses.resize(numClauses);
  model.resize(numVars+1,UNDEF);
  positive_clauses.resize(numVars+1);
  negative_clauses.resize(numVars+1);  
  vsids.resize(numVars+1,0); //counter 0 for each literal
  
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
  //cout << "----" << endl;
  //cout << "****Start setLiteralToTrue (" << lit << ")****" <<endl;
  modelStack.push_back(lit);
  // for (uint m = 0; m < modelStack.size() ; m++) {
  //       cout << "modelStack " << m << ": " << modelStack[m] << endl;
  //   }
  if (lit > 0) model[lit] = TRUE;
  else model[-lit] = FALSE;
  // for (uint c = 1; c <= numVars; c++) {
  //       cout << "model " << c << " -> " << model[c] << endl;
  // }
  // cout << endl;
  // cout << "****End setLiteralToTrue****" << endl;
  // cout << "----" << endl;
}


bool propagate() {
    int lit_being_propagated = modelStack[indexOfNextLitToPropagate];

    uint var_being_propagated = abs(lit_being_propagated);
    vector<vector<int>* >& clauses_opposite = lit_being_propagated > 0 ?
                            negative_clauses[var_being_propagated] :
                            positive_clauses[var_being_propagated];

    for (uint i = 0; i < clauses_opposite.size(); ++i) {
        vector<int>& clause = *clauses_opposite[i];
        bool someLitTrue = false;
        uint numUndefs = 0;
        int lastLitUndef = 0;

        // Could stop when num_undefs >= 2, but doesn't pay off in clauses of size 3
        for (uint j = 0; not someLitTrue and j < clause.size(); ++j) {
            int lit = clause[j];
            int val = currentValueInModel(lit);
            //cout << "clause val: " << val << endl;

            if (val == TRUE) {
              //cout << "val == TRUE" << endl;
              someLitTrue = true;
            }
            else if (val == UNDEF){ 
              //cout << "val == UNDEF" << endl;
              ++numUndefs;
              lastLitUndef = clause[j];
            }
        }

        if (not someLitTrue and numUndefs < 2) {
            if (numUndefs == 1) {
              //cout << "setLiteralToTrue on numUNdefs 1" << endl;
              setLiteralToTrue(lastLitUndef);
            }
            else {
              ++vsids[var_being_propagated];
              return true;
            }
        }
    }

    return false;
}

bool propagateGivesConflict ( ) {          // MODIFICAR
  // cout << "----" << endl;
  //cout << "****Start propagateGivesConflict****" << endl;
  // cout << "indexOfNextLitToPropagate -> " << indexOfNextLitToPropagate << endl;
  while ( indexOfNextLitToPropagate < modelStack.size() ) {
    if (propagate()) return true;
    ++indexOfNextLitToPropagate;  
  }
  
  return false;
}


void backtrack(){
  //cout << endl << endl;
  //cout << "********************************  BACKTRACK   ********************************" << endl << endl;
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



// Heuristic for finding the next decision literal:    MODIFICAR 
int getNextDecisionLiteral(){
  int maxUndefIndex = 0;
  int max = 0;
  for (uint i = 1; i <= numVars; ++i) {// not so stupid heuristic:
    //if (model[i] == UNDEF) cout << "counterIndexValid -> " << i<<endl;
    if (model[i] == UNDEF and vsids[i] >= max) { 
      // int r1 = rand() % 2;
      // if (r1==1){
        max = vsids[i];
        maxUndefIndex = i;
      //}
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
  //cout << "misat start" << endl;
  readClauses(); // reads numVars, numClauses and clauses
  // for (uint c = 1; c <= numVars; c++) {
  //      cout << "counter " << c << " -> " << vsids[c] << endl;
  // }
  // cout << "---CLAUSES---" << endl;
  // 
  // for (uint i = 0;  i < clauses.size(); i++) {
  //      cout << "clause nº " << i << " -> ";
  //      for (uint j = 0; j < clauses[i].size(); ++j) {
  //          cout << clauses[i][j] << " ";
  //      }
  //      cout << endl;
  // }
  // 
  // cout << "---CLAUSES---" << endl;
  
  indexOfNextLitToPropagate = 0;  
  decisionLevel = 0;
  
  // Take care of initial unit clauses, if any
  for (uint i = 0; i < numClauses; ++i)
    if (clauses[i].size() == 1) {
      int lit = clauses[i][0];
      int val = currentValueInModel(lit);
      if (val == FALSE) {cout << "UNSATISFIABLE" << endl; return 10;}
      else if (val == UNDEF) {
        //cout << "setLiteralToTrue on for1" << endl;
        setLiteralToTrue(lit);
      }
    }
  
  // DPLL algorithm
  while (true) {
    //cout << "-------DPLL iter------" << endl;
    while ( propagateGivesConflict() ) {
      //cout << "-------DPLL iter------" << endl;
      if ( decisionLevel == 0) { cout << "UNSATISFIABLE" << endl; return 10; }
      backtrack();
    }
    // cout << "end propagateGivesConflict" << endl;
    // cout << "----" << endl;
    //--
    // for (uint c = 1; c <= numVars; c++) {
    //     cout << "model " << c << " -> " << model[c] << endl;
    //  }
    // 
    //cout << endl;
    // 
    // for (uint c = 1; c <= numVars; c++) {
    //     cout << "counter " << c << " -> " << vsids[c] << endl;
    // }
    
    // cout <<endl;
    
    //--
    
    int decisionLit = getNextDecisionLiteral();
    //cout << "next decisionLit:" << decisionLit << endl; 
    if (decisionLit == 0) { checkmodel(); cout << "SATISFIABLE" << endl; return 20; }
    // start new decision level:
    modelStack.push_back(0);  // push mark indicating new DL
    
    //--
    // cout << endl;
    // for (uint m = 0; m < modelStack.size() ; m++) {
    //     cout << "modelStack " << m << ": " << modelStack[m] << endl;
    // }
    //--
    
    ++indexOfNextLitToPropagate;
    // cout << endl;
    // cout << "indexNextLit: " << indexOfNextLitToPropagate << endl;
    
    ++decisionLevel;
    // cout << endl;
    // cout << "decisionLevel: " << decisionLevel << endl;
    
    setLiteralToTrue(decisionLit);    // now push decisionLit on top of the mark
  }
}  
