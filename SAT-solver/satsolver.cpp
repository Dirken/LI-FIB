/*
-----------------------------------------
|	Assignatura: LI Q2 2017				         |
|	Nom: Ricard Meyerhofer Parra		       |
|	Practica 1: Sat-solver.				         |
-----------------------------------------
*/

#include <iostream>
#include <stdlib.h>
#include <algorithm>
#include <vector>
using namespace std;

#define UNDEF -1
#define TRUE 1
#define FALSE 0

uint numVars;
uint numClauses;
vector<vector<int> > clauses;
vector<int> model;
vector<int> modelStack;
uint indexOfNextLitToPropagate;
uint decisionLevel;

vector<int> conflicts;

vector<vector<int> > posClauses;
vector<vector<int> > negClauses;

uint nPropagations;
uint nDecisions;

//Si s'ordenen les clausules al inici els temps van millor tot i que la meva logica diu que no hauria de ser
//aixi en general, ho entrego amb aixo doncs realment va mes rapid.
void order (int i) {
	for(int k = i; k > 0; --k) {
      if(clauses[k-1].size() > clauses[k].size()) swap(clauses[k-1], clauses[k]);
      else return;
    }
}

/*Al moment de llegir les clausules les dividim en clausules positives i clausules negatives i
pel valor absolut de cada literal, tenim un comptador que s'incrementa segons les vegades que apareix*/
void create_structures(){
	for (uint i = 0; i < numClauses; ++i) {
	    for(uint j = 0; j < clauses[i].size(); ++j) {
	      int lit = clauses[i][j];
	      if(lit < 0) { negClauses[-lit].push_back(i); ++conflicts[-lit];}
	      else { posClauses[lit].push_back(i); ++conflicts[lit];}
	    }
    }   
}

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
  posClauses.resize(numVars+1);
  negClauses.resize(numVars+1);
  conflicts.resize(numVars+1,0);

  for (uint i = 0; i < numClauses; ++i) {
    int lit;
    while (cin >> lit and lit != 0) {
      clauses[i].push_back(lit);
    }
    order(i);
  }
  create_structures();
}

int currentValueInModel(int lit){
  if (lit >= 0) return model[lit];
  else {
    if (model[-lit] == UNDEF) return UNDEF;
    else return 1 - model[-lit];
  }
}

void setLiteralToTrue(int lit){
  modelStack.push_back(lit);
  if (lit > 0) model[lit] = TRUE;
  else model[-lit] = FALSE;		
}

bool propagateGivesConflict () {
  while ( indexOfNextLitToPropagate < modelStack.size() ) {
    int lit = modelStack[indexOfNextLitToPropagate];
    ++indexOfNextLitToPropagate;
    int size;  bool litneg = false;

    //Si el literal es negatiu recorrerem les clausules negatives i en el cas de les positives el contrari
    //doncs son les que ens donen els problemes, les altres ja sabem que són satisfactibles
    if(lit < 0) { size = posClauses[-lit].size(); litneg = true;}
    else size = negClauses[lit].size();
  	for (uint i = 0; i < size ; ++i) {
  		++nPropagations;
  		bool someLitTrue = false;
  		int numUndefs = 0;
  		int lastLitUndef = 0;
  		int pos;
  		if (litneg) pos = posClauses[-lit][i];
  		else 		pos = negClauses[lit][i];

  		for (uint k = 0; not someLitTrue and k < clauses[pos].size(); ++k){
  		  int val = currentValueInModel(clauses[pos][k]);
  		  if (val == TRUE) someLitTrue = true;
  		  else if (val == UNDEF){ ++numUndefs; lastLitUndef = clauses[pos][k]; }
  		}

  		if (not someLitTrue and numUndefs == 0) {
  		  //si hi ha un literal a false llavors es aquest el que ens dona conflicte
  		  if (litneg) 			++conflicts[-lit];
  		  //del contrari hem de marcar els de tota la clausula de la posicio en concret que ens donen conflicte
  		  else for (uint m = 0; m < clauses[pos].size(); ++m)   ++conflicts[abs(clauses[pos][m])];
  		  return true; // conflict! all lits false
  		}
  		else if (not someLitTrue and numUndefs == 1) setLiteralToTrue(lastLitUndef);  
  	}
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
  ++nDecisions;
  int big = 0;
  bool divide = false;
  int i = 1;
  //primer element undef
  while (big == 0 && i < numVars) { 
  	if(model[i] == UNDEF) { 
  		big = i; 
  	}
  	++i; 
  }
   //actualitzem maxim
  for(int j = big; j < numVars; ++j) {
  	if (conflicts[j] > numClauses) divide = true;
    else if(model[j] == UNDEF and conflicts[j] > conflicts[big]) big = j;
  }
  //Dividim periodicament en el cas de que el nombre de conflictes sigui major al de clausules
  //he provat diversos valors i es pel que millor ha anat
  if (divide) for (int k = 1; k < numVars; ++k) conflicts[k] = conflicts[k] / 2;
  return big;
}


void checkmodel(){
  for (int i = 0; i < numClauses; ++i){
    bool someTrue = false;
    for (int j = 0; not someTrue and j < clauses[i].size(); ++j)
      someTrue = (currentValueInModel(clauses[i][j]) == TRUE);
    if (not someTrue) {
      cout << "Error in model, clause is not satisfied:";
      for (int j = 0; j < clauses[i].size(); ++j) cout << clauses[i][j] << " ";
      cout << endl;
      exit(1);
    }
  }  
}

int main(){ 
  time_t now;
  time_t now2;
  float seconds;

  readClauses(); // reads numVars, numClauses and clauses
  model.resize(numVars+1,UNDEF);
  indexOfNextLitToPropagate = 0;  
  decisionLevel = 0;

  nDecisions = 0;
  nPropagations = 0;
  time(&now);

  // Take care of initial unit clauses, if any
  for (uint i = 0; i < numClauses; ++i)
    if (clauses[i].size() == 1) {
      int lit = clauses[i][0];
      int val = currentValueInModel(lit);
      if (val == FALSE) {
        time(&now2);
        seconds = difftime(now2, now);
        cout << "s UNSATISFIABLE" << endl;
        cout << "c "<< seconds << " seconds total run time"  << endl;
        cout << "c "<< nPropagations << " propagations" << endl;
        cout << "c "<< (uint)nPropagations/(seconds*1000000) << " megaprops/second" << endl;
        cout << "c "<< nDecisions << " decisions"  << endl;
        return 10;
      }
      else if (val == UNDEF) setLiteralToTrue(lit);
    }
  
  // DPLL algorithm
  while (true) {
    while ( propagateGivesConflict() ) {
      if ( decisionLevel == 0) {
        time(&now2);
        seconds = difftime(now2, now);
        cout << "s UNSATISFIABLE" << endl;
        cout << "c "<< seconds << " seconds total run time"  << endl;
        cout << "c "<< nPropagations << " propagations" << endl;
        cout << "c "<< (uint)nPropagations/(seconds*1000000) << " megaprops/second" << endl;
        cout << "c "<< nDecisions << " decisions"  << endl;
        return 10;
      }
      backtrack();
    }
    int decisionLit = getNextDecisionLiteral();
    if (decisionLit == 0) {
      time(&now2);
      seconds = difftime(now2, now);
      checkmodel();
      cout << "s SATISFIABLE" << endl;
      cout << "c "<< seconds << " seconds total run time"  << endl;
      cout << "c "<< nPropagations << " propagations" << endl;
      cout << "c "<< (uint)nPropagations/(seconds*1000000) << " megaprops/second" << endl;
      cout << "c "<< nDecisions << " decisions"  << endl;
      return 20; 
    }
    // start new decision level:
    modelStack.push_back(0);  // push mark indicating new DL
    ++indexOfNextLitToPropagate;
    ++decisionLevel;
    setLiteralToTrue(decisionLit);    // now push decisionLit on top of the mark
  }
}
