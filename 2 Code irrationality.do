****************************************************
* 1) Import Excel (first row = variable names)
****************************************************
import excel "/Users/edoardomaira/Desktop/Behavioural/Progetto/Final dataset.xlsx", ///
    sheet("Sheet1") firstrow clear

****************************************************
* 2) Rename variables to clean names
****************************************************
rename Beliefindexcore   belief_index_core
rename Beliefindexext    belief_index_ext
rename RiskyshareAct     risky_share_act
rename Riskysharehyp     risky_share_hyp
rename Female            female
rename Educ              educ
rename Overconfidence    overconfidence
rename Experience        experience
rename Prime             prime
rename Financiallit      financiallit

****************************************************
* 3) Fix decimal commas (if any) and convert to numeric
****************************************************
destring belief_index_core belief_index_ext risky_share_act risky_share_hyp ///
         overconfidence experience, replace dpcomma

****************************************************
* 4) LONG model — Risky share (actual)
****************************************************
reg risky_share_act i.female##i.prime educ overconfidence experience financiallit ///
    if inlist(female,0,1) & inlist(prime,0,1)
	
* Breusch-Pagan / Cook-Weisberg test for heteroskedasticity
estat hettest, rhs

****************************************************
* 5) Regression — Risky share (hypothetical)
****************************************************
reg risky_share_hyp i.female##i.prime educ overconfidence experience financiallit ///
    if inlist(female,0,1) & inlist(prime,0,1)

* Breusch-Pagan / Cook-Weisberg test for heteroskedasticity
estat hettest, rhs
	
****************************************************
* 6) LONG model — Belief index (core)
****************************************************
reg belief_index_core i.female##i.prime educ overconfidence financiallit ///
    if inlist(female,0,1) & inlist(prime,0,1)

* Breusch-Pagan / Cook-Weisberg test for heteroskedasticity
estat hettest, rhs
	
****************************************************
* 7) Regression — Belief index (extended)
****************************************************
reg belief_index_ext i.female##i.prime educ overconfidence financiallit ///
    if inlist(female,0,1) & inlist(prime,0,1)
	
* Breusch-Pagan / Cook-Weisberg test for heteroskedasticity
estat hettest, rhs

