cd "C:\Users\530443\Google Drive\Research Projects\Discounting & inequality aversion\Data&Graphs\"
//discocunt rates are in % i.e. 2 instead of 0.02.
//dummies for eta are 2 or 0 instead of 1/0 for intertemporal questions such that the coefficients can be read as eta
//dummies for eta without time (first question) are 1/0
clear all
set more off
//QUESTIONNAIRES SPRING
import excel ResultsSpring2017.xlsx, sheet("Feuil1") firstrow clear
destring  indifference, replace force 
//replace vote=lower(vote)
drop discount_rate_excel
save ResultsSpringClean, replace

//QUESTIONNAIRES SEPT 2017
forvalues i=3/10 {
import excel ResultsSept2017Group`i'.xlsx, firstrow clear 
qui tostring indifference_value_original vote sex test1 test2 comment, replace
qui destring climate_euros gov_environ_welfare confidence_gov confidence_parties confidence_NGO confidence_people concerned_env concerned_ineq concerned_planet concerned_poll_today saving_at_5perc age income  diploma , replace force
qui drop bad_understanding
save Group`i' , replace
}
//QUESTIONNAIRES SEPT2020
forvalues i=11/17 {
import excel ResultsSept2020Group`i'.xlsx, firstrow clear 
qui tostring indifference_value_original vote sex test1 test2 comment, replace
qui destring climate_euros gov_environ_welfare confidence_gov confidence_parties confidence_NGO confidence_people concerned_env concerned_ineq concerned_planet concerned_poll_today saving_at_5perc age income  diploma , replace force
qui drop bad_understanding
save Group`i' , replace
}

append using Group3 Group4 Group5 Group6 Group7 Group8 Group9 Group10 Group11 Group12 Group13 Group14 Group15 Group16, force //Group 17 still in memory

//Make varables
drop Z // I don't know in which file Z was a column, but it has no values 
destring indifference_value_original, gen (indifference) force
local i=1
foreach xxx in forest air fertility {
gen `xxx'=ppt==`i' | ppt==`i'+3 | ppt==`i'+6 | ppt==`i'+9
local ++i
}
gen loss=mod(ppt,2)==0 //if ppt is an even number

gen twoRegions=ppt==1|ppt==3|ppt==5|ppt==8|ppt==10|ppt==12
replace twoRegions=1 if question==1


gen     decisionPast=question==5|question==6|question==7 if   ppt<7
replace decisionPast=question==3|question==4|question==5 if   ppt>6
replace decisionPast=0 if group==17 //group 17 has different questions order instead of decisionpast.
gen different_order=1 if group==17& (question==2|question==3|question==4|(question==1&ppt==5))
label define decision 0 "Decision Now" 1 "Decision Past", replace
label values decisionPast decision

gen instantaneous=question==1
label variable instantaneous "Eta instantaneous"
gen brownFuture=0
bysort id ppt : replace brownFuture= -2 if (question ==4 | question==7) & (inrange(ppt,1,3) | inrange(ppt,7,9)) 
bysort id ppt : replace brownFuture= -2 if (question ==3 | question==6) & (inrange(ppt,4,6) | inrange(ppt,10,12)) 
label variable brownFuture "Eta brown future"
gen greenFuture=0
bysort id ppt : replace greenFuture= 2 if (question ==3 | question==6) & (inrange(ppt,1,3) | inrange(ppt,7,9)) 
bysort id ppt : replace greenFuture= 2 if (question ==4 | question==7) & (inrange(ppt,4,6) | inrange(ppt,10,12)) 
label variable greenFuture "Eta green future"
//fill in missings with preceding values for other questions
sort id ppt question
foreach xxx in  climate_euros gov_environ_welfare confidence_gov confidence_parties confidence_NGO confidence_people concerned_env concerned_ineq concerned_planet concerned_poll_today saving_at_5perc age income diploma {
//by id : egen `xxx'=min(`xxx') 
by id : replace `xxx'=`xxx'[_n-1] if missing(`xxx')  
}
foreach xxx in vote sex test1 test2{
by id : replace `xxx'=`xxx'[_n-1] if missing(`xxx')  | `xxx'=="."
replace `xxx'=lower(`xxx')
}

append using ResultsSpringClean, force

gen oneRegion=1-twoRegions
gen gain=1-loss
gen timeonly=instantaneous==0 & brownFuture==0 & greenFuture==0
label variable timeonly "time without inequality"
gen intertemporal= 1- instantaneous
label variable intertemporal "Delta"  //old label "Delta without inequality"
gen questiontype=1 if instantaneous==1 //instantaneous question
replace questiontype=2 if timeonly==1 //only time
replace questiontype=3 if greenFuture==2 
replace questiontype=4 if brownFuture==-2
label define type 1 "Inequality only" 2 "Time only" 3 "Green future" 4 "Brown future", replace
label values questiontype type
gen irrational_answer=1 if indifference==. & indifference_value_original!="" 
bys id: egen irrational=count(irrational_answer) 


gen preL=900 
replace preL=600 if greenFuture==2
gen preR=600
replace preR=900 if timeonly==1 | greenFuture==2
gen changeL=10 
replace changeL=5 if greenFuture==2 
gen changeR=.
gen choice=.

gen discount_rate = ln(indifference/changeL)*100/20
replace discount_rate= ln(indifference/changeL)/ln(2/3) if questiontype==1

//redifine variable vote
replace vote="9" if vote=="blanco" | vote=="pas voté" | vote=="destesche"
destring vote, replace
replace vote=11 if vote==5
replace vote=12 if vote==4
replace vote=4 if vote==2
replace vote=5 if vote==1
replace vote=3 if vote==11 //change to 1 if PTB as a separate category
replace vote=2 if vote==12
label define parties  2 "Green" 3 "Social-Dem. & Far Left" 4 "Centre" 5 "Liberal-right" 6 "Défi" 7 "PP" 8 "FN" 9 "Blanco"  //1 "Far Left"
label values vote parties

//Go from indifference values to initial A/B choices
expand 10 if group!=17 //10 is maximum slides per price list
//group 1 and 2
foreach k in 1 2 {
global group =`k'
forvalues j =1/4 { //question types
forvalues i =1/10 { //different choices per question
matrix A=(0.001, 1, 3, 5, 7.5, 10,. , ., .,. \ 10, 12, 14, 16, 18, 20, ., ., ., . \ 5, 6.5, 8, 9.5, 11, 12.5, 15, 20,. ,. \ 0.001, 3, 5, 7, 10, 13, 16, 20, 25,. )
bysort id question :  replace changeR=A[`j',`i'] if group==$group & questiontype==`j' & _n==`i'
}
}
}
//Group 3, 4, 11 (I exclude 9 members of group 17, which use different order of questions)
foreach k in 3 4 11{  
global group =`k'
forvalues j =1/4 { //question types
forvalues i =1/10 { //different choices per question
matrix A=(0.001, 1, 3, 5, 7.5, 10, ., ., ., .\ 10, 12, 14, 16, 18, 20, 25, ., .,. \ 5, 6.5, 8, 9.5, 11, 12.5, 15, 20, 25, .\0.001, 3, 5, 7, 10, 13, 16, 20, 25,. ) //group 11 goes to 35 for green future
bysort id ppt question :  replace changeR=A[`j',`i'] if group ==$group & questiontype==`j' & _n==`i' & (ppt==3 |ppt==5)
matrix A=(0.001, 1, 3, 5, 7.5, 10, ., ., ., .\ 8,10, 12, 14, 16, 18, 20,  ., .,. \ 5, 6.5, 8, 9.5, 11, 12.5, 15, 20, 25, .\0.001, 3, 5, 7, 10, 13, 16, 20, 25,. )
bysort id ppt question :  replace changeR=A[`j',`i'] if group ==$group & questiontype==`j' & _n==`i' & (ppt==4 |ppt==6)
}
}
}
//Group 5 and 6; 12 & 13
foreach k in 5 6 12 13{  
global group =`k'
forvalues j =1/4 { //question types
forvalues i =1/10 { //different choices per question
matrix A=(0.001, 1, 3, 5, 7.5, 10, ., ., ., .\ 8, 10, 12, 14, 16, 18, 20, 25, ., . \ 5, 6.5, 8, 9.5, 11, 13, 15, 20, 25, .\0.001, 3, 5, 7, 10, 13, 16, 20, 25,. )
bysort id ppt question :  replace changeR=A[`j',`i'] if group ==$group & questiontype==`j' & _n==`i'
}
}
}
//Group 7 to 10 ; 14 to 16
foreach k in 7 8 9 10 14 16 {  
global group =`k'
forvalues j =1/4 { //question types
forvalues i =1/10 { //different choices per question
matrix A=(0.001, 1, 3, 5, 7.5, 10, ., ., ., .\8, 10, 12, 14, 16, 18, 20, 25, ., . \ 5, 6.5, 8, 9.5, 11, 13, 15, 20, 25, 35\0.001, 3, 5, 7, 10, 13, 16, 20, 25,. )
bysort id ppt question :  replace changeR=A[`j',`i'] if group ==$group & questiontype==`j' & _n==`i'
}
}
}
//Group 9 & 14 Qustions on air (uniquely defined by ppt==2
forvalues i =1/7 { //different choices per question
matrix A=(10, 12, 14, 16, 18, 20, 25)
bysort id question : replace changeR=A[1,`i'] if ppt==2 & questiontype==2 & _n==`i'
bysort id ppt question : drop if ppt==2 & questiontype==2 & _n==8
}
replace indifference= -indifference if loss==1
replace changeL= -changeL           if loss==1
replace changeR=-changeR 			if loss==1
replace choice= changeR>indifference if  indifference!=.
drop if changeR==. & group!=17

gen repl2020=group>10
sort id ppt question changeR
save ResultsClean2020, replace

//MAKE RESULTSCLEAN2020COMPACT FOR OLS AND NL REGRESSIONS
duplicates drop id ppt question, force // group 1&2 has missing ppt, but unique question numbers
gen ln_discount_rate=ln(discount_rate) if brownFuture==0 // log is impossible for negative discount rates
gen discount_factor=exp(-discount_rate/100)
gen discount_factor_20y=exp(-discount_rate*20/100)

//MAKE INTERACTION VARIABLES
gen eta_inst=instantaneous
gen eta_brownF=brownFuture
gen eta_greenF=greenFuture
gen eta_air=(instantaneous+brownFuture+greenFuture)*air
gen eta_fertility=(instantaneous+brownFuture+greenFuture)*fertility
gen eta_gain=(instantaneous+brownFuture+greenFuture)*gain
gen eta_decisionPast=(instantaneous+brownFuture+greenFuture)*decisionPast
gen eta_F_twoRegions=(brownFuture+greenFuture)*twoRegions
gen eta_F_twoRegions_decisionPast=eta_F_twoRegions*decisionPast
gen eta_brownF_twoRegions=brownFuture*twoRegions
gen eta_greenF_twoRegions=greenFuture*twoRegions
gen eta_brownF_decisionPast=brownFuture*decisionPast 
gen eta_greenF_decisionPast=greenFuture*decisionPast 
gen eta_brownF_twoRegions_decPast=brownFuture*twoRegions*decisionPast
gen eta_greenF_twoRegions_decPast=greenFuture*twoRegions*decisionPast
gen eta_inst_gain = instantaneous * gain
gen eta_brownF_gain = brownFuture * gain
gen eta_greenF_gain = greenFuture * gain
gen eta_inst_air = instantaneous * air
gen eta_brownF_air = brownFuture * air
gen eta_greenF_air = greenFuture * air
gen eta_inst_fertility = instantaneous * fertility
gen eta_brownF_fertility = brownFuture * fertility
gen eta_greenF_fertility = greenFuture * fertility
gen eta_inst_forest = instantaneous * forest
gen eta_brownF_forest = brownFuture * forest
gen eta_greenF_forest = greenFuture * forest
gen delta=intertemporal
gen delta_forest=intertemporal*forest
gen delta_air=intertemporal*air
gen delta_fertility=intertemporal*fertility
gen delta_gain=intertemporal*gain
gen delta_twoRegions=intertemporal*twoRegions
gen delta_decisionPast=intertemporal*decisionPast
gen delta_twoRegions_decisionPast=twoRegions*decisionPast
bysort id (ppt question): gen n=_n 
label variable n "# of question"
save ResultsClean2020compact, replace


