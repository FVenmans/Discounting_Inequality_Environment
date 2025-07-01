//cd "C:\Users\530443\Google Drive\Research Projects\Discounting & inequality aversion\Data&Graphs\"
cd "C:\Users\frank\Google Drive\Research Projects\Discounting & inequality aversion\Data&Graphs\"

use ResultsClean2020compact, clear
set more off

//DESCRIPTIVES
tab indifference_value_original 
tab indifference questiontype
tab discount_rate questiontype
tab irrational
hist discount_rate, start(-10) fraction width(1) by(questiontype) 
graph export hist_discountrate.pdf, replace
hist discount_rate if questiontype==2, start(-1) fraction width(1) by(decisionPast)
graph export hist_discountrate_timeonly_byDecisonPast.pdf, replace
hist discount_factor     , start(0.9) fraction width(0.01) by(questiontype)
graph export hist_discountfactor.pdf, replace
hist discount_factor_20y , start(0)   fraction width(0.2)  by(questiontype)
graph export hist_discountfactor_20y.pdf, replace
twoway hist discount_rate, bfcolor(none) blcolor(navy) start(-10) fraction width(1) by(questiontype) || ///
hist discount_rate if irrational==0, bfcolor(none) blcolor(red)  start(-10) fraction width(1) barw(0.7) by(questiontype) legend(label(1 "Total sample") label(2 "Never irrational answer"))
graph export hist_discountrate_NoIrrationalRed.pdf, replace
twoway hist discount_rate if questiontype==2, bfcolor(none) blcolor(navy) start(-1) fraction width(1) by(decisionPast)  || ///
hist discount_rate if irrational==0 & questiontype==2, bfcolor(none) blcolor(red)  start(-1) fraction width(1) barw(0.7) by(decisionPast) legend(label(1 "Total sample") label(2 "Never irrational answer"))
graph export hist_discr_timeonly_byDecisionPast_NoIrrationalRed.pdf, replace
//Number of people beyond the allowed range
tab questiontype gain if indifference_value_original=="A"
tab questiontype gain if indifference_value_original=="B"
tab questiontype if indifference_value_original=="AB"|indifference_value_original=="ABA"|indifference_value_original=="BA"|indifference_value_original=="BAB"
tab indifference_value_original questiontype


//OLS
//Only delta, eta_inst, eta_brownF, eta_greenF
	reg discount_rate delta eta_inst eta_brownF eta_greenF, nocons vce(cluster id) 
		outreg2 using OLSTotalSample, word  auto(2) replace  //ctitle(OLS) addnote(...) keep(....) stats(...)
		outreg2 using OLSrepl, word auto(2) replace ctitle(Both Samples)
		outreg2 using OLS4firstquestionsTotalSample_loss, word  auto(2) replace  ctitle(Entire Sample)
	reg discount_rate delta eta_inst eta_brownF eta_greenF if repl2020==0, nocons vce(cluster id) 
		test (_b[eta_inst]=_b[eta_brownF]) (_b[eta_inst]=_b[eta_greenF]) 
		test (_b[eta_inst]=_b[eta_brownF]) 
		test (_b[eta_inst]=_b[eta_greenF]) 
		test (_b[eta_brownF]=_b[eta_greenF])
		outreg2 using OLS2017, word  auto(2) replace
		outreg2 using OLSrepl, word auto(2) ctitle(2017)
		outreg2 using OLS4firstquestions2017, word  auto(2) replace  ctitle(Entire Sample)
	reg discount_rate delta eta_inst eta_brownF eta_greenF if repl2020==1, nocons vce(cluster id) 
		outreg2 using OLSrepl, word auto(2) ctitle(2020)
	reg discount_rate delta eta_inst eta_brownF eta_greenF if n<5, nocons vce(cluster id)
		outreg2 using OLS4firstquestionsTotalSample_loss, word  auto(2) ctitle(Subsample) 
	reg discount_rate delta eta_inst eta_brownF eta_greenF if n<5 &repl2020==0, nocons vce(cluster id)
		outreg2 using OLS4firstquestions2017, word  auto(2) ctitle(Subsample) 
	reg discount_rate delta eta_inst eta_brownF eta_greenF if irrational==0 & repl2020==0, nocons vce(cluster id)
		outreg2 using OLS_OnlyRational2017, word  auto(2) replace 
//Add domains 
	reg discount_rate delta delta_air delta_fertility eta_inst eta_brownF eta_greenF eta_air eta_fertility, nocons  vce(cluster id)
		test (_b[eta_air]=0) (_b[eta_fertility]=0) //H2
		test (_b[delta_air]=0) (_b[delta_fertility]=0) //H3
		outreg2 using OLSTotalSample, word  auto(2)
		outreg2 using OLSrepl, word auto(2) ctitle(Both Samples)
		outreg2 using OLS4firstquestionsTotalSample_loss, word  auto(2)  ctitle(Entire Sample)
	reg discount_rate delta delta_air delta_fertility eta_inst eta_brownF eta_greenF eta_air eta_fertility if repl2020==0, nocons  vce(cluster id)
		test (_b[eta_inst]=_b[eta_brownF]) (_b[eta_inst]=_b[eta_greenF]) //H1
		test (_b[eta_inst]=_b[eta_brownF]) 
		test (_b[eta_inst]=_b[eta_greenF]) 
		test (_b[eta_brownF]=_b[eta_greenF])
		outreg2 using OLS2017, word  auto(2)
		outreg2 using OLSrepl, word auto(2) ctitle(2017)
		outreg2 using OLS4firstquestions2017, word  auto(2) ctitle(Entire Sample)
	reg discount_rate delta delta_air delta_fertility eta_inst eta_brownF eta_greenF eta_air eta_fertility if repl2020==1, nocons  vce(cluster id)
		outreg2 using OLSrepl, word auto(2)  ctitle(2020)
		test (_b[delta_air]=0) (_b[delta_fertility]=0)
	reg discount_rate delta delta_air delta_fertility eta_inst eta_brownF eta_greenF eta_air eta_fertility  if n<5, nocons  vce(cluster id)
		outreg2 using OLS4firstquestionsTotalSample_loss, word  auto(2) ctitle(Subsample)
	reg discount_rate delta delta_air delta_fertility eta_inst eta_brownF eta_greenF eta_air eta_fertility  if n<5&repl2020==0, nocons  vce(cluster id)
		outreg2 using OLS4firstquestions2017, word  auto(2) ctitle(Subsample)
	reg discount_rate delta delta_air delta_fertility eta_inst eta_brownF eta_greenF eta_air eta_fertility  if irrational==0 & repl2020==0, nocons  vce(cluster id)
		outreg2 using OLS_OnlyRational2017, word  auto(2) 
//Add framing effects without domains
	reg discount_rate delta  delta_gain delta_decisionPast delta_twoRegions  eta_inst eta_brownF eta_greenF  eta_gain eta_decisionPast eta_F_twoRegions, nocons  vce(cluster id)
		test (_b[delta_gain]=0) (_b[delta_decisionPast]=0) (_b[delta_twoRegions]=0) //H4a
		test (_b[eta_gain]=0) (_b[eta_decisionPast]=0) (_b[eta_F_twoRegions]=0) //H4b
		outreg2 using OLSTotalSample, word  auto(2)
		outreg2 using OLSrepl, word auto(2) ctitle(Both Samples)
	gen delta_loss=intertemporal*(1-gain)
	gen eta_loss=(instantaneous+brownFuture+greenFuture)*(1-gain)
	reg discount_rate delta  delta_loss delta_decisionPast delta_twoRegions  eta_inst eta_brownF eta_greenF  eta_loss eta_decisionPast eta_F_twoRegions, nocons  vce(cluster id)
		outreg2 using OLS4firstquestionsTotalSample_loss, word  auto(2)  ctitle(Entire Sample)
	reg discount_rate delta  delta_gain delta_decisionPast delta_twoRegions  eta_inst eta_brownF eta_greenF  eta_gain eta_decisionPast eta_F_twoRegions if repl2020==0, nocons  vce(cluster id)
		test (_b[delta_gain]=0) (_b[delta_decisionPast]=0) (_b[delta_twoRegions]=0) //H4a
		test (_b[eta_gain]=0) (_b[eta_decisionPast]=0) (_b[eta_F_twoRegions]=0) //H4b
		outreg2 using OLS2017, word  auto(2) 
		outreg2 using OLSrepl, word auto(2) ctitle(2017)
		outreg2 using OLS4firstquestions2017, word  auto(2) ctitle(Entire Sample)
	reg discount_rate delta  delta_gain delta_decisionPast delta_twoRegions  eta_inst eta_brownF eta_greenF  eta_gain eta_decisionPast eta_F_twoRegions if repl2020==1, nocons  vce(cluster id)
		outreg2 using OLSrepl, word auto(2)  ctitle(2020)
	reg discount_rate delta  delta_loss delta_decisionPast delta_twoRegions  eta_inst eta_brownF eta_greenF  eta_loss eta_decisionPast   if n<5, nocons  vce(cluster id) //problem: first questions are always gains.
		outreg2 using OLS4firstquestionsTotalSample_loss, word auto(2) ctitle(Subsample)
	reg discount_rate delta  delta_gain delta_decisionPast delta_twoRegions  eta_inst eta_brownF eta_greenF  eta_gain eta_decisionPast   if n<5& repl2020==0, nocons  vce(cluster id) //problem: first questions are always gains.
		outreg2 using OLS4firstquestions2017, word auto(2) ctitle(Subsample)
	reg discount_rate delta  delta_gain delta_decisionPast delta_twoRegions  eta_inst eta_brownF eta_greenF eta_gain eta_decisionPast eta_F_twoRegions  if irrational==0&repl2020==0, nocons  vce(cluster id)
		outreg2 using OLS_OnlyRational2017, word  auto(2) 
//add domains and framing
	reg discount_rate delta delta_air delta_fertility delta_gain delta_decisionPast delta_twoRegions  eta_inst eta_brownF eta_greenF eta_air eta_fertility eta_gain eta_decisionPast eta_F_twoRegions, nocons  vce(cluster id)
		outreg2 using OLSTotalSample, word  auto(2)
		outreg2 using OLSrepl, word auto(2) ctitle(Both Samples)
	reg discount_rate delta delta_air delta_fertility delta_loss delta_decisionPast delta_twoRegions  eta_inst eta_brownF eta_greenF eta_air eta_fertility eta_loss eta_decisionPast eta_F_twoRegions, nocons  vce(cluster id)
		outreg2 using OLS4firstquestionsTotalSample_loss, word  auto(2)  ctitle(Entire Sample)
	reg discount_rate delta delta_air delta_fertility delta_gain delta_decisionPast delta_twoRegions  eta_inst eta_brownF eta_greenF eta_air eta_fertility eta_gain eta_decisionPast eta_F_twoRegions if repl2020==0, nocons  vce(cluster id)
		outreg2 using OLS2017, word  auto(2) 
		outreg2 using OLSrepl, word auto(2) ctitle(2017)
		outreg2 using OLS4firstquestions2017, word  auto(2) ctitle(Entire Sample)
	reg discount_rate delta delta_air delta_fertility delta_gain delta_decisionPast delta_twoRegions  eta_inst eta_brownF eta_greenF eta_air eta_fertility eta_gain eta_decisionPast eta_F_twoRegions if repl2020==1, nocons  vce(cluster id)
		outreg2 using OLSrepl, word auto(2) ctitle(2020) sortvar(delta  delta_air  delta_fertility  delta_gain  delta_decisionPast  delta_twoRegions eta_inst   eta_brownF eta_greenF eta_air eta_fertility eta_gain  eta_decisionPast  eta_F_twoRegions )
	reg discount_rate delta delta_air delta_fertility delta_loss delta_decisionPast delta_twoRegions  eta_inst eta_brownF eta_greenF eta_air eta_fertility eta_loss eta_decisionPast if n<5, nocons  vce(cluster id)
		outreg2 using OLS4firstquestionsTotalSample_loss, word  auto(2) ctitle(Subsample) sortvar(delta delta_air delta_fertility delta_loss delta_decisionPast delta_twoRegions  eta_inst eta_brownF eta_greenF eta_air eta_fertility eta_loss eta_decisionPast)
	reg discount_rate delta delta_air delta_fertility delta_gain delta_decisionPast delta_twoRegions  eta_inst eta_brownF eta_greenF eta_air eta_fertility eta_gain eta_decisionPast if n<5 &repl2020==0, nocons  vce(cluster id)
		outreg2 using OLS4firstquestions2017, word  auto(2) ctitle(Subsample) sortvar(delta delta_air delta_fertility delta_gain delta_decisionPast delta_twoRegions  eta_inst eta_brownF eta_greenF eta_air eta_fertility eta_gain eta_decisionPast)
	reg discount_rate delta delta_air delta_fertility delta_gain delta_decisionPast delta_twoRegions  eta_inst eta_brownF eta_greenF eta_air eta_fertility 	eta_gain eta_decisionPast eta_F_twoRegions  if irrational==0&repl2020==0, nocons  vce(cluster id)
		outreg2 using OLS_OnlyRational2017, word  auto(2) 
//add domains and gain for each eta
	reg discount_rate delta delta_air delta_fertility delta_gain delta_decisionPast delta_twoRegions  eta_inst   eta_inst_air   eta_inst_fertility   eta_inst_gain ///
			eta_brownF eta_brownF_air eta_brownF_fertility eta_brownF_gain eta_greenF eta_greenF_air eta_greenF_fertility eta_greenF_gain eta_decisionPast eta_F_twoRegions, nocons  vce(cluster id)
		outreg2 using OLSTotalSample, word  auto(2) sortvar(delta      delta_air      delta_fertility      delta_gain      delta_decisionPast      delta_twoRegions eta_inst   eta_inst_air   eta_inst_fertility   eta_inst_gain ///
			eta_brownF eta_brownF_air eta_brownF_fertility eta_brownF_gain eta_greenF eta_greenF_air eta_greenF_fertility eta_greenF_gain  eta_air 	eta_fertility    eta_gain 	    eta_decisionPast     eta_F_twoRegions      )
	reg discount_rate delta delta_air delta_fertility delta_gain delta_decisionPast delta_twoRegions  eta_inst   eta_inst_air   eta_inst_fertility   eta_inst_gain ///
			eta_brownF eta_brownF_air eta_brownF_fertility eta_brownF_gain eta_greenF eta_greenF_air eta_greenF_fertility eta_greenF_gain eta_decisionPast eta_F_twoRegions if repl2020==0, nocons  vce(cluster id)
		outreg2 using OLS2017, word  auto(2) sortvar(delta      delta_air      delta_fertility      delta_gain      delta_decisionPast      delta_twoRegions eta_inst   eta_inst_air   eta_inst_fertility   eta_inst_gain ///
			eta_brownF eta_brownF_air eta_brownF_fertility eta_brownF_gain eta_greenF eta_greenF_air eta_greenF_fertility eta_greenF_gain  eta_air 	eta_fertility    eta_gain 	    eta_decisionPast     eta_F_twoRegions      )
	reg discount_rate delta delta_air delta_fertility delta_gain delta_decisionPast delta_twoRegions  eta_inst eta_brownF eta_greenF eta_inst_air eta_brownF_air eta_greenF_air ///
			eta_inst_fertility eta_brownF_fertility eta_greenF_fertility eta_inst_gain eta_brownF_gain eta_greenF_gain eta_decisionPast eta_F_twoRegions  if irrational==0&repl2020==0, nocons  vce(cluster id)
		outreg2 using OLS_OnlyRational2017, word  auto(2) sortvar( 	delta      delta_air      delta_fertility      delta_gain      delta_decisionPast      delta_twoRegions       ///
			eta_inst   eta_inst_air   eta_inst_fertility   eta_inst_gain eta_brownF eta_brownF_air eta_brownF_fertility eta_brownF_gain  eta_greenF eta_greenF_air eta_greenF_fertility eta_greenF_gain  eta_air  eta_fertility eta_gain    eta_decisionPast     eta_F_twoRegions) 
//Unreported: add domains and 3 framings for each eta
	reg discount_rate delta delta_air delta_fertility delta_gain delta_decisionPast delta_twoRegions  eta_inst eta_inst_air eta_inst_fertility eta_inst_gain eta_brownF eta_brownF_fertility eta_brownF_air eta_brownF_gain eta_brownF_decisionPast eta_brownF_twoRegions eta_brownF_twoRegions_decPast ///
	eta_greenF eta_greenF_air eta_greenF_fertility eta_greenF_gain eta_greenF_decisionPast eta_greenF_twoRegions eta_greenF_twoRegions_decPast if repl2020==0, nocons  vce(cluster id)
//Remark : specifications 5, 6 have different values for deltas. This is because eta_gain eta_air and eta_fertility are also one for instantaneous questions. So only after eta_gain, eta_air and eta_fertility are allowed to vary for instantaneous, brownF, greenF, do deltas reflect means of time-only questions. I still do not understand why delta’s change between specification 6 and 7. This is just subdividing dummies into mutually exclusive subcategories, concerning other questions that time-only. Never mind, effects are minimal, I spent hours thinking on this.  

//WALD TEST IF 2020 DATA IS SIGNIFICANTLY DIFFERENT
//H0 data are idential always rejected with pvalue<1%
foreach var1 in delta eta_inst eta_brownF eta_greenF ///
				delta_air delta_fertility eta_air eta_fertility ///
				delta_gain delta_decisionPast delta_twoRegions eta_gain eta_decisionPast eta_F_twoRegions ///
				eta_inst_air   eta_inst_fertility   eta_inst_gain eta_brownF_air eta_brownF_fertility eta_brownF_gain  eta_greenF_air eta_greenF_fertility eta_greenF_gain {
gen `var1'2=`var1'*(repl2020==1)
}
//drop if irrational>0
//drop if group==1| group==2 
reg discount_rate 	delta eta_inst eta_brownF eta_greenF delta2 eta_inst2 eta_brownF2 eta_greenF2, nocons vce(cluster id) 
	testparm *2 //Identical to:	test (0 = _b[delta2]) (0 = _b[eta_inst2]) (0 = _b[eta_brownF2]) (0 = _b[eta_greenF2])
	outreg2 using OLS_repl_test, word  auto(2) addstat(Wald test replication (p value),`r(p)') replace  
reg discount_rate   delta  delta_air  delta_fertility  eta_inst  eta_brownF  eta_greenF  eta_air  eta_fertility ///
					delta2 delta_air2 delta_fertility2 eta_inst2 eta_brownF2 eta_greenF2 eta_air2 eta_fertility2 , nocons  vce(cluster id)
	testparm *2
	outreg2 using OLS_repl_test, word  auto(2) addstat(Wald test replication (p value),`r(p)')
reg discount_rate 	delta  delta_gain  delta_decisionPast  delta_twoRegions   eta_inst  eta_brownF  eta_greenF   eta_gain  eta_decisionPast  eta_F_twoRegions ///
					delta2 delta_gain2 delta_decisionPast2 delta_twoRegions2  eta_inst2 eta_brownF2 eta_greenF2  eta_gain2 eta_decisionPast2 eta_F_twoRegions2 , nocons  vce(cluster id)
	testparm *2
	outreg2 using OLS_repl_test, word  auto(2) addstat(Wald test replication (p value),`r(p)')	
reg discount_rate   delta  delta_air  delta_fertility  delta_gain  delta_decisionPast  delta_twoRegions  eta_inst eta_brownF  eta_greenF  eta_air  eta_fertility  eta_gain  eta_decisionPast  eta_F_twoRegions ///
					delta2 delta_air2 delta_fertility2 delta_gain2 delta_decisionPast2 delta_twoRegions2 eta_inst2 eta_brownF2 eta_greenF2 eta_air2 eta_fertility2 eta_gain2 eta_decisionPast2 eta_F_twoRegions2 , nocons  vce(cluster id)
	testparm *2
	outreg2 using OLS_repl_test, word  auto(2) addstat(Wald test replication (p value),`r(p)')	
reg discount_rate delta  delta_air  delta_fertility  delta_gain  delta_decisionPast  delta_twoRegions  eta_inst   eta_inst_air   eta_inst_fertility   eta_inst_gain  eta_brownF  eta_brownF_air  eta_brownF_fertility  eta_brownF_gain  eta_greenF  eta_greenF_air  eta_greenF_fertility  eta_greenF_gain   eta_decisionPast  eta_F_twoRegions      ///
				  delta2 delta_air2 delta_fertility2 delta_gain2 delta_decisionPast2 delta_twoRegions2 eta_inst2  eta_inst_air2  eta_inst_fertility2  eta_inst_gain2 eta_brownF2 eta_brownF_air2 eta_brownF_fertility2 eta_brownF_gain2 eta_greenF2 eta_greenF_air2 eta_greenF_fertility2 eta_greenF_gain2  eta_decisionPast2  eta_F_twoRegions2 , nocons  vce(cluster id)		
	testparm *2
	outreg2 using OLS_repl_test, word  auto(2) addstat(Wald test replication (p value),`r(p)')

//TEST IF GROUP 1 & 2 HAVE DIFFERENT OUTCOMES. Outcomes are different. If they had not, this showed that there are no ordering effects
//eta_inst should be the same, because not affected by order of decision now/past, nor green/brown 
foreach var1 in delta delta_air delta_fertility delta_gain delta_decisionPast delta_twoRegions  ///
	eta_brownF eta_brownF_air eta_brownF_fertility eta_brownF_gain  ///
	eta_greenF eta_greenF_air eta_greenF_fertility eta_greenF_gain eta_decisionPast eta_F_twoRegions  {
gen `var1'12=`var1'*(group==1|group==2)
}
reg discount_rate delta delta_air delta_fertility delta_gain delta_decisionPast delta_twoRegions ///
	eta_inst eta_inst_air eta_inst_fertility eta_inst_gain ///
	eta_brownF eta_brownF_fertility eta_brownF_air eta_brownF_gain  ///
	eta_greenF eta_greenF_air eta_greenF_fertility eta_greenF_gain eta_decisionPast eta_F_twoRegions  ///
	delta12 delta_air12 delta_fertility12 delta_gain12 delta_decisionPast12 delta_twoRegions12  ///
	eta_brownF12 eta_brownF_fertility12 eta_brownF_air12 eta_brownF_gain12 ///
	eta_greenF12 eta_greenF_air12 eta_greenF_fertility12 eta_greenF_gain12 eta_decisionPast12 eta_F_twoRegions12 , nocons  vce(cluster id)  //if group!=7 & group!=8 & group!=4 does not change test outcome
	 //if group==9&forest==1 | group==3&fertility==1 | group==10&fertility==1 | group==5&air==1 | group==6&forest==1 | group==1 |group==2  does not change test outcome
test (0 = _b[delta12]) (0 = _b[delta_air12]) (0 = _b[delta_fertility12]) (0 = _b[delta_gain12]) (0 = _b[delta_twoRegions12]) ///
	(0 = _b[eta_brownF12]) (0 = _b[eta_brownF_fertility12]) (0 = _b[eta_brownF_air12]) (0 = _b[eta_brownF_gain12]) ///
	(0 = _b[eta_greenF12]) (0 = _b[eta_greenF_air12]) (0 = _b[eta_greenF_fertility12]) (0 = _b[eta_greenF_gain12]) (0 = _b[eta_decisionPast12]) (0 = _b[eta_F_twoRegions12]) 

//TEST DIFFERENCES FOR GROUP 1 and 2. 
//Outcomes are different. If there were no differences, we could have argued that there are no ordering effects.
//problem with following approach is that in group 3-10, decision now is followed by decision past, whereas in group 1 and 2 there is only decision now, except 1 decision past.
//Therefore, file 2 has a test which distinguishes all different effects.		
//eta_inst should be the same, because not affected by order of decision now/past, nor green/brown 
gen greenFirstFor=group==9 & forest==1 
gen brownFirstFor=group==2 & twoRegion==1 & forest==1
gen greenFirstFert=(group==3|group==10) & fertility==1
gen brownFirstFert=group==1 & twoRegion==1 & fertility==1 	   
gen decPastFirstGF=group==5 & air==1 
gen decNowFirstBF=group==2 &twoRegion==1 & air==1 
gen decPastFirstFor=group==6 & forest==1
gen decNowFirstFor=group==1& twoRegion==1 & forest==1

foreach var1 in intertemporal brownFuture greenFuture {
gen `var1'_greenFirstFor=`var1' * greenFirstFor if greenFirstFor+brownFirstFor==1
gen `var1'_brownFirstFor=`var1' * brownFirstFor if greenFirstFor+brownFirstFor==1
gen `var1'_greenFirstFert=`var1' * greenFirstFert if greenFirstFert+brownFirstFert==1
gen `var1'_brownFirstFert=`var1' * brownFirstFert if greenFirstFert+brownFirstFert==1
gen `var1'_decPastFirstGF=`var1' * decPastFirstGF if decPastFirstGF+decNowFirstBF==1
gen `var1'_decNowFirstBF=`var1' * decNowFirstBF if decPastFirstGF+decNowFirstBF==1
gen `var1'_decPastFirstFor=`var1' * decPastFirstFor if decPastFirstFor+decNowFirstFor==1
gen `var1'_decNowFirstFor=`var1' * decNowFirstFor if decPastFirstFor+decNowFirstFor==1
}
reg discount_rate  intertemporal_greenFirstFor intertemporal_brownFirstFor instantaneous ///
				   brownFuture_greenFirstFor brownFuture_brownFirstFor greenFuture_greenFirstFor greenFuture_brownFirstFor  if greenFirstFor+brownFirstFor==1 , nocons vce(cluster id)
test (_b[intertemporal_greenFirstFor] = _b[intertemporal_brownFirstFor])  (_b[brownFuture_greenFirstFor] = _b[brownFuture_brownFirstFor]) (_b[greenFuture_greenFirstFor] = _b[greenFuture_brownFirstFor])
reg discount_rate  intertemporal_greenFirstFert intertemporal_brownFirstFert  instantaneous  ///
				   brownFuture_greenFirstFert brownFuture_brownFirstFert greenFuture_greenFirstFert greenFuture_brownFirstFert  if greenFirstFert+brownFirstFert==1 , nocons vce(cluster id)
test (_b[intertemporal_greenFirstFert] = _b[intertemporal_brownFirstFert])  (_b[brownFuture_greenFirstFert] = _b[brownFuture_brownFirstFert]) (_b[greenFuture_greenFirstFert] = _b[greenFuture_brownFirstFert])
reg discount_rate  intertemporal_decPastFirstGF intertemporal_decNowFirstBF instantaneous  ///
				   brownFuture_decPastFirstGF brownFuture_decNowFirstBF greenFuture_decPastFirstGF greenFuture_decNowFirstBF  if decPastFirstGF+decNowFirstBF==1 , nocons vce(cluster id)
test (_b[intertemporal_decPastFirstGF] = _b[intertemporal_decNowFirstBF]) (_b[brownFuture_decPastFirstGF] = _b[brownFuture_decNowFirstBF]) (_b[greenFuture_decPastFirstGF] = _b[greenFuture_decNowFirstBF])
reg discount_rate  intertemporal_decPastFirstFor intertemporal_decNowFirstFor instantaneous  ///
				   brownFuture_decPastFirstFor brownFuture_decNowFirstFor greenFuture_decPastFirstFor greenFuture_decNowFirstFor  if decPastFirstFor+decNowFirstFor==1 , nocons vce(cluster id)
test (_b[intertemporal_decPastFirstFor] = _b[intertemporal_decNowFirstFor]) (_b[brownFuture_decPastFirstFor] = _b[brownFuture_decNowFirstFor]) (_b[greenFuture_decPastFirstFor] = _b[greenFuture_decNowFirstFor])
//the last test is the only that gives a non-significant result

	
//NONLINEAR REGRESSIONS
nl (discount_factor    =exp(-({delta=1}*intertemporal +{eta_inst=1.5}*eta_inst +{eta_brownF}*eta_brownF+{eta_greenF}*eta_greenF)/100)) 
estimates store exp_r_1
nl (discount_factor_20y=exp(-({delta=1}*intertemporal +{eta_inst=1.5}*eta_inst +{eta_brownF}*eta_brownF+{eta_greenF}*eta_greenF)*20/100)) 
estimates store exp_r_20y_1
nl (ln_discount_rate   = ln( {delta=1}*intertemporal +{eta_inst=1.86}*eta_inst + {eta_greenF=1}*eta_greenF)) //if no starting values are given, parameters start at zero, resulting in ln(0)
estimates store ln_r_1

nl (discount_factor    =exp(-({delta=0.75}*delta +{delta_air}*delta_air +{delta_fertility}*delta_fertility +{delta_gain}*delta_gain +{delta_twoRegions}*delta_twoRegions +{delta_decisionPast}*delta_decisionPast ///
+{eta_inst=2}*eta_inst +{eta_brownF}*eta_brownF +{eta_greenF}*eta_greenF +{eta_air}*eta_air +{eta_fertility}*eta_fertility +{eta_gain}*eta_gain )/100)) 
estimates store exp_r_2
nl (discount_factor_20y=exp(-({delta=0.75}*delta +{delta_air}*delta_air +{delta_fertility}*delta_fertility +{delta_gain}*delta_gain +{delta_twoRegions}*delta_twoRegions +{delta_decisionPast}*delta_decisionPast ///
+{eta_inst=2}*eta_inst +{eta_brownF}*eta_brownF +{eta_greenF}*eta_greenF +{eta_air}*eta_air +{eta_fertility}*eta_fertility +{eta_gain}*eta_gain )*20/100)) 
estimates store exp_r_20y_2
nl (ln_discount_rate   = ln({delta=0.75}*delta +{delta_air}*delta_air +{delta_fertility}*delta_fertility +{delta_gain}*delta_gain +{delta_twoRegions}*delta_twoRegions +{delta_decisionPast}*delta_decisionPast ///
+{eta_inst=2}*eta_inst +{eta_brownF}*eta_brownF +{eta_greenF}*eta_greenF +{eta_air}*eta_air +{eta_fertility}*eta_fertility +{eta_gain}*eta_gain ))
estimates store ln_r_2

nl (discount_factor    =exp(-({delta=0.75}*delta +{delta_air}*delta_air +{delta_fertility}*delta_fertility +{delta_gain}*delta_gain +{delta_twoRegions}*delta_twoRegions +{delta_decisionPast}*delta_decisionPast ///
+{eta_inst=2}*eta_inst +{eta_brownF}*eta_brownF +{eta_greenF}*eta_greenF +{eta_air}*eta_air +{eta_fertility}*eta_fertility +{eta_gain}*eta_gain +{eta_F_twoRegions}*eta_F_twoRegions+{eta_decisionPast}*eta_decisionPast)/100)) 
estimates store exp_r_3
nl (discount_factor_20y=exp(-({delta=0.75}*delta +{delta_air}*delta_air +{delta_fertility}*delta_fertility +{delta_gain}*delta_gain +{delta_twoRegions}*delta_twoRegions +{delta_decisionPast}*delta_decisionPast ///
+{eta_inst=2}*eta_inst +{eta_brownF}*eta_brownF +{eta_greenF}*eta_greenF +{eta_air}*eta_air +{eta_fertility}*eta_fertility +{eta_gain}*eta_gain  +{eta_F_twoRegions}*eta_F_twoRegions+{eta_decisionPast}*eta_decisionPast)*20/100)) 
estimates store exp_r_20y_3
nl (ln_discount_rate   = ln({delta=0.75}*delta +{delta_air}*delta_air +{delta_fertility}*delta_fertility +{delta_gain}*delta_gain +{delta_twoRegions}*delta_twoRegions +{delta_decisionPast}*delta_decisionPast ///
+{eta_inst=2}*eta_inst +{eta_brownF}*eta_brownF +{eta_greenF}*eta_greenF +{eta_air}*eta_air +{eta_fertility}*eta_fertility +{eta_gain}*eta_gain  +{eta_F_twoRegions}*eta_F_twoRegions+{eta_decisionPast}*eta_decisionPast))
estimates store ln_r_3

estimates table  exp_r_1 exp_r_20y_1 ln_r_1 exp_r_2 exp_r_20y_2 ln_r_2 exp_r_3 exp_r_20y_3 ln_r_3 , star(0.1 0.05 0.01)  b(%3.2f) varlabel stats(N ll r2_a bic aic)  //style(noline)  //se
estimates table  exp_r_1 exp_r_20y_1 exp_r_2 exp_r_20y_2 exp_r_3 exp_r_20y_3 , star(0.1 0.05 0.01)  b(%3.2f) varlabel stats(N ll r2_a bic aic)  //style(noline)  //se
estimates save NonLinearRegressions, replace
//NONLINEAR REGRESSIONS for subsample with only rational
drop if irrational==1 //Check, this must probably be drop if irrational>0
nl (discount_factor    =exp(-({delta=1}*intertemporal +{eta_inst=1.5}*eta_inst +{eta_brownF}*eta_brownF+{eta_greenF}*eta_greenF)/100)) 
estimates store exp_r_1
nl (discount_factor_20y=exp(-({delta=1}*intertemporal +{eta_inst=1.5}*eta_inst +{eta_brownF}*eta_brownF+{eta_greenF}*eta_greenF)*20/100)) 
estimates store exp_r_20y_1
nl (discount_factor    =exp(-({delta=0.75}*delta +{delta_air}*delta_air +{delta_fertility}*delta_fertility +{delta_gain}*delta_gain +{delta_twoRegions}*delta_twoRegions +{delta_decisionPast}*delta_decisionPast ///
+{eta_inst=2}*eta_inst +{eta_brownF}*eta_brownF +{eta_greenF}*eta_greenF +{eta_air}*eta_air +{eta_fertility}*eta_fertility +{eta_gain}*eta_gain )/100)) 
estimates store exp_r_2
nl (discount_factor_20y=exp(-({delta=0.75}*delta +{delta_air}*delta_air +{delta_fertility}*delta_fertility +{delta_gain}*delta_gain +{delta_twoRegions}*delta_twoRegions +{delta_decisionPast}*delta_decisionPast ///
+{eta_inst=2}*eta_inst +{eta_brownF}*eta_brownF +{eta_greenF}*eta_greenF +{eta_air}*eta_air +{eta_fertility}*eta_fertility +{eta_gain}*eta_gain )*20/100)) 
estimates store exp_r_20y_2
nl (discount_factor    =exp(-({delta=0.75}*delta +{delta_air}*delta_air +{delta_fertility}*delta_fertility +{delta_gain}*delta_gain +{delta_twoRegions}*delta_twoRegions +{delta_decisionPast}*delta_decisionPast ///
+{eta_inst=2}*eta_inst +{eta_brownF}*eta_brownF +{eta_greenF}*eta_greenF +{eta_air}*eta_air +{eta_fertility}*eta_fertility +{eta_gain}*eta_gain +{eta_F_twoRegions}*eta_F_twoRegions+{eta_decisionPast}*eta_decisionPast)/100)) 
estimates store exp_r_3
nl (discount_factor_20y=exp(-({delta=0.75}*delta +{delta_air}*delta_air +{delta_fertility}*delta_fertility +{delta_gain}*delta_gain +{delta_twoRegions}*delta_twoRegions +{delta_decisionPast}*delta_decisionPast ///
+{eta_inst=2}*eta_inst +{eta_brownF}*eta_brownF +{eta_greenF}*eta_greenF +{eta_air}*eta_air +{eta_fertility}*eta_fertility +{eta_gain}*eta_gain  +{eta_F_twoRegions}*eta_F_twoRegions+{eta_decisionPast}*eta_decisionPast)*20/100)) 
estimates store exp_r_20y_3
estimates table  exp_r_1 exp_r_20y_1 exp_r_2 exp_r_20y_2 exp_r_3 exp_r_20y_3 , star(0.1 0.05 0.01)  b(%3.2f) varlabel stats(N ll r2_a bic aic)  //style(noline)  //se
estimates save NonLinearReg_OnlyRationalResp, replace

