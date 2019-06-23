
/* 1. Data adjustment  */
* labelling 
	label variable sr_0 "Celltrion"
	label variable sr_1 "Hanmi"
	label variable sr_2 "Yuhan"
	label variable sr_3 "Daewoong"
	label variable sr_4 "GreenCross"
	label variable sr_5 "DongaST"
	drop in 1/2
	drop in 1474/1476
	rename symbol date

* date format, destring variables
	gen daily = date(date,"YMD")   
	format daily %td  
	drop date 
	destring _all, replace force

	gen t=_n
	tsset t // date: 2013-06-03 to 2019-06-03(obs 1472)
	save rawdata.dta, replace


/* 2. Basic stat. */
* Data description
	forv i=0/5{
		summarize sr_`i',d
	}
		** Jarque bera
		forv i=0/5{
			jb sr_`i'
		}

		** q-stat (ljung-box) for residuals autocorrelation
		forv i=0/5{
			corrgram sr_`i',lags(15)
		}

		** ARCH-LM
		forv i=0/5{
			reg sr_`i' L.sr_`i'
			estat archlm, lag(1/10)
		}

* corr
	pwcorr sr_0 sr_1 sr_2 sr_3 sr_4 sr_5, sig star(.01) 

* tsline graph : SR
	rename daily Date
	tsset Date

	forv i=0/5 {
		qui tsline sr_`i', name(d`i', replace)
	}
	graph combine d0 d1 d2 d3 d4 d5, cols(2)
	/* 	// extrate date (month and year)
		ssc install numdate
		extrdate month month=Date
		extrdate year year=Date
		// combine month&year to bys:
		gen monthyear = real(string(year)+string(month))
		drop year month
		drop monthyear
	 */

 
/* 3. Unit root. Cointrgraion test */
* Unit root 
	forv i=0/5{
		dfuller sr_`i', constant	
	}
	forv i=0/5{
		dfuller sr_`i', noconstant	
	}
	forv i=0/5{
		dfuller sr_`i', trend
	}

* Cointegraion
	vecrank sr_0 sr_1 sr_2 sr_3 sr_4 sr_5, lags(1) t(constant)
	vecrank sr_0 sr_1 sr_2 sr_3 sr_4 sr_5, lags(1) t(none)
	vecrank sr_0 sr_1 sr_2 sr_3 sr_4 sr_5, lags(1) t(trend) //no cointegration

 
/* 4. VAR-DCC-MGARCH */
* DCC-MGARCH : SR, 2013-06-03 to 2019-06-03, lag 1
	tsset t
	mgarch dcc (sr_0 sr_1 sr_2 sr_3 sr_4 sr_5 = L.sr_0 L.sr_1 L.sr_2 L.sr_3 L.sr_4 L.sr_5), arch(1) garch(1)

	estat summarize
	estimates


/* 5. Conditional Correlation */
* (1) conditional variance & covariance, residuals
	predict H*, variance 
	predict R*, residuals

	tsset t
* (2) make stadardized residual = residual/standard deviation
// (Standard deviation (S) = square root of the variance)
	forv i=0/5 {
		gen SDR_sr_`i' = R_sr_`i'/(sqrt(H_sr_`i'_sr_`i'))
	}

	/* gen SDR_sr = R_sr/(sqrt(H_sr_sr))
	gen SDR_sr_1 = R_sr_1/(sqrt(H_sr_1_sr_1))
	gen SDR_sr_2 = R_sr_2/(sqrt(H_sr_2_sr_2))
	gen SDR_sr_3 = R_sr_3/(sqrt(H_sr_3_sr_3))
	gen SDR_sr_4 = R_sr_4/(sqrt(H_sr_4_sr_4))
	gen SDR_sr_5 = R_sr_5/(sqrt(H_sr_5_sr_5)) */

		// DCC-MGARCH: Q(.) for stadardized residual: ljung-box
		forv i=0/5{
			lmalb SDR_sr_`i', lags(15)
		}
		//DCC-MGARCH: SQ(.) for squared stadardized residual
		forv i=0/5{
			gen SDR2_sr_`i' = SDR_sr_`i'^2
		}
		forv i=0/5{
			lmalb SDR2_sr_`i', lags(15)
		}

		// normal test of standardized residuaal
		forv i=0/5{
			jb SDR_sr_`i'
		}

* (3) conditional correlation = covariance12/sqrt(var1*var2)
	forv i=1/5{
		gen corr_sr_`i'_sr_0 = H_sr_`i'_sr_0/(sqrt(H_sr_0_sr_0)*sqrt(H_sr_`i'_sr_`i'))
	}
	/* gen corr_sr_1_sr = H_sr_1_sr/(sqrt(H_sr_sr))*(sqrt(H_sr_1_sr_1))
	gen corr_sr_2_sr = H_sr_2_sr/(sqrt(H_sr_sr))*(sqrt(H_sr_2_sr_2))
	gen corr_sr_3_sr = H_sr_3_sr/(sqrt(H_sr_sr))*(sqrt(H_sr_3_sr_3))
	gen corr_sr_4_sr = H_sr_4_sr/(sqrt(H_sr_sr))*(sqrt(H_sr_4_sr_4))
	gen corr_sr_5_sr = H_sr_5_sr/(sqrt(H_sr_sr))*(sqrt(H_sr_5_sr_5)) */

	forv i=2/5{
		gen corr_sr_`i'_sr_1 = H_sr_`i'_sr_1/(sqrt(H_sr_1_sr_1)*sqrt(H_sr_`i'_sr_`i'))
	}
	/* gen corr_sr_2_sr_1 = H_sr_2_sr_1/(sqrt(H_sr_1_sr_1))*(sqrt(H_sr_2_sr_2))
	gen corr_sr_3_sr_1 = H_sr_3_sr_1/(sqrt(H_sr_1_sr_1))*(sqrt(H_sr_3_sr_3))
	gen corr_sr_4_sr_1 = H_sr_4_sr_1/(sqrt(H_sr_1_sr_1))*(sqrt(H_sr_4_sr_4))
	gen corr_sr_5_sr_1 = H_sr_5_sr_1/(sqrt(H_sr_1_sr_1))*(sqrt(H_sr_5_sr_5))
	 */

 	forv i=3/5{
		gen corr_sr_`i'_sr_2 = H_sr_`i'_sr_2/(sqrt(H_sr_2_sr_2)*sqrt(H_sr_`i'_sr_`i'))
	}
	/* gen corr_sr_3_sr_2 = H_sr_3_sr_2/(sqrt(H_sr_2_sr_2))*(sqrt(H_sr_3_sr_3))
	gen corr_sr_4_sr_2 = H_sr_4_sr_2/(sqrt(H_sr_2_sr_2))*(sqrt(H_sr_4_sr_4))
	gen corr_sr_5_sr_2 = H_sr_5_sr_2/(sqrt(H_sr_2_sr_2))*(sqrt(H_sr_5_sr_5))
	 */
  	forv i=4/5{
 		gen corr_sr_`i'_sr_3 = H_sr_`i'_sr_3/(sqrt(H_sr_3_sr_3)*sqrt(H_sr_`i'_sr_`i'))
	}
	/* gen corr_sr_4_sr_3 = H_sr_4_sr_3/(sqrt(H_sr_3_sr_3))*(sqrt(H_sr_4_sr_4))
	gen corr_sr_5_sr_3 = H_sr_5_sr_3/(sqrt(H_sr_3_sr_3))*(sqrt(H_sr_5_sr_5))
	 */
	gen corr_sr_5_sr_4 = H_sr_5_sr_4/(sqrt(H_sr_4_sr_4)*sqrt(H_sr_5_sr_5))
	gsort t

save bio6_corr.dta,replace

/* 6. Conditional correlation graph */

* (1) conditional variance (GARCH), Graph1
	tsset Date
	forv i=0/5 {
	qui tsline H_sr_`i'_sr_`i', name(H_sr_`i', replace)
	}
	graph combine H_sr_0 H_sr_1 H_sr_2 H_sr_3 H_sr_4 H_sr_5 , cols(2)

	/* 	qui tsline H_sr_sr, name(v, replace)
		qui tsline H_sr_1_sr_1, name(v1, replace)
		qui tsline H_sr_2_sr_2, name(v2, replace)
		qui tsline H_sr_3_sr_3, name(v3, replace)
		qui tsline H_sr_4_sr_4, name(v4, replace)
		qui tsline H_sr_5_sr_5, name(v5, replace)

		graph combine v v1 v2 v3 v4 v5, cols(2) */

* (2) conditional co-variance, Graph2
	forv i=1/5{
		qui tsline H_sr_`i'_sr_0, name(cv0`i', replace)
	}

	forv i=2/5{
		qui tsline H_sr_`i'_sr_1, name(cv1`i', replace)
	}

 	forv i=3/5{
		qui tsline H_sr_`i'_sr_2, name(cv2`i', replace)
	}

  	forv i=4/5{
		qui tsline H_sr_`i'_sr_3, name(cv3`i', replace)
	}
	qui tsline H_sr_5_sr_4, name(cv45, replace)

	graph combine cv01 cv02 cv03 cv04 cv05 cv12 cv13 cv14 cv15 cv23 cv24 cv25 cv34 cv35 cv45, cols(3)
	/* 
		qui tsline H_sr_1_sr, name(cv1, replace)
		qui tsline H_sr_2_sr, name(cv2, replace)
		qui tsline H_sr_3_sr, name(cv3, replace)
		qui tsline H_sr_4_sr, name(cv4, replace)
		qui tsline H_sr_5_sr, name(cv5, replace)

		qui tsline H_sr_2_sr_1, name(cv12, replace)
		qui tsline H_sr_3_sr_1, name(cv13, replace)
		qui tsline H_sr_4_sr_1, name(cv14, replace)
		qui tsline H_sr_5_sr_1, name(cv15, replace)

		qui tsline H_sr_3_sr_2, name(cv23, replace)
		qui tsline H_sr_4_sr_2, name(cv24, replace)
		qui tsline H_sr_5_sr_2, name(cv25, replace)

		qui tsline H_sr_4_sr_3, name(cv34, replace)
		qui tsline H_sr_5_sr_3, name(cv35, replace)

		qui tsline H_sr_5_sr_4, name(cv45, replace) */

* (3) Conditional correlations, Graph3
	forv i=1/5{
		qui tsline corr_sr_`i'_sr_0, name(cor0`i', replace)
	}

	forv i=2/5{
		qui tsline corr_sr_`i'_sr_1, name(cor1`i', replace)
	}

 	forv i=3/5{
		qui tsline corr_sr_`i'_sr_2, name(cor2`i', replace)
	}

  	forv i=4/5{
		qui tsline corr_sr_`i'_sr_3, name(cor3`i', replace)
	}
		qui tsline corr_sr_5_sr_4, name(cor45, replace)
	
		graph combine cor01 cor02 cor03 cor04 cor05 cor12 cor13 cor14 cor15 cor23 cor24 cor25 cor34 cor35 cor45, cols(3)

	/* 	qui tsline corr_sr_1_sr, name(cor1, replace)
		qui tsline corr_sr_2_sr, name(cor2, replace)
		qui tsline corr_sr_3_sr, name(cor3, replace)
		qui tsline corr_sr_4_sr, name(cor4, replace)
		qui tsline corr_sr_5_sr, name(cor5, replace)

		qui tsline corr_sr_2_sr_1, name(cor12, replace)
		qui tsline corr_sr_3_sr_1, name(cor13, replace)
		qui tsline corr_sr_4_sr_1, name(cor14, replace)
		qui tsline corr_sr_5_sr_1, name(cor15, replace)

		qui tsline corr_sr_3_sr_2, name(cor23, replace)
		qui tsline corr_sr_4_sr_2, name(cor24, replace)
		qui tsline corr_sr_5_sr_2, name(cor25, replace)

		qui tsline corr_sr_4_sr_3, name(cor34, replace)
		qui tsline corr_sr_5_sr_3, name(cor35, replace)

		qui tsline corr_sr_5_sr_4, name(cor45, replace) */


/* 7. merge BIO + KOSPI + VKOSPI */ 

* merge KOSPI, VKOSPI : 2013-06-03 to 2019-06-03 
// KOSPI
	clear 
	cd "/Users/daeunjung/Desktop"
	set more off
	import excel "kospi13_19.xls", firstrow

	destring _all,replace force
	save kospi1319.dta

// VKOSPI
	clear
	import excel "vkospi13_19.xls",firstrow
	rename A Date
	rename B vkospi

	gen daily = date(Date,"YMD")   
	format daily %td  
	drop Date
	rename daily Date
	order Date vkospi
	save vkospi1319.dta

// KOSPI + VKOSPI
	clear 
	use kospi1319.dta
	keep Date Close
	merge n:n Date using vkospi1319.dta

	drop in 543
	replace Close = 2026.16 in 883
	replace Close = 2329.65 in 1049
	replace Close = 2388.71 in 1471
	replace Close = 2472.37 in 1472

	sort Date
	drop _merge
	save vkskos.dta,replace

// bio + KOSPI + VKOSPI
	clear
	use bio6_corr.dta
	merge 1:1 Date using vkskos.dta
	gsort Date

	list Date if Close==.|vkospi==.
	drop _merge
	save bio6_merge.dta,replace

/* 8. Regression */ 

//gen lag DC
	g corr1_sr_1_sr_0	=	corr_sr_1_sr_0[_n-1]
	g corr1_sr_2_sr_0	=	corr_sr_2_sr_0[_n-1]
	g corr1_sr_3_sr_0	=	corr_sr_3_sr_0[_n-1]
	g corr1_sr_4_sr_0	=	corr_sr_4_sr_0[_n-1]
	g corr1_sr_5_sr_0	=	corr_sr_5_sr_0[_n-1]
			
	g corr1_sr_2_sr_1	=	corr_sr_2_sr_1[_n-1]
	g corr1_sr_3_sr_1	=	corr_sr_3_sr_1[_n-1]
	g corr1_sr_4_sr_1	=	corr_sr_4_sr_1[_n-1]
	g corr1_sr_5_sr_1	=	corr_sr_5_sr_1[_n-1]
				
	g corr1_sr_3_sr_2	=	corr_sr_3_sr_2[_n-1]
	g corr1_sr_4_sr_2	=	corr_sr_4_sr_2[_n-1]
	g corr1_sr_5_sr_2	=	corr_sr_5_sr_2[_n-1]
				
	g corr1_sr_4_sr_3	=	corr_sr_4_sr_3[_n-1]
	g corr1_sr_5_sr_3	=	corr_sr_5_sr_3[_n-1]
				
	g corr1_sr_5_sr_4	=	corr_sr_5_sr_4[_n-1]

// log diff. kospi and vkospi 
	tsset t

	gen l_Close = log(Close)
	gen d_Close = d.l_Close

	gen l_vkospi = log(vkospi)
	gen d_vkospi = d.l_vkospi

// regression : corr = lag.corr kospi vkospi			
	reg	corr_sr_1_sr_0	corr1_sr_1_sr_0	sr_0 sr_1 Close vkospi d_Close d_vkospi
	estimates store m1, title(Model 1)
	reg	corr_sr_2_sr_0	corr1_sr_2_sr_0	sr_0 sr_2 Close vkospi d_Close d_vkospi
	estimates store m2, title(Model 2)
	reg	corr_sr_3_sr_0	corr1_sr_3_sr_0	sr_0 sr_3 Close vkospi d_Close d_vkospi 
	estimates store m3, title(Model 3)
	reg	corr_sr_4_sr_0	corr1_sr_4_sr_0	sr_0 sr_4 Close vkospi d_Close d_vkospi
	estimates store m4, title(Model 4)
	reg	corr_sr_5_sr_0	corr1_sr_5_sr_0	sr_0 sr_5 Close vkospi d_Close d_vkospi
	estimates store m5, title(Model 5)
					
	reg	corr_sr_2_sr_1	corr1_sr_2_sr_1	sr_1 sr_2 Close vkospi d_Close d_vkospi
	estimates store m6, title(Model 6)
	reg	corr_sr_3_sr_1	corr1_sr_3_sr_1	sr_1 sr_3 Close vkospi d_Close d_vkospi
	estimates store m7, title(Model 7)
	reg	corr_sr_4_sr_1	corr1_sr_4_sr_1	sr_1 sr_4 Close vkospi d_Close d_vkospi
	estimates store m8, title(Model 8)
	reg	corr_sr_5_sr_1	corr1_sr_5_sr_1	sr_1 sr_5 Close vkospi d_Close d_vkospi
	estimates store m9, title(Model 9)
		
	reg	corr_sr_3_sr_2	corr1_sr_3_sr_2	sr_2 sr_3 Close vkospi d_Close d_vkospi
	estimates store m10, title(Model 10)
	reg	corr_sr_4_sr_2	corr1_sr_4_sr_2	sr_2 sr_4 Close vkospi d_Close d_vkospi
	estimates store m11, title(Model 11)
	reg	corr_sr_5_sr_2	corr1_sr_5_sr_2	sr_2 sr_5 Close vkospi d_Close d_vkospi
	estimates store m12, title(Model 12)
					
	reg	corr_sr_4_sr_3	corr1_sr_4_sr_3	sr_3 sr_4 Close vkospi d_Close d_vkospi
	estimates store m13, title(Model 13)
	reg	corr_sr_5_sr_3	corr1_sr_5_sr_3	sr_3 sr_5 Close vkospi d_Close d_vkospi
	estimates store m14, title(Model 14)
					
	reg	corr_sr_5_sr_4	corr1_sr_5_sr_4	sr_4 sr_5 Close vkospi d_Close d_vkospi
	estimates store m15, title(Model 15)

	estout m1 m2 m3 m4 m5 ,cells(b(star fmt(3)) se(par fmt(2)))
	estout m6 m7 m8 m9 m10 ,cells(b(star fmt(3)) se(par fmt(2)))
	estout m11 m12 m13 m14 m15 ,cells(b(star fmt(3)) se(par fmt(2)))


/* 9. VAR */ 
// optimal lags
	varsoc sr_0 sr_1 sr_2 sr_3 sr_4 sr_5 
// Vector Autoregressive
	var sr_*, lags(4) lut
// vargranger
	vargranger






