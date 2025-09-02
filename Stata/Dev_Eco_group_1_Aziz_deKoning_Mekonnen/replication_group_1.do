clear

*****************************************************************************
*** REPLICATION and EXTENSION of the Paper "Parents' Beliefs about Their Children's Academic Ability: Implications for Educational Investments." (2019)

*Group 1: Christelle Aziz, Teun de Koning, Hibst Mekonnen

*22.09.2024

*****************************************************************************

*Install Packages if the package estout is not installed yet
*ssc install estouts

*** Set up

	clear all
	set mem 1000M
	set matsize 11000
	set more off
	cap log close
	set scheme s1color

		
	if c(os) == "Windows" local prefix "C:"
	if c(os) == "MacOSX"  local prefix ""
	
	global main_folder "`prefix'/Users/Hibst1/Documents/Dev_Eco_group_1_Aziz_deKoning_Mekonnen/main_folder" // The main folder needs to be adjusted to the user's preferred folder.
	display in red "The main folder is: $main_folder"
	
	*** Set directories
	
	global output_data "$main_folder/output_data"
	global output_gph  "$main_folder/output_graphs"
	global output_tbl  "$main_folder/output_tables"
*****************************************************************************
* Add all figures and tables to one pdf

	putpdf begin
	
*****************************************************************************

local wb_scale = 1 //parametrization of advanced workbooks in the data	
local wb_scale_lbl = 1 //parametrization for labeling advanced workbooks on the graphs 	

	*Load data
		use "$output_data/Ability_Clean_2.dta", clear

***********************************************************
***********************************************************
*   Recreation of Figures
***********************************************************
***********************************************************

	* Create dummy for treatment
		gen treat_dum = treat == 1
		
	* Labeling for graphing later on
		label define treat_dum 0 "Control" 1 "Treat"
		label values treat_dum treat_dum

	* Generate the different variables needed
	gen dp_ave_t = dp_ave if treat == 1 // generate abs. val. baseline beliefs - true score if treat
	gen dp_ave_c = dp_ave if treat == 0 //generate abs. val. baseline beliefs - true score if control
	gen dpu_ave_t = dpu_ave if treat == 1 //generate abs. val. endline beliefs - true score if treat
	gen dpu_ave_c = dpu_ave if treat == 0 //generate abs. val. endline beliefs - true score if control
	
	* Checking these variables
		sum dp_ave_t dp_ave_c dpu_ave_t dpu_ave_c
		hist dp_ave_t
		hist dp_ave_c
		hist dpu_ave_t
		hist dpu_ave_c

	*after checking we have to drop one datapoint with household id 968 child 2
		drop if hhid==968 & refchild==2

*******************************************
*	Graphing panels (a) and (c)
*******************************************
	
	*Graphing graph 3a
		quietly graph bar dp_ave, over(treat_dum) bargap(50) legend(off) ytitle("Abs. Val. (True Score - Baseline beliefs)") aspect(1) blab(bar, position(base) format(%5.1f) size(medlarge)) bar(1, fcolor(stblue)) bar(2, fcolor(midgreen) lc(midgreen)) asyvars showyvars title("(a) Gap between True Scores and Baseline Beliefs", span size(medium))
	quietly graph export "$output_gph/Replication_3a.png", replace 
	
	putpdf paragraph
	putpdf image "$output_gph/Replication_3a.png", width(15cm)
	
	*Graphing graph 3c
		quietly graph bar dpu_ave, over(treat_dum) bargap(50) legend(off) ytitle("Abs. Val. (True Score - Endline beliefs)") aspect(1) blab(bar, position(base) format(%5.1f) size(medlarge)) bar(1, fcolor(stblue)) bar(2, fcolor(midgreen) lc(midgreen)) asyvars showyvars title("(c) Gap between True Scores and Endline Beliefs", span size(medium))
		quietly graph export "$output_gph/Replication_3c.png", replace 
	putpdf paragraph
	putpdf image "$output_gph/Replication_3c.png", width(15cm)
	
*******************************************
*	Graphing panels (b) and (d)
*******************************************

	* Generate the different variables needed
	gen b_ave_t = b_ave if treat==1 // Rounded belief of child's performance if treated
	gen b_ave_c = b_ave if treat==0 // Rounded belief of child's performance if NOT treated
	gen u_ave_t = u_ave if treat==1 // Rounded UPDATED belief of child's performance if treated
	gen u_ave_c = u_ave if treat==0 // Rounded UPDATED belief of child's performance if NOT treated

* Graphing Figure 3(b)
	quietly twoway (lpolyci b_ave_t ave, clp(dash) fcolor(gs13)) (lpolyci b_ave_c ave, clc(stblue) clw(medthick)) (lpoly b_ave_t ave, clp(dash) clc(midgreen) clw(medthick)), yscale(range(0,100)) ylab(#6) ytit("") xtit("True scores", size(medlarge)) legend(order (4 5) label(4 "Control") label (5 "Treatment") bplace(se) ring(0)) aspect(1) title("(b) Attenuation of Baseline Beliefs", span size(medium))
	quietly graph export "$output_gph/Replication_3b.png", replace 
	putpdf paragraph
	putpdf image "$output_gph/Replication_3b.png", width(15cm)
	
* Graphing Figure 3(d)
	quietly twoway (lpolyci u_ave_t ave, clp(dash)  fcolor(gs13)) (lpolyci u_ave_c ave, clc(stblue) clw(medthick)) (lpoly u_ave_t ave, clp(dash) clc(midgreen) clw(medthick)), yscale(range(0,100))	ylab(#6) ytit("") xtit("True scores", size(medlarge)) legend(order (4 5) label(4 "Control") label (5 "Treatment") bplace(se) ring(0)) aspect(1) title("(d) Attenuation of Endline Beliefs", span size(medium))
	quietly graph export "$output_gph/Replication_3d.png", replace 
	putpdf paragraph
	putpdf image "$output_gph/Replication_3d.png", width(15cm)
***********************************************************
***********************************************************
*	Gender modification
***********************************************************
***********************************************************

	* Setting up gendered dummies for variables of interest
	
		gen b_ave_t_f = b_ave_t if female==1
		gen b_ave_t_m = b_ave_t if female==0
		gen b_ave_c_f = b_ave_c if female==1
		gen b_ave_c_m = b_ave_c if female==0
		gen u_ave_t_f = u_ave_t if female==1
		gen u_ave_t_m = u_ave_t if female==0
		gen u_ave_c_f = u_ave_c if female==1
		gen u_ave_c_m = u_ave_c if female==0

		label define female 0 "Male" 1 "Female"
		label values female female
	
	* Graphing Figure 4a with gender heterogeneity
		quietly graph bar dp_ave, over(treat_dum) over(female) bargap(50) legend(off) ytitle("Abs. Val. (True Score - Baseline beliefs)") aspect(1) blab(bar, position(base) format(%5.1f) size(medlarge)) bar(1, fcolor(stblue)) bar(2, fcolor(midgreen) lc(midgreen)) asyvars showyvars title("Figure 4a with Gender Heterogeneity", span size(medium))
		quietly graph export "$output_gph/Gender_heterogeneity_4a.png", replace 
		
		putpdf paragraph
		putpdf image "$output_gph/Gender_heterogeneity_4a.png", width(15cm)
		
	* Graphing graph 4c with gender heterogeneity
		quietly graph bar dpu_ave, over(treat_dum) over(female) bargap(50) legend(off) ytitle("Abs. Val. (True Score - Endline beliefs)") aspect(1) blab(bar, position(base) format(%5.1f) size(medlarge))bar(1, fcolor(stblue)) bar(2, fcolor(midgreen) lc(midgreen)) asyvars showyvars title("Figure 4c with Gender Heterogeneity", span size(medium))
		quietly graph export "$output_gph/Gender_heterogeneity_4c.png", replace 
	
		putpdf paragraph
		putpdf image "$output_gph/Gender_heterogeneity_4c.png", width(15cm)
		
	* Graphing graph 4b for control with gender heterogeneity
		twoway (lpolyci b_ave_c_f ave, clp(dash)  fcolor(gs13)) (lpolyci b_ave_c_m ave, clc(stblue) clw(medthick)) (lpoly b_ave_c_f ave, clp(dash) clc(red) clw(medthick)), yscale(range(0,100)) ylab(#6) ytit("") xtit("True scores", size(medlarge)) legend(order (4 5) label(4 "Male") label (5 "Female") bplace(se) ring(0)) aspect(1) title("Figure 4b with Gender Heterogeneity for the Control Group", span size(medium))
		quietly graph export "$output_gph/Gender_heterogeneity_4b_c.png", replace 
		
		putpdf paragraph
		putpdf image "$output_gph/Gender_heterogeneity_4b_c.png", width(15cm)
		
	* Graphing graph 4b' for treatment with gender heterogeneity
		twoway (lpolyci b_ave_t_f ave, clp(dash)  fcolor(gs13)) (lpolyci b_ave_t_m ave, clc(stblue) clw(medthick)) (lpoly b_ave_t_f ave, clp(dash) clc(red) clw(medthick)), yscale(range(0,100))ylab(#6) ytit("") xtit("True scores", size(medlarge)) legend(order (4 5) label(4 "Male") label (5 "Female") bplace(se) ring(0)) aspect(1) title("Figure 4b' with Gender Heterogeneity for the Treatment Group", span size(medium))
		quietly graph export "$output_gph/Gender_heterogeneity_4b_t.png", replace 
		
		putpdf paragraph
		putpdf image "$output_gph/Gender_heterogeneity_4b_t.png", width(15cm)
		
		
	* Graphing graph 4d for control with gender heterogeneity
		twoway (lpolyci u_ave_c_f ave, clp(dash)  fcolor(gs13)) (lpolyci u_ave_c_m ave, clc(stblue) clw(medthick)) (lpoly u_ave_c_f ave, clp(dash) clc(red) clw(medthick)), yscale(range(0,100)) ylab(#6) ytit("") xtit("True scores", size(medlarge)) legend(order (4 5) label(4 "Male") label (5 "Female") bplace(se) ring(0)) aspect(1) title("Figure 4d with Gender Heterogeneity for Control Group", span size(medium))
		quietly graph export "$output_gph/Gender_heterogeneity_4d_c.png", replace 
		
		putpdf paragraph
		putpdf image "$output_gph/Gender_heterogeneity_4d_c.png", width(15cm)
		
	* Graphing graph 4d' for treatment with gender heterogeneity
				twoway (lpolyci u_ave_t_f ave, clp(dash)  fcolor(gs13)) (lpolyci u_ave_t_m ave, clc(stblue) clw(medthick)) (lpoly u_ave_t_f ave, clp(dash) clc(red) clw(medthick)), yscale(range(0,100)) ylab(#6) ytit("") xtit("True scores", size(medlarge)) legend(order (4 5) label(4 "Male") label (5 "Female") bplace(se) ring(0)) aspect(1) title("Figure 4d' with Gender Heterogeneity for Treatment Group", span size(medium))
		quietly graph export "$output_gph/Gender_heterogeneity_4d_t.png", replace 
		
		putpdf paragraph
		putpdf image "$output_gph/Gender_heterogeneity_4d_t.png", width(15cm)

***********************************************************
***********************************************************	
*	Ex Ante High and Low Performing Modification
***********************************************************
***********************************************************

	* Setting up high and low performance dummies for variables of interest
		egen mean_ave = mean(ave)
		gen b_ave_t_h = b_ave_t if ave>mean_ave
		gen b_ave_t_l = b_ave_t if ave<mean_ave
		gen b_ave_c_h = b_ave_c if ave>mean_ave
		gen b_ave_c_l = b_ave_c if ave<mean_ave
		gen u_ave_t_h = u_ave_t if ave>mean_ave
		gen u_ave_t_l = u_ave_t if ave<mean_ave
		gen u_ave_c_h = u_ave_c if ave>mean_ave
		gen u_ave_c_l = u_ave_c if ave<mean_ave
		
		gen betterhalf = (ave>mean_ave)
		label define betterhalf 0 "Low-Performing" 1 "High-Performing"
		label values betterhalf betterhalf
		
	* Graphing graph 5a with ex-ante performance heterogeneity
		graph bar dp_ave, over(treat_dum) over(betterhalf) bargap(50) legend(off) ytitle("Abs. Val. (True Score - Baseline beliefs)") aspect(1) blab(bar, position(base) format(%5.1f) size(medlarge)) bar(1, fcolor(stblue)) bar(2, fcolor(midgreen) lc(midgreen)) asyvars showyvars title("Figure 5a with ex-ante Performance Heterogeneity", span size(medium))
		graph export "$output_gph/Performance_heterogeneity_5a.png", replace 
		
		putpdf paragraph
		putpdf image "$output_gph/Performance_heterogeneity_5a.png", width(15cm)
		
	* Graphing graph 5c with ex-ante performance heterogeneity
		graph bar dpu_ave, over(treat_dum) over(betterhalf) bargap(50) legend(off) ytitle("Abs. Val. (True Score - Endline beliefs)") aspect(1) blab(bar, position(base) format(%5.1f) size(medlarge)) bar(1, fcolor(stblue)) bar(2, fcolor(midgreen) lc(midgreen)) asyvars showyvars title("Figure 5c with ex-ante Performance Heterogeneity", span size(medium))
		
		graph export "$output_gph/Performance_heterogeneity_5c.png", replace 
		
		putpdf paragraph
		putpdf image "$output_gph/Performance_heterogeneity_5c.png", width(15cm)
		
	* Graphing graph 5b for control with ex-ante performance heterogeneity
		twoway (lpolyci b_ave_c_h ave, clp(dash)  fcolor(gs13)) (lpolyci b_ave_c_l ave, clc(stblue) clw(medthick)) (lpoly b_ave_c_h ave, clp(dash) clc(red) clw(medthick)), yscale(range(0,100)) ylab(#6) ytit("") xtit("True scores", size(medlarge)) legend(order (4 5) label(4 "Low-Performing") label (5 "High-Performing") bplace(se) ring(0)) aspect(1) title("Figure 5b with ex-ante Performance Heterogeneity for the Control Group", span size(medium))
	graph export "$output_gph/Performance_heterogeneity_5b_c.png", replace 
	
		putpdf paragraph
		putpdf image "$output_gph/Performance_heterogeneity_5b_c.png", width(15cm)
		
	* Graphing graph 5b' for treatment with ex-ante performance heterogeneity
		twoway (lpolyci b_ave_t_h ave, clp(dash)  fcolor(gs13)) (lpolyci b_ave_t_l ave, clc(stblue) clw(medthick)) (lpoly b_ave_t_h ave, clp(dash) clc(red) clw(medthick)), yscale(range(0,100)) ylab(#6) ytit("") xtit("True scores", size(medlarge)) legend(order (4 5) label(4 "Low-Performing") label (5 "High-Performing") bplace(se) ring(0)) aspect(1) title("Figure 5b' with ex-ante Performance Heterogeneity for the Treatment Group", span size(medium))
	graph export "$output_gph/Performance_heterogeneity_5b_t.png", replace 
	
		putpdf paragraph
		putpdf image "$output_gph/Performance_heterogeneity_5b_t.png", width(15cm)
		

	* Graphing graph 5d for control with ex-ante performance heterogeneity
		twoway (lpolyci u_ave_c_h ave, clp(dash)  fcolor(gs13)) (lpolyci u_ave_c_l ave, clc(stblue) clw(medthick)) (lpoly u_ave_c_h ave, clp(dash) clc(red) clw(medthick)), yscale(range(0,100)) ylab(#6) ytit("") xtit("True scores", size(medlarge)) legend(order (4 5) label(4 "Low-Performing") label (5 "High-Performing") bplace(se) ring(0)) aspect(1) title("Figure 5d with ex-ante Performance Heterogeneity for the Control Group", span size(medium))
		
	graph export "$output_gph/Performance_heterogeneity_5d_c.png", replace 
	
		putpdf paragraph
		putpdf image "$output_gph/Performance_heterogeneity_5d_c.png", width(15cm)
		
	* Graphing graph 5d' for treatment with ex-ante performance heterogeneity
		twoway (lpolyci u_ave_t_h ave, clp(dash)  fcolor(gs13)) (lpolyci u_ave_t_l ave, clc(stblue) clw(medthick)) (lpoly u_ave_t_h ave, clp(dash) clc(red) clw(medthick)), yscale(range(0,100)) ylab(#6) ytit("") xtit("True scores", size(medlarge)) legend(order (4 5) label(4 "Low-Performing") label (5 "High-Performing") bplace(se) ring(0)) aspect(1) title("Figure 5d' with ex-ante Performance Heterogeneity for the Treatment Group", span size(medium))
		graph export "$output_gph/Performance_heterogeneity_5d_t.png", replace 
		
		putpdf paragraph
		putpdf image "$output_gph/Performance_heterogeneity_5d_t.png", width(15cm)
					
***********************************************************
***********************************************************
*	Recreation of Table 1
***********************************************************
***********************************************************							

			* Load data
				use "$output_data/Ability_Clean_2.dta", clear
					egen hhid_tag=tag(hhid)
					gen one=1
			
			* Create a dummy
			gen overconf_ave = d_ave>0 if d_ave!=.
			* Create a dummy variable for "believed score lower than true score" 
			gen underestimate_ave = (d_ave < 0) if !missing(d_ave)

			* Replace neg's with missings 
			foreach var in exp_fees exp_uniforms exp_text exp_exbks exp_bks exp_tutoring exp_backpack exp_bks exp_exbks exp_text tot_bks_text tot_bks_all any_bks_text any_bks_all any_bks_text_s5 any_bks_all_s5 any_tutoring any_bks_text_tut any_exbks {
				replace `var'=. if `var'<0
			}
			
			* Any kid with tutoring
			bys hhid : egen any_taking_tutoring=max(taking_tutoring)


			* Set parameters
			local tabvar="treat" 
			local crit="one==1"
				
			* Setting up panels
			local panelA="primary_resp_fem resp_ed_decision age_par1 educ secondary lit farmer inc_resp_wk"  //  Respondent characteristics  
			local panelB="tot_kids one_par educ_ave any_secondary " //HH Characteristics
			local panelC="std age female attendance_sv tot_exp exp_fees exp_uniforms tot_exp_suppl any_suppl "  // Student info
			local panelD=" ave math engl chich math_engl " //  Child performance  
			local panelE="b_ave b_math b_engl b_chich b_math_engl sd_ave" //Beliefs 
			local panelF="dp_ave dp_math dp_engl dp_chich dp_math_engl dp_ave_12 d_ave overconf_ave wrong_12" //misperceptions
			local panelG="complementary"  //Beliefs about complementarity


	* Creating labels
		* Respondent Background
		label var primary_resp_fem "Female"
		label var resp_ed_decision "Primary education decision maker"
		label var age_par1 "Age"
		label var educ "Education (years)"
		label var secondary "Respondent has secondary education +"
		label var lit "Parent can read or write Chichewa"
		label var farmer "Respondent is farmer"
		label var inc_resp_wk "Respondent's weekly income"
		* Household Background
		label var tot_kids "Family size (Number of children)"
		label var one_par "One-parent household"
		label var educ_ave "Parents' average education (years)"
		label var any_secondary "Any parent has secondary education +"
		* Student Information
		label var std "Child's grade level"
		label var age "Child's age"
		label var female "Child is female"
		label var attendance_sv "Baseline attendance "
		label var tot_exp "Annual per-child education expenditures"
		label var exp_fees "\hspace{6 mm} Fees paid to schools"
		label var exp_uniforms "\hspace{6 mm} Uniform expense"
		label var tot_exp_suppl "\hspace{6 mm} School supplies, books, tutoring, etc."
		label var any_suppl "Any supplementary expenditures on child"
		* Academic Performance
		label var ave "Overall score"
		label var math "Math score"
		label var engl "English score"
		label var chich "Chichewa score"
		label var math_engl "(Math $-$ English) Score"
		* Respondent's Beliefs
		label var b_ave "Believed Overall Score"
		label var b_math "Believed Math Score"
		label var b_engl "Believed English Score"
		label var b_chich "Believed Chichewa Score"
		label var b_math_engl "Beliefs about (Math $-$ English) Score"
		label var sd_ave "SD of Individual Beliefs about Score"		
		* Gaps between believed and true performance
		label var dp_ave "Abs Val [Believed $-$ True Overall Score]"
		label var dp_math "Abs Val [Believed $-$ True Math Score]"
		label var dp_engl "Abs Val [Believed $-$ True English Score]"
		label var dp_chich "Abs Val [Believed $-$ True Chichewa Score]"
		label var dp_math_engl "Abs Val [Believed $-$ True (Math-English) Score]"
		label var dp_ave_12 "Abs Val [Believed $-$ True Overall Score (Child1-2)]"
		label var d_ave "Believed - True Overall Score"
		label var overconf_ave "Believed score higher than true score"
		label var wrong_12 "Wrong about who (child 1 or 2) is higher-scoring" 
		* Beliefs about Complementarity
		label var complementary "Believes educ. and achievement complementary"

		

cap file close sumstat
local file "$output_tbl/Tbl_Balance.tex"
di "file open sumstat using `file', write replace"
file open sumstat using `file', write replace
		
foreach tabnum in panelA panelB panelC panelD panelE panelF panelG{

	if "`_preso'"~="_preso" {

		if "`tabnum'"=="panelA" file write sumstat "\multicolumn{8}{@{}l}{\textbf{\emph{A. Respondent Background}}}    \\ \addlinespace[3pt]"  _n 
		if "`tabnum'"=="panelB" file write sumstat "\addlinespace[2pt] \multicolumn{8}{@{}l}{\textbf{\emph{B. Household Background}}}    \\ \addlinespace[3pt]"  _n 
		if "`tabnum'"=="panelC" file write sumstat "\addlinespace[2pt] \multicolumn{8}{@{}l}{\textbf{\emph{C. Student Information}}}    \\ \addlinespace[3pt]"  _n 
		if "`tabnum'"=="panelD" file write sumstat "\addlinespace[2pt] \multicolumn{8}{@{}l}{\textbf{\emph{D. Academic Performance (Average Achievement Scores)}}}    \\ \addlinespace[3pt]"  _n 
		if "`tabnum'"=="panelE" file write sumstat "\addlinespace[2pt] \multicolumn{8}{@{}l}{\textbf{\emph{E. Respondent's Beliefs about Child's Academic Performance}}}    \\ \addlinespace[3pt]"  _n 
		if "`tabnum'"=="panelF" file write sumstat "\addlinespace[2pt] \multicolumn{8}{@{}l}{\textbf{\emph{F. Gaps Between Believed and True Academic Performance}}}    \\ \addlinespace[3pt]"  _n 
		if "`tabnum'"=="panelG" file write sumstat "\addlinespace[2pt] \multicolumn{8}{@{}l}{\textbf{\emph{G. Beliefs about Complementarity}}}    \\ \addlinespace[3pt]"  _n 
			
		}	
			 
			foreach var of local `tabnum'{
			
				local varlab: variable label `var'		
				qui su `var' if `crit'
					local col1=`r(mean)'
					local col2=`r(sd)'

				qui su `var' if `tabvar'==0 & `crit'
					local col3=`r(mean)'
					local col4=`r(sd)'
	
				qui su `var' if `tabvar'==1 & `crit'
					local col5=`r(mean)'
					local col6=`r(sd)'
	
				display "qui xi: areg `var' treat ${controls} if `crit', robust cluster(hhid) absorb(school_code)"
				qui xi: areg `var' treat ${controls} if `crit', robust cluster(hhid) absorb(school_code)		
					qui test treat=0
					local col9=`r(p)'
					local col7=_b[treat]
					local col8=_se[treat]	
		
			foreach stat in col1 col2 col3 col4 col5 col6 col7 col8 col9 { // clean up locals
				local `stat'=round(``stat'',.01) // 2 decimal places
				if regexm(strofreal(``stat''),".") local `stat'=substr(strofreal(``stat''),1,strpos(strofreal(``stat''),".")+2) // issues with rounding
				if regexm(strofreal(``stat''),".") & abs(``stat'')<1 local `stat'=subinstr("``stat''",".","0.",1) // add a 0 for |x|<1
				if ``stat''>10 & ``stat''<=100{
				local `stat'=round(``stat'',0.1)			
				local `stat': di %12.1fc ``stat''
				}				
				if ``stat''>100{
				local `stat'=round(``stat'',1)			
				local `stat': di %12.0fc ``stat''
				}
			}
			
			file write sumstat "`varlab'& `col1' & `col2' & `col3' & `col5' & `col7' & `col8' & `col9'\\"  _n
			
	}
}

file write sumstat "\addlinespace[2pt] \multicolumn{6}{@{}l}{\textbf{\emph{Sample Sizes}}}    \\ \addlinespace[3pt]"  _n 

			qui: su one if `crit' & hhid_tag
				local col1=`r(N)'
			qui: su one if `crit' & hhid_tag & `tabvar'==0
				local col3=`r(N)'
			qui: su one if `crit' & hhid_tag & `tabvar'==1
				local col5=`r(N)'
			foreach stat in col1 col3 col5 { // clean up locals
				if ``stat''>10{
				local `stat'=round(``stat'',1)			
				local `stat': di %12.0fc ``stat''
				}
			}				
			file write sumstat "Sample Size--HHs& `col1' &  & `col3' & `col5' &  &  & \\"  _n
			
			qui: su one if `crit'
				local col1=`r(N)'
			qui: su one if `crit' & `tabvar'==0
				local col3=`r(N)'
			qui: su one if `crit' & `tabvar'==1
				local col5=`r(N)'
			foreach stat in col1 col3 col5 { // clean up locals
				if ``stat''>10{
				local `stat'=round(``stat'',1)			
				local `stat': di %12.0fc ``stat''
				}
			}					
			file write sumstat "Sample Size--Kids& `col1' &  & `col3' & `col5' &  &  & \\"  _n
file close sumstat 



***********************************************************
***********************************************************
*	Recreation of Table 2
***********************************************************
***********************************************************					
eststo clear
		
		* Set Parameters
		local controls="educ_ave_reg educ_ave_miss i.std female_reg female_miss primary_resp_fem i.perf_gap_used i.C1_strat " 
						
			
		* Run loop
		local append_replace="replace"
		local append_replace2="replace"
		foreach dataset in attendance wb_math wb_engl textbook  {

			use "$output_data/`dataset'_working.dta", clear

			
	* Column 1
	
			if "`dataset'"=="attendance" {
				local yvar="u_ave"
				local controls="educ_ave_reg educ_ave_miss i.std female_reg female_miss primary_resp_fem i.perf_gap_used i.C1_strat " 
				local absorb="school_code"
				local other_outreg="auto(2)"
				local crit="one==1"
			
				* Replace variables for outreg
				replace ds_=ds_ave
				replace ds_educ= ds_ave*educ_ave
				replace treat_ds_=treat_ds_ave
				replace treat_ds_educ=treat_ds_ave*educ_ave
				replace treat_educ_used=treat* educ_ave
				replace perf_=ave
				replace perf_educ=ave*educ_ave
				replace treat_perf=treat_ave
				replace treat_perf_educ=treat_ave*educ_ave	
			}

	* Column 2		
			
			if "`dataset'"=="wb_engl" {
				local yvar="wb_engl"
				local controls="educ_ave_reg educ_ave_miss i.std female_reg female_miss primary_resp_fem i.perf_gap_used i.C1_strat " 
				local absorb="school_code"
				local other_outreg="auto(2)"
				local crit="one==1"
			
				* Replace variables for outreg
				replace ds_=ds_engl
				replace ds_educ= ds_engl*educ_ave
				replace treat_ds_=treat_ds_engl
				replace treat_ds_educ=treat_ds_engl*educ_ave
				replace treat_educ_used=treat*educ_ave
				replace perf_=engl
				replace perf_educ=engl*educ_ave
				replace treat_perf=treat_engl
				replace treat_perf_educ=treat_engl*educ_ave				
				
				qui su wb_engl
				if `r(max)'==1 {
					replace wb_engl = wb_engl*100 
					}
			}
			
	* Column 3
	
			if "`dataset'"=="wb_math" {
				local yvar="wb_math"
				local controls="educ_ave_reg educ_ave_miss i.std female_reg female_miss primary_resp_fem i.perf_gap_used i.C1_strat " 
				local absorb="school_code"
				local other_outreg="auto(2)"
				local crit="one==1"
			
				* Replace variables for outreg
				replace ds_=ds_math
				replace ds_educ= ds_math*educ_ave
				replace treat_ds_=treat_ds_math
				replace treat_ds_educ=treat_ds_math*educ_ave
				replace treat_educ_used=treat*educ_ave
				replace perf_=math
				replace perf_educ=math*educ_ave
				replace treat_perf=treat_math
				replace treat_perf_educ=treat_math*educ_ave	
				
				qui su wb_math
				if `r(max)'==1 {
					replace wb_math = wb_math*100 
					}
			}
			
	* Column 4
	
			if "`dataset'"=="textbook" {
				* Set parameters
				local yvar="ln10p_wtp_math_engl"
				local math_engl = "engl_math" 
				local controls="educ_ave_reg educ_ave_miss i.std female_reg female_miss primary_resp_fem i.perf_gap_used i.C1_strat" 
				local absorb="one"
				local other_outreg="auto(2)"
				local crit="one==1"
			
				* Replace variables for outreg
				replace ln10p_wtp_engl_math = ln10p_wtp_engl- ln10p_wtp_math
				foreach prefix in "" ds_ treat_ treat_ds_ {
					gen `prefix'engl_math = -`prefix'math_engl
					}
					
				replace ds_=ds_`math_engl'
				replace ds_educ= ds_`math_engl'*educ_ave
				replace treat_ds_=treat_ds_`math_engl'
				replace treat_ds_educ=treat_ds_`math_engl'*educ_ave
				replace treat_educ_used=treat*educ_ave
				replace perf_=`math_engl'
				replace perf_educ=`math_engl'*educ_ave
				replace treat_perf=treat_`math_engl'
				replace treat_perf_educ=treat_`math_engl'*educ_ave	
					
			}
			
				* Labelling
				label var treat_perf "Treat $\times$ Score"
				label var perf_ "Score"	


	* Storing each column
				eststo: xi : areg `yvar' treat_perf treat perf_ `controls' if one==1 & `crit', robust cluster(hhid) absorb(`absorb')


}

	* Column 5
	
		
			use "$output_data/Ability_Clean_2.dta", clear
				cap gen one=1

				rename (treat_ave ave) (treat_perf perf_) // Rename vars so it matches the names from previous tables
				local crit="one==1" //"clean==1"
				
				eststo: areg tix treat_perf perf_ high_perf_sib `controls' if `crit', a(hhid) 

				label var treat_perf "Treat $\times$ Score"
				label var perf_ "Score"	
			
	* Exporting
	
			esttab using "$output_tbl/Table2", ///
				b(a2) se booktabs nostar fragment nonotes nomtitles nonumbers nolines brackets replace label style(tex) gaps ///
				keep(treat_perf  perf_ treat) order(treat_perf perf_ treat ) stats(N r2, labels("Observations" "R-squared") fmt(%9.0fc %9.3fc))
					 
***********************************************************
***********************************************************	
*	Ex Ante High and Low Beliefs Modification
***********************************************************
***********************************************************
		eststo clear	
		* Set Parameters
		local controls="educ_ave_reg educ_ave_miss i.std female_reg female_miss primary_resp_fem i.perf_gap_used i.C1_strat " 
		
		* Run loop
		local append_replace="replace"
		local append_replace2="replace"
		foreach dataset in attendance wb_math wb_engl textbook  {

			use "$output_data/`dataset'_working.dta", clear

			

	* Adding beliefs
	* Setting up high and low performance dummies for variables of interest
	
	
				egen mean_b_ave = mean(b_ave)
				egen mean_b_math = mean(b_math)
				egen mean_b_engl = mean(b_engl)
				egen mean_b_math_engl = mean(b_math_engl)
				gen hb_ave = (b_ave>mean_b_ave) //high beliefs on the overall score
				gen hb_math = (b_math>mean_b_math) //high beliefs on the maths score
				gen hb_engl = (b_engl>mean_b_engl) //high beliefs on the english score
				gen hb_diff = (b_math_engl<mean_b_math_engl) //high beliefs on the english - math score		
				label define hb_ave 0 "Low-Belief" 1 "High-Belief"
				label define hb_math 0 "Low-Belief" 1 "High-Belief"
				label define hb_engl 0 "Low-Belief" 1 "High-Belief"
				label define hb_diff 0 "Low-Belief" 1 "High-Belief"
			
	* Column 1
			if "`dataset'"=="attendance" {
				local yvar="u_ave"
				local controls="educ_ave_reg educ_ave_miss i.std female_reg female_miss primary_resp_fem i.perf_gap_used i.C1_strat " 
				local absorb="school_code"
				local other_outreg="auto(2)"
				local crit="one==1"
		
				* Replace variables for outreg
				replace ds_=ds_ave
				replace ds_educ= ds_ave*educ_ave
				replace treat_ds_=treat_ds_ave
				replace treat_ds_educ=treat_ds_ave*educ_ave
				replace treat_educ_used=treat* educ_ave
				replace perf_=ave
				replace perf_educ=ave*educ_ave
				replace treat_perf=treat_ave
				replace treat_perf_educ=treat_ave*educ_ave	
			
				* Adding beliefs
				gen treat_perf_belief=treat_ave*hb_ave
				gen treat_belief=treat*hb_ave
				gen perf_belief=ave*hb_ave
				gen belief=hb_ave

			}

	* Column 2		
				
			if "`dataset'"=="wb_math" {
				local yvar="wb_math"
				local controls="educ_ave_reg educ_ave_miss i.std female_reg female_miss primary_resp_fem i.perf_gap_used i.C1_strat " 
				local absorb="school_code"
				local other_outreg="auto(2)"
				local crit="one==1"
			
				* Replace variables for outreg
				replace ds_=ds_math
				replace ds_educ= ds_math*educ_ave
				replace treat_ds_=treat_ds_math
				replace treat_ds_educ=treat_ds_math*educ_ave
				replace treat_educ_used=treat*educ_ave
				replace perf_=math
				replace perf_educ=math*educ_ave
				replace treat_perf=treat_math
				replace treat_perf_educ=treat_math*educ_ave	
			
				* Adding beliefs
				gen treat_perf_belief=treat_ave*hb_ave
				gen treat_belief=treat*hb_ave
				gen perf_belief=ave*hb_ave
				gen belief=hb_ave
				
				qui su wb_math
				if `r(max)'==1 {
					replace wb_math = wb_math*100 
					}
			}
			
			
	* Column 3
	
			if "`dataset'"=="wb_engl" {
				local yvar="wb_engl"
				local controls="educ_ave_reg educ_ave_miss i.std female_reg female_miss primary_resp_fem i.perf_gap_used i.C1_strat " 
				local absorb="school_code"
				local other_outreg="auto(2)"
				local crit="one==1"
			
				* Replace variables for outreg
				replace ds_=ds_engl
				replace ds_educ= ds_engl*educ_ave
				replace treat_ds_=treat_ds_engl
				replace treat_ds_educ=treat_ds_engl*educ_ave
				replace treat_educ_used=treat*educ_ave
				replace perf_=engl
				replace perf_educ=engl*educ_ave
				replace treat_perf=treat_engl
				replace treat_perf_educ=treat_engl*educ_ave				

				* Adding beliefs
				gen treat_perf_belief=treat_ave*hb_ave
				gen treat_belief=treat*hb_ave
				gen perf_belief=ave*hb_ave
				gen belief=hb_ave
				
				qui su wb_engl
				if `r(max)'==1 {
					replace wb_engl = wb_engl*100 
					}
			}
			
	* Column 4
	
			if "`dataset'"=="textbook" {
				* Set parameters
				local yvar="ln10p_wtp_math_engl"
				local math_engl = "engl_math" 
				local controls="educ_ave_reg educ_ave_miss i.std female_reg female_miss primary_resp_fem i.perf_gap_used i.C1_strat" 
				local absorb="one"
				local other_outreg="auto(2)"
				local crit="one==1"
			
				* Replace variables for outreg
				replace ln10p_wtp_engl_math = ln10p_wtp_engl- ln10p_wtp_math
				foreach prefix in "" ds_ treat_ treat_ds_ {
					gen `prefix'engl_math = -`prefix'math_engl
					}
				replace ds_=ds_`math_engl'
				replace ds_educ= ds_`math_engl'*educ_ave
				replace treat_ds_=treat_ds_`math_engl'
				replace treat_ds_educ=treat_ds_`math_engl'*educ_ave
				replace treat_educ_used=treat*educ_ave
				replace perf_=`math_engl'
				replace perf_educ=`math_engl'*educ_ave
				replace treat_perf=treat_`math_engl'
				replace treat_perf_educ=treat_`math_engl'*educ_ave	
				
				* Adding beliefs
				gen treat_perf_belief=treat_ave*hb_diff
				gen treat_belief=treat*hb_diff
				gen perf_belief=ave*hb_diff
				gen belief=hb_diff
				
			}
	
				* Labelling
				label var treat_perf_belief "Treat $\times$ Score $\times$ Beliefs"
				label var treat_perf "Treat $\times$ Score"
				label var treat_belief "Treat $\times$ Beliefs"
				label var perf_belief "Score $\times$ Beliefs"
				label var perf_ "Score"	
				label var belief "Beliefs"

	* Storing each column
			eststo: xi : areg `yvar' treat_perf_belief treat_perf treat_belief perf_belief treat perf_ belief `controls' if one==1 & `crit', robust cluster(hhid) absorb(`absorb')

		}
		
	* Column 5
	
		
			use "$output_data/Ability_Clean_2.dta", clear
				cap gen one=1

				* Adding beliefs
				egen mean_b_ave = mean(b_ave)
				egen mean_b_math = mean(b_math)
				egen mean_b_engl = mean(b_engl)
				egen mean_b_math_engl = mean(b_math_engl)
				gen hb_ave = (b_ave>mean_b_ave) //high beliefs on the overall score
				gen hb_math = (b_math>mean_b_math) //high beliefs on the maths score
				gen hb_engl = (b_engl>mean_b_engl) //high beliefs on the english score
				gen hb_diff = (b_math_engl<mean_b_math_engl) //high beliefs on the english - math score		
				label define hb_ave 0 "Low-Belief" 1 "High-Belief"
				label define hb_math 0 "Low-Belief" 1 "High-Belief"
				label define hb_engl 0 "Low-Belief" 1 "High-Belief"
				label define hb_diff 0 "Low-Belief" 1 "High-Belief"
				gen treat_perf_belief=treat_ave*hb_ave
				gen treat_belief=treat*hb_ave
				gen perf_belief=ave*hb_ave
				gen belief=hb_ave
				
				rename (treat_ave ave) (treat_perf perf_) // Rename vars so it matches
				local crit="one==1" //"clean==1"
				
				eststo: areg tix treat_perf_belief treat_perf treat_belief perf_belief perf_ belief high_perf_sib `controls' if `crit', a(hhid) 

				* Labelling
				label var treat_perf_belief "Treat $\times$ Score $\times$ Beliefs"
				label var treat_perf "Treat $\times$ Score"
				label var treat_belief "Treat $\times$ Beliefs"
				label var perf_belief "Score $\times$ Beliefs"
				label var perf_ "Score"	
				label var belief "Beliefs"
			
	* Exporting
	
			esttab using "$output_tbl/Table2_extension", ///
				b(a2) se booktabs nostar fragment nonotes nomtitles nonumbers nolines brackets replace label style(tex) gaps ///
				keep(treat_perf_belief treat_perf treat_belief perf_belief treat perf_ belief) order(treat_perf_belief treat_perf treat_belief perf_belief treat perf_ belief) stats(N r2, labels("Observations" "R-squared") fmt(%9.0fc %9.3fc))
							 

***********************************************************
***********************************************************
*	Recreation of Table 4
***********************************************************
***********************************************************	
		
eststo clear		

		* Panel A: Attenuation of Beliefs			
				
					eststo clear
					use "$output_data/Ability_Clean_2.dta", clear
					egen hhid_tag=tag(hhid)
					gen one=1
					
					gen score_used = .
					gen score_used_educ = .
					gen educ_used = .
				
			* Set Parameters
				local subject="math"
				local educ="educ_ave"
				local controls=""
				local sample="one==1"
				local absorb = "one"
				local crit = "one==1"
			
			* Run loop
			local append_replace="replace"
			local append_replace2="replace"

			
			foreach type in ave math engl chich math_engl ave_12 {
				replace score_used = `type'
				replace score_used_educ = score_used*educ_ave
				replace educ_used = educ_ave
			
			
			
			* Full sample
			* Base table
					eststo: xi : areg b_`type' score_used_educ score_used educ_used `controls' if one==1 & `crit', robust cluster(hhid) absorb(`absorb')

			} //dataset	

			
			label var score_used "Score"
			label var score_used_educ "Score $\times$ Parents yrs of educ."
			label var educ_used "Parent yrs of educ."
			
	
			esttab using "$output_tbl/Tbl_4_panel_a", ///
				b(a2) se booktabs nostar fragment nonotes nomtitles nonumbers nolines brackets replace label style(tex) gaps ///
				keep(score_used_educ score_used educ_used) order(score_used_educ score_used educ_used) ///
				stats(N, labels("\addlinespace[2pt] Observations") fmt(%9.0fc))
			eststo clear						 
	
		**********************************************************
		* Panel B: Selected Experimental Outcomes: 
		*		   Heterogneneity in treatment effect on slope, by parent education
		*********************************************************
					
			** Set Parameters **
				local subject="math"
				local educ="educ_ave" 
				local controls="educ_ave_reg educ_ave_miss i.std female_reg female_miss primary_resp_fem i.perf_gap_used i.C1_strat " 
				local sample="one==1"
				local educ_ave_hi_lbl0="Below Median Parent Education"
				local educ_ave_hi_lbl1="Above Median Parent Education"
				local secondary_lbl0="Below Secondary Educ"
				local secondary_lbl1="Above Secondary Educ"
				local any_secondary_lbl0="No Parent with"
				local any_secondary_lbl0_2="Secondary Educ"
				local any_secondary_lbl1="Any Parent with"
				local any_secondary_lbl1_2="Secondary Educ"
				local mathlbl="Math Score"
				local engllbl="English Score"			
			
			** Set up outreg files
				use "$output_data/lottery_working.dta", clear

			
			
			*** Loop over datasets and make table: Original Heterogeneity Table
			
			eststo clear
			
			foreach dataset in wb_math wb_engl  {
			
				use "$output_data/`dataset'_working.dta", clear

					local crit="one==1"
			
				

				if "`dataset'"=="wb_math" {
					local yvar="wb_math"
					local controls="educ_ave_reg educ_ave_miss i.std female_reg female_miss primary_resp_fem i.perf_gap_used i.C1_strat" 
					local absorb="school_code"
					local other_outreg="auto(2)"
					local column_lab "Math"
					local perf = "math"

			
					* Replace variables for outreg
					replace ds_=ds_math
					replace ds_educ= ds_math*educ_ave
					replace treat_ds_=treat_ds_math
					replace treat_ds_educ=treat_ds_math*educ_ave
					replace treat_educ_used=treat*educ_ave
					replace perf_=math
					replace perf_educ=math*educ_ave
					replace treat_perf=treat_math
					replace treat_perf_educ=treat_math*educ_ave	
					qui su wb_math
					if `r(max)'==1 {
						replace wb_math = wb_math*100 
						}			
			
				}


				if "`dataset'"=="wb_engl" {
					local yvar="wb_engl"
					local controls="educ_ave_reg educ_ave_miss i.std female_reg female_miss primary_resp_fem i.perf_gap_used i.C1_strat " 
					local absorb="school_code"
					local other_outreg="auto(2)"
					local column_lab "English"
					local perf = "engl"
			
					* Replace variables for outreg
					replace ds_=ds_engl
					replace ds_educ= ds_engl*educ_ave
					replace treat_ds_=treat_ds_engl
					replace treat_ds_educ=treat_ds_engl*educ_ave
					replace treat_educ_used=treat*educ_ave
					replace perf_=engl
					replace perf_educ=engl*educ_ave
					replace treat_perf=treat_engl
					replace treat_perf_educ=treat_engl*educ_ave				
					qui su wb_engl
					if `r(max)'==1 {
						replace wb_engl = wb_engl*100 
						}			
			
				}

			
			* Table for heterogeneity on performance -- Heterogeneity by Parents' education

				* Heterogeneity by parent educ in treat*score spec

					di "`dataset'"
					
				
				
					eststo: xi : areg `yvar' treat_perf treat_perf_educ treat treat_educ_used perf_ perf_educ educ_ave ///
						   `controls' if one==1 & `crit', robust cluster(hhid) absorb(`absorb')
					estadd local score_used "`column_lab'"
					
						   qui test treat_perf_educ
							if `r(p)'<0.0001 local p1: di %7.3e `r(p)'
							else local p1: di %7.3fc `r(p)'
							estadd local p_val1 "`p1'"
							di "p_val1 `p1'"
							
						   qui test treat_educ
						   	local p2=`r(p)'
						   	
						   	qui test treat_perf_educ+perf_educ=0
						   		local p3=`r(p)'

						   	qui test treat_educ_used+educ_ave=0
						   		local p4=`r(p)'
								
							local perf_educ_in_treat = _b[treat_perf_educ]+_b[perf_educ]
							
					estadd local perf_educ_in_treat `perf_educ_in_treat'
					estadd local pval_perf_educ_in_treat `p3'

			
					
					
			} //datasets
			
			
				
					
					
			** U ave 		

				use "$output_data/wb_math_working.dta", clear
			
					local yvar="u_ave"
					local controls="educ_ave_reg educ_ave_miss i.std female_reg female_miss primary_resp_fem i.perf_gap_used i.C1_strat" 
					local absorb="school_code"
					local other_outreg="auto(2)"
					local column_lab "Overall"
					local perf = "ave"
			
					* Replace variables for outreg
					replace ds_=ds_`perf'
					replace ds_educ= ds_`perf'*educ_ave
					replace treat_ds_=treat_ds_`perf'
					replace treat_ds_educ=treat_ds_`perf'*educ_ave
					replace treat_educ_used=treat*educ_ave
					replace perf_=`perf'
					replace perf_educ=`perf'*educ_ave
					replace treat_perf=treat_`perf'
					replace treat_perf_educ=treat_`perf'*educ_ave				
			
			
					* Replace variables for outreg
					replace perf_=ave
					replace perf_educ=perf_*educ_ave
					replace treat_perf=treat*perf_
					replace treat_perf_educ=treat*perf_*educ_ave
					replace treat_educ_used=treat*educ_ave


					eststo: xi : areg `yvar' treat_perf treat_perf_educ treat treat_educ_used perf_ perf_educ educ_ave ///
						   `controls' if one==1 & `crit', robust cluster(hhid) absorb(`absorb')
					estadd local score_used "`column_lab'"
			
						   qui test treat_perf_educ
							if `r(p)'<0.0001 local p1: di %7.3e `r(p)'
							else local p1: di %7.3fc `r(p)'
							estadd local p_val1 "`p1'"
							di "p_val1 `p1'"	
							
					eststo: xi: areg `yvar' treat treat_educ_used `perf' educ_ave `controls' if one==1 & `crit', robust cluster(hhid) absorb(`absorb')										
					estadd local score_used "`column_lab'"	
			
			** Make table
			
			
			label var treat_perf "Treat $\times$ Score"
			label var treat_perf_educ "Treat $\times$ Score $\times$ Parent yrs of educ."
			label var perf_ "Score"
			label var perf_educ "Score $\times$ Parent yrs of educ."
			label var treat "Treat"
			label var treat_educ_used "Treat $\times$ Parent yrs of educ."
			label var educ_ave "Parent yrs of educ."
			
		
			
				esttab est3 est1 est2 using "$output_tbl/Tbl_4_panel_b", ///
					b(a2) se booktabs nostar fragment nonotes nomtitles nonumbers nolines brackets replace label style(tex) gaps ///
					keep(treat_perf_educ treat_perf perf_educ perf_ treat_educ_used treat educ_ave) order(treat_perf_educ treat_perf perf_educ perf_ treat_educ_used treat educ_ave) ///
					stats(N r2 p_val1 score_used, labels("\addlinespace[2pt] Observations" "R-squared" "P-val: Treat $\times$ Score $\times$ Yrs.Educ.=0" "\\ Score Used") fmt(%9.0fc %9.3fc %20s %20s))

			

**********************************************
* Save the final PDF
			
putpdf save "Output_Figures.pdf", replace
