
proc format;
value genderx 1='Men' 2='Women';
value raceethx 1='Mexican American' 2='other Hispanic' 3='non-Hispanic White' 4= 'non-Hispaic Black' 5= 'other race';
run;

/* BPA analysis*/
proc import dbms = xls file = 'C:\Users\jmbarkoski\Dropbox\GitHub programs\practice datasets\bpa2_PracticeDataSet.xls'
	out=BPA
	replace; 
	getnames=yes;
run;

proc contents data=BPA;
run;

/* distribution of urinary BPA*/
proc univariate data=BPA;
var ubpa;
histogram;
run;

/* natural log trasform urinary BPA concentrations*/
data bpa;
set bpa;
if ubpa ne . then ln_BPA = log(ubpa);
run;

proc univariate data=bpa;
format gender genderx.;
  class gender;
  var hdl;     
  histogram hdl / nrows=2 odstitle="HDL by gender";
  ods select histogram; 
run;

/* distribution of hdl*/
proc univariate data=bpa;
var hdl;
histogram;
run;

/* distributions of BPA */
proc sort data=bpa;
by gender; /* where 1=male, 2=female*/
run;

proc boxplot data =bpa;
format gender genderx.;
  plot ln_bpa*gender/ boxstyle = schematic 
clipsymbol  = dot
clipfactor =2
height=3
;
run;

proc sort data=bpa;
by raceeth; /* where 1=male, 2=female*/
run;

proc boxplot data =bpa;
format raceeth raceethx.;
  plot ln_bpa*raceeth/ boxstyle = schematic 
clipsymbol  = dot
clipfactor =2
height=3
;
run;


/*scatter plot*/
SYMBOL1 V=circle C=blue I=r;
TITLE 'urinary BPA concentrations in relation to age ';
PROC GPLOT DATA=bpa;
     PLOT ln_BPA*age ;
RUN;
QUIT; 

/* bivariate associations*/
proc freq data = bpa;
format raceeth raceethx. gender genderx.;
tables gender*raceeth / chisq norow nocol ;
run;

/* mann whitney test*/
proc sort data=bpa;
by gender; run; 

proc npar1way data = bpa wilcoxon;
format gender genderx.;
  class gender;
  var ln_BPA;
run;

/* Regression analysis*/

/* crude model of BPA on HDL*/

proc surveyreg data=bpa ;
model hdl = ln_bpa ucr gender bmi smk;
weight WTMEC2YR;
run;

/* stratified by gender*/
proc surveyreg data=bpa;
model hdl = ln_bpa ucr bmi smk;
weight WTMEC2YR;
where gender=1;
run;

proc surveyreg data=bpa;
model hdl = ln_bpa ucr bmi smk;
weight WTMEC2YR;
where gender=2;
run;
