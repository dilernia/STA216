%macro forestPlot(dataSet, response, explanatory, phAdjust = Tukey);

/* Code obtained from: https://support.sas.com/resources/papers/proceedings14/1902-2014.pdf*/

/* Arguments
dataSet: SAS data set 
response: quantitative response variable
explanatory: categorical explanatory variable
phAdjust: post-hoc analysis adjustment method for multiple comparisons. Default is tukey.
*/

/* Fitting One-Way ANOVA Model */ 
ods select none;
ODS OUTPUT diffs=dfs(KEEP= &explanatory _&explanatory estimate adjlower adjupper adjp);
PROC GLIMMIX DATA=&dataSet order=internal;
	CLASS &explanatory;
	model &response = &explanatory / solution; 
	LSMEANS &explanatory / cl diff adjust = &phAdjust ;
run;
ods output close;
quit;

* Forest Plot of Differences ;
* Add a text variable called label describing each comparison;
DATA dfs2(replace = yes);
	SET dfs(rename =( estimate=mndif1 adjlower=adjlower1 adjupper=adjupper1));
	DROP ptxt;
	LENGTH label $100 ptxt $4;
	IF adjp < .001 then ptxt=', p'; else ptxt=', p=';
	label = CAT(put( &explanatory, ct.),' vs ',
 	put( _&explanatory, ct.),ptxt,STRIP(put(adjp,pvalue6.3)));
* new variables for non-significant differences, set existing to missing;
IF (adjp GE 0.05) then do;
 mndif2 = mndif1; adjlower2 = adjlower1; adjupper2 =adjupper1;
 mndif1 = . ; adjlower1 = . ; adjupper1 = . ;
end;
maxUpper = max(adjupper1, adjupper2);
minLower = min(adjlower1, adjlower2);
RUN;

/* Calculating axis limits dynamically */
proc sql; 
create table minMaxLimits as(select*, 
max(maxUpper) as maxLim,
min(minLower) as minLim
from dfs2); 
run;

data _NULL_;
	set minMaxLimits;
	CALL SYMPUT('maxLimit', max(maxLim, 0));
	CALL SYMPUT('minLimit', min(minLim, 0));
run;

ods select all;
PROC SGPLOT DATA=dfs2 noautolegend ;
REFLINE 0 / axis=x lineattrs=(color=black pattern=2 thickness=1) transparency=0;
SCATTER y=label x=mndif1 / xerrorlower=adjlower1 xerrorupper=adjupper1
 errorbarattrs=(color=black pattern=1 thickness=2)
 markerattrs=(color=black symbol= circlefilled size=6)
 datalabel=mndif1 datalabelattrs=(color=black weight=bold size=7);
/* print the differences at the center of each confidence interval */
SCATTER y=label x=mndif2 / xerrorlower=adjlower2 xerrorupper=adjupper2
 errorbarattrs=(color=black pattern=2 thickness=2)
 markerattrs=(color=black symbol= circlefilled size=6)
 datalabel=mndif2 datalabelattrs=(color=black weight=bold size=7);
XAXIS offsetmin=0.05 offsetmax=0.05 label="Difference in Average &response" 
MAX = &maxLimit MIN = &minLimit;
YAXIS offsetmin=0.12 offsetmax=0.12 display=(Noticks) reverse 
label = "&explanatory Comparisons";
FORMAT mndif1 mndif2 5.1;
TITLE "Differences and &phAdjust Adjusted 95% Confidence Intervals";
RUN;

%mend forestPlot;
