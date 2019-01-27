new; 
cls;

output file=Mason_&_Myers_GAUSS_output.out reset;
format /m1 /rd 12,6;


print "Linear & threshold VARs for";
"Market variables: Choma, Lusaka, & SAFEX wholesale prices (ZMK/kg)";
"          Mchinji retail price (ZMK/kg)";
"Policy variables: FRA buy and sell price premia (BPP, SPP, ZMK/kg)";
"Candidate threshold variables:"; 
"	(i) FRA smallholder market share (%, t-1) (mktshare),";
"	(ii) Smallholder marketable surplus remaining after FRA purchases (kg/capita, t-1),";
"	(iii) Smallholder maize quantity harvested(kg/capita),";
"	(iv) Structural break (time).";
"GAUSS TVAR code adapted from Galvao(2006)";
"GAUSS code for multivariate Hansen tests adapted from Hansen (1996), Lo & Zivot (2001),";
"		and Hansen & Seo (2002).";
"Code written & adapted by Nicole Mason, July 2011";
"Monthly data, July 1996 through December 2008";
" ";
" ";
"**************************************************************************";

/*Load data*/
load z[150,10]=Mason_&_Myers_dataset_for_GAUSS.csv;

nn=rows(z); /*maximum number of observations*/

/*col1=date, col2=CHOMA, col3=LUSAKA, col4=SAFEX, col5=MCHINJI  */
/*col6=BPP_CHOMA, col7=SPP_LUSAKA,*/ 
/*col8=mktshare, col9=msleftpc, col10=aprodpc*/
/*date 438 is July 1996*/

/*Number of endogenous variables*/ l=6;
/*Autoregressive order of VAR from Stata*/ phat=3;
/*Set seed for random number generator*/ rndseed 3456;

/*organising data*/
yi=z[1:nn,2:7]; @ yi are endog variables -- rows 1 to nn, columns 2 to 6, of dataset @
ye=yi[1+phat:nn,.]; @ observations of y used in analysis (lose 1st phat due to lags) @

/*Min number of obs per regime*/ minobs=round(((phat*l)+1)+rows(ye)*.20); @ # of parameters + 20% of obs @

"Number of endogenous variables:" l;
"Autoregressive order of VAR from Stata:" phat;
"Number of observations before lagging endog. vars.:" nn;
"Min number of obs per regime:" minobs;

@ q's are the candidate threshold variables - cols 7 to 10 of dataset @

qms=z[1:nn,8];  @ mktshare @
qmse=qms[1+phat:nn,.]; 

qmp=z[1:nn,9]; @ msleftpc @
qmpe=qmp[1+phat:nn,.]; 

qap=z[1:nn,10]; @ aprodpc @
qape=qap[1+phat:nn,.]; 

/* Create time trend vector to use for structural break testing in TVAR command.
Galvao has separate code for SBVAR but easier to do Hansen test if just treat time
like the other threshold variables */

qte=seqa(1,1,rows(ye));


/*Define xmatrix - creates matrix of constant and lagged endog vars*/
proc(1)=xmatrix(y, p);  
local x, xx, i, j, t; 
t=rows(y); 
x=ones(t-phat, 1);
i=1; do while i<=cols(y); 
xx=y[.,i];
j=1; do while j<=p; 
x=x~xx[1+p-j:t-j];
j=j+1; endo; i=i+1;endo;
retp(x); 
endp; 

/*organising data (cont.)*/
xe=xmatrix(yi,phat); @ 1, y1(t-1), ..., y1(t-phat), ..., yL(t-1), ..., yL(t-phat) @
nn=rows(ye);

/***************************************************************************************/
/************************************** LINEAR VAR *************************************/
b_var=invpd(xe'xe)*(xe'ye); @ Estimate linear VAR @
e_var=ye-(xe*b_var); @ Get residuals @
sigvar=(e_var'e_var)./rows(e_var); @ Residual covariance matrix @
llvar1=-(rows(e_var)*l/2)*ln(2*pi)+(rows(e_var)/2)*ln(det(invpd(sigvar)))-(rows(e_var)*l/2); 
ssrvar1=sumc(diag(e_var'e_var));
" ";
" ";
"*************************** LINEAR VAR RESULTS ***************************";
"Unique regime with # of obs.:" rows(e_var);
"Number of parameters estimated:" l*(phat*l+1);
"Log likelihood:"  llvar1;
"SSR:" ssrvar1;
"Matrix of RF VAR coefficients:" b_var;
"Residual covariance matrix:" sigvar;
" ";
" ";
/***************************************************************************************/
/***************************************************************************************/



/*Define gpbic2 - GP BIC in terms of LL's for VAR application*/
proc(1)=gpbic2(llr, llu, t, m); 
local gp2;
/* where llr and llu are the log likelihoods for the restricted and unrestricted models,
t is the number of observations, and m is the number of threshold parameters to be estimated */
gp2=(-2/t)*(llr-llu)-(ln(t)/t)*l*(phat*l+1)*m;
retp(gp2);
endp;


/*Define threestc- finds threshold level <ghat> that min ln det of the resid cov matrix*/
proc(2)=threestc (y, x, q, minobs, rssr); @ rssr is the SSR for the restricted model @
local t, qi, qq, qqs, qqn, s, qqd, du, xl, xh, yl, yh, el, eh; 
local ghat, shat;  
local sigtvar;
t=rows(y); 
qi=sortc(q[.,1],1); 
qq=qi[minobs:t-minobs]; 
qqn=rows(qq); 
s=zeros(qqn,1); 
for i(1, qqn, 1);
qqd=qq[i];
du=(q.<=qqd);  
xl=selif(x, du); xh=selif(x, (1-du));
yl=selif(y, du); yh=selif(y, (1-du));
el=yl-xl*invpd(xl'xl)*(xl'yl); 
eh=yh-xh*invpd(xh'xh)*(xh'yh);
sigtvar=((el'el)+(eh'eh))/(rows(el)+rows(eh));
s[i]=ln(det(sigtvar));
endfor; 
ghat=qq[minindc(s)]; @ optimal threshold level in 2-regime TVAR @
shat=s[minindc(s)]; @ shat is the min weighted ave ln det of the resid cov matrix @
retp(ghat, shat);
endp; 



/*Define twotest - estimates TVAR conditional on the optimal threshold level*/
proc(4) = twotest(y, x, q, r);
local d, el, eh, xl, yl, xh, yh, xxl, xxh, bl, bh;
d=(q.<=r); 
xl=selif(x, d); xh=selif(x, (1-d)); @ xl is rows of x w/ q values <=threshold; q> threshold for xh @
yl=selif(y, d); yh=selif(y, (1-d));
xxl=invpd(xl'xl); xxh=invpd(xh'xh); @ estimate VARs for xl and xh obs @
bl=xxl*(xl'yl); bh=xxh*(xh'yh); @ VAR RF coeff. @
el=yl-xl*bl; eh=yh-xh*bh; @ VAR RF residuals @
retp (bl, bh, el, eh); 
endp;


/***************************************************************************************/
/********* DEFINE PROCEDURES FOR MULTIVARIATE LR STAT & COMPUTE HANSEN P-VALUES ********/ 
/****************** FOR TESTING H0: 1 regime VAR vs. H1: 2-regime VAR ******************/
/** CODE ADAPTED FROM LO & ZIVOT (2001) & HANSEN & SEO (2002) BY NICOLE MASON, 4/2011 **/

/*suplrtvar: estimates linear & 2-regime threshold VARs, computes & returns supLR stat*/
/*Analogous to _vtarboot3.prc in Lo & Zivot */
proc (1)=suplrtvar(y,qe, minobs, rssr); 
@ y is matrix of endog vars before lagging @ 
@ qe is threshold variable without 1st phat obs (if qe not one of endog vars) @
local ye1, xe1, e, sighat1, lndet1, d, r, s, bl, bh, el, eh;
local sigtvar, lndet2, supLR12;
@ organize data, estimate linear VAR, compute ln(det(sigma)) @
ye1=y[1+phat:rows(y),.];
xe1=xmatrix(y,phat);
e=ye1-xe1*(invpd(xe1'xe1)*(xe1'ye1));
sighat1=(e'e)./rows(e);
lndet1=ln(det(sighat1));
@ estimate TVAR, compute ln(det(sigma)), and supLR stat @
{r, s}=threestc(ye1, xe1, qe, minobs, rssr); @ find optimal threshold level @
{bl, bh, el, eh}=twotest(ye1, xe1, qe, r); 
sigtvar=((el'el)+(eh'eh))/(rows(el)+rows(eh));
lndet2=ln(det(sigtvar));
supLR12=(rows(ye1))*(lndet1-lndet2);
retp(supLR12);
endp;

/*hansenp: compute Hansen p-value & 5% & 10% critical values. Code adapted 
from Hansen (1996) and Hansen & Seo (2002) */
proc (4)=hansenp(boot, qe, rhatf, suplr); 
/* boot is # of bootstrap reps (Hansen & Seo recommend at least 1000 reps), 
qe is the threshold variable,
rhatf is the estimated TVAR threshold level,
suplr is the model supLR statistic (actual, not simulated) */
local fix01, t, r, ur, e1b, bl2b, bh2b, el2b, eh2b, sighat1b, lndet1b, sigtvarb, lndet2b; 
local supLR12b, phansen, hanscv5, hanscv10; 
rndseed 3456;
fix01=zeros(boot,1);
t=rows(ye);
r=1; do while r<=boot; 
ur=rndn(t,l).*e_var; @ multiple each OLS residual by N(0,1) random variable, row(ye) x l @
@ Regress ur on xe and get residuals >> get 1-regime bootstrap residuals, e1b @
e1b=ur-xe*(invpd(xe'xe)*(xe'ur));
@ Regress ur on xe for low & high regimes based on rhatf @
{bl2b, bh2b, el2b, eh2b}=twotest(ur, xe, qe, rhatf);
@ Compute LR statistic per Zivot & Lo, 2001 @
sighat1b=(e1b'e1b)./rows(e1b);
lndet1b=ln(det(sighat1b));
sigtvarb=((el2b'el2b)+(eh2b'eh2b))/(rows(el2b)+rows(eh2b));
lndet2b=ln(det(sigtvarb));
supLR12b=(rows(e1b))*(lndet1b-lndet2b);
fix01[r]=supLR12b;
r=r+1; endo;
phansen=meanc(fix01.>suplr); @ % of simulated supLRs that exceed the model supLR @
fix01=sortc(fix01,1);
hanscv5=fix01[round(.95*boot),.]; @ 5% critical value @
hanscv10=fix01[round(.90*boot),.]; @ 10% critical value @
retp(phansen, hanscv5, hanscv10, boot);
endp;




/***************************************************************************************/
/***************************************************************************************/
/******************************* CHECK FOR FIRST THRESHOLD *****************************/
/***************************************************************************************/
/***************************************************************************************/


/***************************************************************************************/
/****** 2-REGIME TVAR: FRA smallholder market share(t-1)******/
{rhat2, shat}=threestc(ye, xe, qmse, minobs, ssrvar1); @ Find optimal threshold level  @ 
{bl2, bh2, el2, eh2}=twotest(ye, xe, qmse, rhat2); @ Estimate VARs for <= and > threshold @
sigl2=(el2'el2)./rows(el2); sigh2=(eh2'eh2)./rows(eh2); @ Resid. cov. matrices @
lltvarl=-(rows(el2)*l/2)*ln(2*pi)-(rows(el2)*l/2)+(rows(el2)/2)*ln(det(invpd(sigl2)));
/*lltvar2h=-(rows(eh2)*l/2)*ln(2*pi)-(rows(eh2)*l/2)+(rows(eh2)/2)*ln(det(invpd(sigh2))); 
lltvar2=lltvarl+lltvar2h;*/
ssrtvar2=sumc(diag(el2'el2))+sumc(diag(eh2'eh2));
ssrtvar2h=sumc(diag(eh2'eh2));
sigma2h=sigh2;
evar2h=eh2;
suplrm=suplrtvar(yi, qmse, minobs, ssrvar1);
{phansen, hanscv5, hanscv10, boot}=hansenp(10000, qmse, rhat2, suplrm); 
"************ 2-R TVAR RESULTS: FRA smallholder market share (t-1) ****************";
/*"Log likelihood:"  lltvar2;*/
"SSR:" ssrtvar2;
"Total number of parameters estimated including threshold level:" 2*l*(phat*l+1)+1; 
"Threshold level:" rhat2;
"# of obs <= threshold:" rows(el2);
"# of obs > threshold::" rows(eh2);
"LL, <= threshold:"  lltvarl;
"Matrix of RF VAR coef., <= threshold:" bl2;
"Residual covariance matrix, <= threshold:" sigl2;
/*"LL, > threshold:" lltvar2h;*/
"Matrix of RF VAR coef., > threshold:" bh2;
"Residual covariance matrix, > threshold:" sigh2;
"GP BIC value: high regime var-cov matrix not pos. def.>>can't compute LL or GP BIC" ;
/*gpbic2(llvar1, lltvar2, rows(ye), 1);*/
"Multivariate Hansen test LR statistic:" suplrm;
"Fixed Regressor (Asymptotic) Hansen p-value:" phansen;
"Fixed Regressor (Asymptotic) 5% & 10% critical values:" hanscv5~hanscv10;
"Number of bootstrap replications for Hansen tests:" boot; 
" ";
" ";




/***************************************************************************************/
/****** 2-REGIME TVAR: SH marketable surplus remaining after FRA purchases(kg/capita, t-1)******/
{rhat3, shat}=threestc(ye, xe, qmpe, minobs, ssrvar1); @ Find optimal threshold level  @ 
{bl2, bh2, el2, eh2}=twotest(ye, xe, qmpe, rhat3); @ Estimate VARs for <= and > threshold @
sigl2=(el2'el2)./rows(el2); sigh2=(eh2'eh2)./rows(eh2); @ Resid. cov. matrices @
lltvarl=-(rows(el2)*l/2)*ln(2*pi)-(rows(el2)*l/2)+(rows(el2)/2)*ln(det(invpd(sigl2)));
lltvar3h=-(rows(eh2)*l/2)*ln(2*pi)-(rows(eh2)*l/2)+(rows(eh2)/2)*ln(det(invpd(sigh2))); 
lltvar3=lltvarl+lltvar3h;
ssrtvar3=sumc(diag(el2'el2))+sumc(diag(eh2'eh2));
ssrtvar3h=sumc(diag(eh2'eh2));
sigma3h=sigh2;
evar3h=eh2;
suplrm=suplrtvar(yi, qmpe, minobs, ssrvar1);
{phansen, hanscv5, hanscv10, boot}=hansenp(10000, qmpe, rhat3, suplrm); 
"***************** 2-R TVAR RESULTS:  SH marketable surplus remaining ****************";
"************************** after FRA purchases(kg/cap, t-1) *************************";
"Log likelihood:"  lltvar3;
"SSR:" ssrtvar3;
"Total number of parameters estimated including threshold level:" 2*l*(phat*l+1)+1; 
"Threshold level:" rhat3;
"# of obs <= threshold:" rows(el2);
"# of obs > threshold::" rows(eh2);
"LL, <= threshold:"  lltvarl;
"Matrix of RF VAR coef., <= threshold:" bl2;
"Residual covariance matrix, <= threshold:" sigl2;
"LL, > threshold:" lltvar3h;
"Matrix of RF VAR coef., > threshold:" bh2;
"Residual covariance matrix, > threshold:" sigh2;
"GP BIC value:" gpbic2(llvar1, lltvar3, rows(ye), 1);
"Multivariate Hansen test LR statistic:" suplrm;
"Fixed Regressor (Asymptotic) Hansen p-value:" phansen;
"Fixed Regressor (Asymptotic) 5% & 10% critical values:" hanscv5~hanscv10;
"Number of bootstrap replications for Hansen tests:" boot; 
" ";
" ";


/***************************************************************************************/
/******** 2-REGIME TVAR: Smallholder maize quantity harvested(kg/cap)*******************/
{rhat4, shat}=threestc(ye, xe, qape, 36, ssrvar1); @ Find optimal threshold level (only 24 obs if use minobs=24)  @ 
{bl2, bh2, el2, eh2}=twotest(ye, xe, qape, rhat4); @ Estimate VARs for <= and > threshold @
sigl2=(el2'el2)./rows(el2); sigh2=(eh2'eh2)./rows(eh2); @ Resid. cov. matrices @
lltvarl=-(rows(el2)*l/2)*ln(2*pi)-(rows(el2)*l/2)+(rows(el2)/2)*ln(det(invpd(sigl2)));
lltvarh=-(rows(eh2)*l/2)*ln(2*pi)-(rows(eh2)*l/2)+(rows(eh2)/2)*ln(det(invpd(sigh2))); 
lltvar4=lltvarl+lltvarh;
ssrtvar4=sumc(diag(el2'el2))+sumc(diag(eh2'eh2));
ssrtvar4l=sumc(diag(el2'el2));
ssrtvar4h=sumc(diag(eh2'eh2));
suplrm=suplrtvar(yi, qape, 36, ssrvar1);
{phansen, hanscv5, hanscv10, boot}=hansenp(10000, qape, rhat4, suplrm); 
"*********** 2-R TVAR RESULTS:  SH maize quantity harvested(kg/cap) **********";
"Log likelihood:"  lltvar4;
"SSR:" ssrtvar4;
"Total number of parameters estimated including threshold level:" 2*l*(phat*l+1)+1;
"Threshold level:" rhat4;
"# of obs <= threshold:" rows(el2);
"# of obs > threshold::" rows(eh2);
"LL, <= threshold:"  lltvarl;
"Matrix of RF VAR coef., <= threshold:" bl2;
"Residual covariance matrix, <= threshold:" sigl2;
"LL, > threshold:" lltvarh;
"Matrix of RF VAR coef., > threshold:" bh2;
"Residual covariance matrix, > threshold:" sigh2;
"GP BIC value (using LLs):" gpbic2(llvar1, lltvar4, rows(ye), 1);
"Multivariate Hansen test LR statistic:" suplrm;
"Fixed Regressor (Asymptotic) Hansen p-value:" phansen;
"Fixed Regressor (Asymptotic) 5% & 10% critical values:" hanscv5~hanscv10;
"Number of bootstrap replications for Hansen tests:" boot; 
" ";
" ";


/***************************************************************************************/
/************* 2-REGIME TVAR: Structural break (time is threshold variable) ************/
{rhat5, shat}=threestc(ye, xe, qte, minobs, ssrvar1); @ Find optimal threshold level @ 
{bl2, bh2, el2, eh2}=twotest(ye, xe, qte, rhat5); @ Estimate VARs for <= and > threshold @
sigl2=(el2'el2)./rows(el2); sigh2=(eh2'eh2)./rows(eh2); @ Resid. cov. matrices @
lltvarl=-(rows(el2)*l/2)*ln(2*pi)-(rows(el2)*l/2)+(rows(el2)/2)*ln(det(invpd(sigl2)));
lltvarh=-(rows(eh2)*l/2)*ln(2*pi)-(rows(eh2)*l/2)+(rows(eh2)/2)*ln(det(invpd(sigh2))); 
lltvar5=lltvarl+lltvarh;
ssrtvar5=sumc(diag(el2'el2))+sumc(diag(eh2'eh2));
ssrtvar5l=sumc(diag(el2'el2));
sig5l=sigl2;
e5l=el2;
suplrm=suplrtvar(yi, qte, minobs, ssrvar1);
{phansen, hanscv5, hanscv10, boot}=hansenp(10000, qte, rhat5, suplrm); 
"*************** 2-REGIME TVAR RESULTS:  Structural break (time) ****************";
"Log likelihood:"  lltvar5;
"SSR:" ssrtvar5;
"Total number of parameters estimated including threshold level:" 2*l*(phat*l+1)+1; 
"Threshold level:" rhat5;
"Month  structural break (Stata date format):" rhat5+438+phat-1;
"# of obs <= threshold:" rows(el2);
"# of obs > threshold::" rows(eh2);
"LL, <= threshold:"  lltvarl;
"Matrix of RF VAR coef., <= threshold:" bl2;
"Residual covariance matrix, <= threshold:" sigl2;
"LL, > threshold:" lltvarh;
"Matrix of RF VAR coef., > threshold:" bh2;
"Residual covariance matrix, > threshold:" sigh2;
"GP BIC value (using LLs):" gpbic2(llvar1, lltvar5, rows(ye), 1);
"Multivariate Hansen test LR statistic:" suplrm;
"Fixed Regressor (Asymptotic) Hansen p-value:" phansen;
"Fixed Regressor (Asymptotic) 5% & 10% critical values:" hanscv5~hanscv10;
"Number of bootstrap replications for Hansen tests:" boot; 
" ";
" ";


