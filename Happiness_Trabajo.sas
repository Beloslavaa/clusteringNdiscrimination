LIBNAME MULTI 'C:\Users\User\Documents\MULTIVARIANTE I';

proc print data=multi.Happiness;
run;

PROC CONTENTS DATA=multi.Happiness; RUN;

DATA _NULL_; SET NOMBRES;
PUT NAME;
WHERE TYPE=2;
RUN;

/***********************ANALISIS DE BONDAD DE LOS DATOS******************************/

/* VALORES PERDIDOS*/
PROC MEANS DATA=multi.Happiness NMISS;
RUN; /*no hay valores perdidos*/


/* VALORES ATIPICOS */
%outliers_mult (data=multi.Happiness, var=GDP_per_capita Score Social_support Healthy_life_expectancy 
Freedom_to_make_life_choices Generosity Perceptions_of_corruption, id=Country_or_region); 

/*
 34 = Singapore
 131 = Myanmar
 155 = Central African Republic
*/


/*me he hecho una copia de la base de datos, que no contiene datos atipicos para el analisis posterior 
proc print data=multi.happyscore;  run; */

proc sql;
delete from multi.happyscore
where Country_or_region="Central African Republic" | Country_or_region="Myanmar" | Country_or_region="Singapore";



/* NORMALIDAD */

%NORMAL_MULT(DATA=multi.Happiness, var=GDP_per_capita Social_support Healthy_life_expectancy 
Freedom_to_make_life_choices Generosity Perceptions_of_corruption); 

proc univariate data=multi.Happiness;
var GDP_per_capita Score Social_support Healthy_life_expectancy 
Freedom_to_make_life_choices Generosity Perceptions_of_corruption;
histogram / normal(color=red);
inset n min max mean std / pos = ne header="Summary Statistics";
axis1 label =(a=90 r=0);
run;

/* Se accepta Normalidad multidimensional, pero con unos p-valores del apuntamiento y asimetria 0, rechazamos la normalidad de las
variables individuales*/

/*Solo Generosidad y Score se distribuyen normalmente*/


/* ANALISIS DE MULTICOLINEALIDAD */
PROC CORR DATA=multi.Happiness; RUN;
/* Hay dependencia entre variables, se puede hacer estudio de CP y analisis factorial*/
/*Si ninguno fuese con p-valor<0.001 entonces no se puede acceptar la existencia de multicolinealidad*/

/*medimos la inercia - suma de las varianzas*/
proc means data=multi.Happiness n min max mean var median; 
var GDP_per_capita--Perceptions_of_corruption; 
RUN;
/*las varianzas tienen varianzas diferentes; para no cometer el error de que las variables con mayor varianza pesen mas en el estudio, es decir
que la variable sola explica un porcentaje muy grande de la varianza total, tenemos que tipificar*/


/*********************************COMPONENTES PRINCIPALES***********************************************/

/*Intentar explicar la inercia con pocas variables*/

PROC PRINCOMP DATA=multi.Happiness /*n=2 /*pongo despues de tomar mi desicion*/ OUTSTAT=Public OUT=PCSCORES;
	VAR GDP_per_capita Score Social_support Healthy_life_expectancy 
Freedom_to_make_life_choices Generosity Perceptions_of_corruption;
run;

/*observacion - los tres valores - libertad, generosidad y corrupcion estan poco correlacionados con el resto de las variables y tambien
entre si. Por esta razon simpre se quedan poco explicadas por los factores que sacamos en analisis factorial.*/
PROC PRINCOMP DATA=multi.Happiness OUTSTAT=Public OUT=PCSCORES;
	VAR /*GDP_per_capita Score Social_support Healthy_life_expectancy*/
Freedom_to_make_life_choices Generosity Perceptions_of_corruption;
run;

/*las primeras 2 componentes explican 73% de la varianza y son mayores que 1; para el analisis posterior podemos considerar 2 o 3 factores*/

/*representacion de variables*/

PROC PRINT DATA=PUBLIC WIDTH=MINIMUM;
FORMAT GDP_per_capita Score Social_support Healthy_life_expectancy 
Freedom_to_make_life_choices Generosity Perceptions_of_corruption 6.2;
RUN;


Proc transpose data=Public out=Public2;
where  _type_='SCORE'; 
var GDP_per_capita Score Social_support Healthy_life_expectancy 
Freedom_to_make_life_choices Generosity Perceptions_of_corruption; 
run;


proc print data=Public2; 
run;

%plotit(data=Public2, plotvars= PRIN1 PRIN2, labelvar=_NAME_, cframe=cream, 
colors=green, unit=cm, href=0, options=nocenter,vref=0);

/*representacion de Paises*/

%plotit(data=PCSCORES, plotvars=PRIN1 PRIN2, labelvar=Country_or_region, cframe=cream, colors=green, unit=cm, href=0, options=nocenter, vref=0);


/************************************************ANALISIS FACTORIAL*****************************************************/


PROC FACTOR data=multi.Happiness 
SIMPLE CORR MSA  METHOD=PRINCIPAL PRIORS=ONE n=2 REORDER RESIDUAL OUTSTAT=TABLA2;
VAR GDP_per_capita Score Social_support Healthy_life_expectancy 
Freedom_to_make_life_choices Generosity Perceptions_of_corruption;
RUN;
/*2 factores explican 75% y 3 - 84% de la variabilidad; no se queda ninguna sin explicar*/


/* KMO - dividir las correlaciones entre correlaciones + correlaciones parciales*/
/*Tenemos la KMO 0.83644809 y ninguna de las variables <0.5 asi que no quitamos ninguna variable*/

/* Diferencia entre autovectores y cargas - los autovectores son cargas de norma UNO;
Los autovectores tienen norma (suma de todos los valores al cuadrado)=uno y las cargas tienen norma lambda;
Entonces para obtener las cargas se multiplica la raiz del autovalor por el autovector de cada componente*/


/*Ahora cambiamos el metodo de estimar las comunalidades iniciales - con SMC minimos cuadrados;
Miramos los autovalores y vemos que la varianza explicada acumulada para 2 factores es 1.0928, mayor que 1, entonces explican al 100% la
varianza;
Aunque parece de la ultima tabla que se explica menos varianza, tenemos que recordar que el total con este metodo cambia; 
De todas formas, tres variables se quedan sin explicar, asi que vamos a probar otros metodos*/


PROC FACTOR data=multi.Happiness 
SIMPLE CORR MSA  METHOD=PRINCIPAL PRIORS=SMC n=4 REORDER OUTSTAT=TABLA2;
VAR GDP_per_capita Score Social_support Healthy_life_expectancy 
Freedom_to_make_life_choices /*Generosity*/ Perceptions_of_corruption;
RUN;



/*otro metodo - por iteraciones*/
PROC FACTOR data=multi.Happiness 
SIMPLE CORR MSA  METHOD=PRINIT PRIORS=ONE maxiter=100 HEYWOOD  N=1 RESIDUAL OUT=TABLA1 OUTSTAT=TABLA2;
VAR GDP_per_capita Score Social_support Healthy_life_expectancy 
/*Freedom_to_make_life_choices /*Generosity Perceptions_of_corruption*/;
RUN;

%plotit(data=tabla1, plotvars= FACTOR1, labelvar=Country_or_region, cframe=cream, 
colors=green, unit=cm, href=0, options=nocenter,vref=0);


PROC FACTOR data=multi.Happiness 
SIMPLE CORR MSA  METHOD=PRINIT PRIORS=SMC  n=2 HEYWOOD;
VAR GDP_per_capita Score Social_support Healthy_life_expectancy 
Freedom_to_make_life_choices /*Generosity*/ Perceptions_of_corruption;
RUN;


/*estos dos metodos tampoco me valen, ya que otra vez se explica 100% de la variabilidad, pero en la ultima tabla veo que 3 variables se quedan sin explicar,
porque tienen la comunalidad final<0.5; por esto descartamos este metodo*/

/*Hasta aqui veo que los metodos Prinit con priors=(one, smc) y Principal con priors=smc explican con dos factores 100% de la variabilidad, es decir
mejora el modelo, pero al final resulta que se quedan 3 variables sin explicar por estos dos facotres;
Por otra parte, los componentes principales explican 75% de la varianza*/

/*probamos el metodo de la maxima verosimilitud, ya que la normalidad multi fue previamente acceptada*/

proc factor data=multi.Happiness SIMPLE MSA METHOD=ML PRIORS=ONE N=1 RESIDUAL REORDER HEYWOOD;
	VAR GDP_per_capita Score Social_support Healthy_life_expectancy 
/*Freedom_to_make_life_choices /*Generosity Perceptions_of_corruption*/;
RUN; /*ESTO*/

proc factor data=multi.Happiness SIMPLE MSA METHOD=ML PRIORS=SMC  N=3 REORDER HEYWOOD; 
	VAR GDP_per_capita Score Social_support Healthy_life_expectancy 
Freedom_to_make_life_choices /*Generosity /*Perceptions_of_corruption*/;
RUN;


/*******************************ROTAMOS************************************************/

/*El objetivo es poder dar a cada factor un nombre. Hemos decidido que el metodo de componentes principales ha dado mejores resultados, asi que 
rotamos este. De todas formas, antes de hacerlo, analizamos los residuos que tienen que ser menor que 1/raiz(n) o menores que 0.07. Con los 
componentes principales los residuos son demasiado grandes asi que despues de otro estudio se ha elegido ML con N=3 factores y quitando la
generosidad de las variables.*/

proc factor data=multi.Happiness SIMPLE MSA METHOD=ML PRIORS=ONE ROTATE=QUARTIMAX N=3 REORDER HEYWOOD RESIDUAL OUT=TABLA1 OUTSTAT=TABLA2
plots=all
plots(flip)=loadings
plots=(loadings(flip) scree(unpack));
	VAR GDP_per_capita Score Social_support Healthy_life_expectancy 
Freedom_to_make_life_choices /*Generosity*/ Perceptions_of_corruption;
RUN;

/*Factor 1: Calidad de Vida - el dinero que ganan, eperanza de la vida, ayudas sociales*/
/*Factor 2: Percepcion de la corrupcion*/
/*Factor 3: Libertad de poder elegir*/

%plotit(data=tabla1, plotvars= FACTOR1 FACTOR2, labelvar=Country_or_region, cframe=cream, 
colors=green, unit=cm, href=0, options=nocenter,vref=0);


/*************************SIN ATIPICOS****************************/

/*Como hemos comprobado, existen 3 observaciones que son atipicos - Singapore, Myanmar y Central African Republic. Vamos a repetir el estudio
sin ellas*/

PROC FACTOR data=multi.happyscore
SIMPLE CORR MSA  METHOD=PRINCIPAL PRIORS=ONE n=1 REORDER RESIDUAL OUTSTAT=TABLA2;
VAR GDP_per_capita Score Social_support Healthy_life_expectancy 
/*Freedom_to_make_life_choices Generosity Perceptions_of_corruption*/;
RUN;

PROC FACTOR data=multi.happyscore
SIMPLE CORR MSA  METHOD=PRINIT PRIORS=ONE  REORDER RESIDUAL OUT=TABLA1 OUTSTAT=TABLA2;
VAR GDP_per_capita Score Social_support Healthy_life_expectancy 
/*Freedom_to_make_life_choices Generosity Perceptions_of_corruption*/;
RUN;


proc factor data=multi.HAPPYSCORE SIMPLE MSA METHOD=ML PRIORS=one N=1 RESIDUAL REORDER HEYWOOD;
	VAR GDP_per_capita Score Social_support Healthy_life_expectancy 
/*Freedom_to_make_life_choices /*Generosity Perceptions_of_corruption*/;
RUN; 

proc factor data=multi.happyscore SIMPLE MSA METHOD=prinit PRIORS=ONE ROTATE=varimax N=1 REORDER HEYWOOD RESIDUAL OUT=TABLA1 OUTSTAT=TABLA2
plots=all
plots(flip)=loadings
plots=(loadings(flip) scree(unpack));
	VAR GDP_per_capita Score Social_support Healthy_life_expectancy 
/*Freedom_to_make_life_choices /*Generosity Perceptions_of_corruption*/;
RUN;

%plotit(data=tabla1, plotvars= FACTOR1, labelvar=Country_or_region, cframe=cream, 
colors=green, unit=cm, href=0, options=nocenter,vref=0);

/***************************ANALISIS DE CORRESPONDENCIA*********************************/

/*haciendo un analisis de correlacion se puede ver que las variables PIB per capita y Esperanza de vida son mas relacionadas - 
vamos a discretizar ellas y ver como se relacionan con un analisis de correspondencia*/

proc univariate data=multi.Happiness;
var GDP_per_capita Healthy_life_expectancy;
output out=percentiles pctlpts=25 50 75  pctlpre= GDP Health pctlname=P25 P50 P75;
run;
proc print data=percentiles; run;


data multi.Happiness;
set multi.Happiness;
if GDP_per_capita <= 0.5945 then gdp="Low GDP";
if 0.5945<GDP_per_capita <=0.96  then gdp="Lower GDP";
if 0.96<GDP_per_capita <=1.234  then gdp="Higher GDP";
if GDP_per_capita > 1.234  then gdp="High GDP";

if Healthy_life_expectancy <=0.5445 then health="Poor Health";
if 0.5445<Healthy_life_expectancy <=0.789 then health="Lower Health";
if 0.789<Healthy_life_expectancy <=0.8825 then health="Higher Health";
if Healthy_life_expectancy > 0.8825 then health="Good Health";

run;


PROC CORRESP DATA=multi.Happiness ALL /*short*/ chi2p OUTC=GRAFICA /*n=3*/; 
	TABLES GDP, Health; 
RUN; 

/*Por filas:
	-Las categorias que mejor explican Dim1 son los dos extremos - PIB per capita mas alto y el mas bajo
	-Dim2: La categoria PIB per capita medio alto y la PIB per capita medio bajo
  Por columnas:
	-Dim1: Las categorias que mejor explican Dim1 son tambien los extremos - esperanza de vida alta y baja
	-Dim2: La speranza de vida medio alta y la medio baja

Como se puede esperar (y como sucede realmente) las categorias que estan es en mismo quartil estan muy relacionadas. 
Es decir a PIB per capita alto le corresponde una esperanza de vida alta. Para PIB medio alto - esperanza medio alta y asi las 4 categorias*/ 











/****************************CLUSTERING*****************************************/

/*Vamos a hacer clusterig explorativo. En un principio, como hemos hecho analisis facotrial, usaremos las tres variables
independientes para el analisis de clusters*/



/**************************************************/
proc stdize data=multi.happyscore out=happystd;
   var GDP_per_capita Score Social_support Healthy_life_expectancy 
Freedom_to_make_life_choices Generosity Perceptions_of_corruption;
run;

proc print data=happystd; run;

PROC FACTOR data=multi.happyscore
SIMPLE CORR MSA  METHOD=PRINIT PRIORS=ONE maxiter=100 HEYWOOD  N=1 RESIDUAL OUT=TABLA1 OUTSTAT=TABLA2;
VAR GDP_per_capita Score Social_support Healthy_life_expectancy 
/*Freedom_to_make_life_choices /*Generosity Perceptions_of_corruption*/;
RUN;

/*****lejano*****/


/*cl=3 F=55.4 t=19.1 */
proc cluster NONORM std data=tabla1 method=com print=15 ccc pseudo
out=cluster_com;
var Freedom_to_make_life_choices Generosity Perceptions_of_corruption Factor1;
ID overall_rank;
copy Country_or_region;
run;

proc tree data=cluster_com ncl=3 out=saltree_com;
id overall_rank;
copy Country_or_region Freedom_to_make_life_choices Generosity Perceptions_of_corruption Factor1;
run;

proc sort data=saltree_com; by cluster; 

proc means data=saltree_com; by cluster; output out=centroides;
var Freedom_to_make_life_choices Generosity Perceptions_of_corruption Factor1;
run;

/*Se observa que las variables, cuyas medias son mas distinatas entre los 3 grupos son de Freedom y Factor1*/
proc  sgplot data=saltree_com;
scatter x=Freedom_to_make_life_choices y=Factor1/group=cluster;
run;
/*discrimina bastante bien*/

proc standard data=centroides mean=0 std=1 out=stcentr;
var Freedom_to_make_life_choices Generosity Perceptions_of_corruption Factor1;
run;

proc standard data=tabla1 mean=0 std=1 out=sdata;
var Freedom_to_make_life_choices Generosity Perceptions_of_corruption Factor1;
run;

proc fastclus data=sdata seed=stcentr radius=0 replace=FULL list distance maxclusters=3 out=new maxiter=20;
var Freedom_to_make_life_choices Generosity Perceptions_of_corruption Factor1;
run;

proc  sgplot data=new;
scatter x=Perceptions_of_corruption y=Factor1/group=cluster;
run; /*F mejora a 72.61 y ccc=10.720*/

/*****average******/

/*
  cl=5 F=37.7 t=3.9 */

proc cluster NONORM std data=tabla1 method=ave print=15 ccc pseudo
out=cluster_ave;
var Freedom_to_make_life_choices Generosity Perceptions_of_corruption Factor1;
ID overall_rank;
copy Country_or_region;
run;

proc tree data=cluster_ave ncl=5 out=saltree_ave;
id overall_rank;
copy Country_or_region Freedom_to_make_life_choices Generosity Perceptions_of_corruption Factor1;
run;

proc sort data=saltree_ave; by cluster; 

proc  sgplot data=saltree_ave;
scatter x=Freedom_to_make_life_choices y=Factor1/group=cluster;
run;


/*tal vez 2 variables no son suficientes*/

proc candisc data=saltree_ave out=canave distance anova;
class cluster;
var Freedom_to_make_life_choices Generosity Perceptions_of_corruption Factor1;
run;

proc discrim data=saltree_ave out=discrim_happy outstat=estadisticos
pool=test testlisterr posterr manova distance; priors proportional; ;
  class cluster;
  var Freedom_to_make_life_choices Generosity Perceptions_of_corruption Factor1;
run;


/*no jerarquico*/
proc means data=saltree_ave; by cluster; output out=centroides;
var Freedom_to_make_life_choices Generosity Perceptions_of_corruption Factor1;
run;

proc standard data=centroides mean=0 std=1 out=stcentr;
var Freedom_to_make_life_choices Generosity Perceptions_of_corruption Factor1;
run;

proc standard data=tabla1 mean=0 std=1 out=sdata;
var Freedom_to_make_life_choices Generosity Perceptions_of_corruption Factor1;
run;

proc fastclus data=sdata seed=stcentr radius=0 replace=FULL list distance maxclusters=5 out=new maxiter=20;
var Freedom_to_make_life_choices Generosity Perceptions_of_corruption Factor1;
run;

proc  sgplot data=new;
scatter x=Freedom_to_make_life_choices y=Factor1/group=cluster;
run; 


proc sort data=new; by cluster; 
proc print data=new; var Country_or_region cluster;

/*analisis discriminatorio*/
proc candisc data=NEW out=outcan distance anova;
class cluster;
var Freedom_to_make_life_choices Generosity Perceptions_of_corruption Factor1;
run;

/*todas las variables son aptas para discriminar, basado en la informacion de la distancia de Mahanalobis. Ahora veremos si los
factores canonicos, o las variables originales descriminan mejor*/

proc discrim data=NEW out=discrim_happy outstat=estadisticos
pool=test testlisterr posterr manova distance; priors proportional; ;
  class cluster;
  var Freedom_to_make_life_choices Generosity Perceptions_of_corruption Factor1;
run;

/* a priori - (proportional) 0.0128 ; (equal) 0.0147
   a posteriori - (proportional) 0.0689 ; (equal) 0.0597 y 0.0717 no estratificado
*/

proc discrim data=outcan out=discrim_happy outstat=estadisticos
pool=test testlisterr posterr manova distance; priors equal; ;
  class cluster;
  var can1 can2;
run;

/* a priori - (proportional) 0.0256 ; (equal) 0.0196
   a posteriori - (proportional) 0.0833 ; (equal) 0.0737 
*/


/*****ward*****/


/*cl=5 F=63.1 t=28.5 */
proc cluster NONORM std data=tabla1 method=ward print=15 ccc pseudo
out=cluster_ward;
var Freedom_to_make_life_choices Generosity Perceptions_of_corruption Factor1;
ID overall_rank;
copy Country_or_region;
run;

proc tree data=cluster_ward ncl=6 out=saltree_ward;
id overall_rank;
copy Country_or_region Freedom_to_make_life_choices Generosity Perceptions_of_corruption Factor1;
run;

proc sort data=saltree_ward; by cluster; 

proc  sgplot data=saltree_ward;
scatter x=Freedom_to_make_life_choices y=Factor1/group=cluster;
run;

proc means data=saltree_ward; by cluster; output out=centroides;
var Freedom_to_make_life_choices Generosity Perceptions_of_corruption Factor1;
run;

proc standard data=centroides mean=0 std=1 out=stcentr;
var Freedom_to_make_life_choices Generosity Perceptions_of_corruption Factor1;
run;

proc standard data=tabla1 mean=0 std=1 out=sdata;
var Freedom_to_make_life_choices Generosity Perceptions_of_corruption Factor1;
run;

proc fastclus data=sdata seed=stcentr radius=0 replace=FULL list distance maxclusters=6 out=new maxiter=20;
var Freedom_to_make_life_choices Generosity Perceptions_of_corruption Factor1;
run;

proc  sgplot data=new;
scatter x=Perceptions_of_corruption y=Factor1/group=cluster;
run;

/* F mejora a 70.64 y ccc a 7.220*/


/***********************merge********************/

data final2;
merge new multi.happyscore;
by overall_rank;
run;

proc print data=final2; run;
proc sort data=final2; by cluster; run;


/**********centroid*******/

/*cl=4 F=24.4 t=3.0 */
proc cluster NONORM std data=tabla1 method=centroid print=15 ccc pseudo
out=cluster_cent;
var Freedom_to_make_life_choices Generosity Perceptions_of_corruption Factor1;
ID overall_rank;
copy Country_or_region;
run;

proc tree data=cluster_cent ncl=4 out=saltree_cent;
id overall_rank;
copy Country_or_region Freedom_to_make_life_choices Generosity Perceptions_of_corruption Factor1;
run;

proc sort data=saltree_cent; by cluster; 

proc  sgplot data=saltree_cent;
scatter x=Freedom_to_make_life_choices y=Factor1/group=cluster;
run;
/*casi no hay observaciones en grupos 3 y 4*/

proc means data=saltree_cent; by cluster; output out=centroides;
var Freedom_to_make_life_choices Generosity Perceptions_of_corruption Factor1;
run;

proc standard data=centroides mean=0 std=1 out=stcentr;
var Freedom_to_make_life_choices Generosity Perceptions_of_corruption Factor1;
run;

proc standard data=tabla1 mean=0 std=1 out=sdata;
var Freedom_to_make_life_choices Generosity Perceptions_of_corruption Factor1;
run;

proc fastclus data=sdata seed=stcentr radius=0 replace=FULL list distance maxclusters=4 out=new maxiter=20;
var Freedom_to_make_life_choices Generosity Perceptions_of_corruption Factor1;
run;

proc  sgplot data=new;
scatter x=Perceptions_of_corruption y=Factor1/group=cluster;
run;
/*la F =73.70 y ccc=9.730*/


/*****grupos canonicos*******/

proc aceclus data=multi.happyscore out=ace p=.03 /*noprint*/;
var Freedom_to_make_life_choices Generosity Perceptions_of_corruption 
GDP_per_capita Score Social_support Healthy_life_expectancy ;
run;

proc cluster std data=ACE method=com print=15 ccc pseudo
out=cluster_ace;
var CAN1-CAN7;
ID overall_rank;
copy Country_or_region Freedom_to_make_life_choices Generosity Perceptions_of_corruption 
GDP_per_capita Score Social_support Healthy_life_expectancy;
run;

/* las F son muy bajas y las t>F, no nos sirve*/

/**********************analisis discriminatorio final************/
/*hay que ejecutar primero todos los pasos del vecino lejano y el proc merge*/

PROC stepdisc DATA=final2 method=forward sle=0.15 
/*Buscamos la mayor F y la menor p-valor. La variable con estos es la primera que entra en nuestra lista de variables que discriminan mejor.
Luego recalculamos otra vez estos estadistico y repetimos hasta que el p-valor para todas las variable es mayor que 0.15
y no avanzamos mas con el estudio*/;
class cluster; var  Freedom_to_make_life_choices Generosity Perceptions_of_corruption Factor1 ; RUN;


proc  sgplot data=final2;
scatter x=Freedom_to_make_life_choices y=Factor1/group=cluster;
run;

proc  sgplot data=final2;
scatter y=Perceptions_of_corruption x=Factor1/group=cluster datalabel=country_or_region;
run;

%NORMAL_MULT(DATA=final2, var=Freedom_to_make_life_choices Generosity Perceptions_of_corruption Factor1); 

proc print data=multi.happyscore; run;


proc candisc data=final2 out=canave distance anova;
class cluster;
var score GDP_per_capita Social_support Healthy_life_expectancy Freedom_to_make_life_choices Generosity Perceptions_of_corruption ;
run;

proc discrim data=canave out=out_candisc outstat=estadisticos
pool=test testlisterr posterr manova distance; priors proportional; ;
  class cluster;
  var can1-can2;
run;

proc  sgplot data=canave;
scatter x=can1 y=can2/group=cluster datalabel=country_or_region;
run;

proc discrim data=final2 out=discrim_happy outstat=estadisticos
pool=test testlisterr posterr manova distance; priors proportional; ;
  class cluster;
  var /*Freedom_to_make_life_choices Generosity*/ Perceptions_of_corruption Factor1;
run;


proc sort data=final2; by cluster; run;

proc template;
  define statgraph classify;
    begingraph;
      layout overlay;
        contourplotparm x=can1  y=can2 z=_into_ / contourtype=fill  
						 nhint = 30 gridded = false;
        scatterplot x=can1  y=can2 / group=cluster includemissinggroup=false
	                 	    markercharacter = cluster;
      endlayout;
    endgraph;
  end;
run;
