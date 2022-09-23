# TECA    **************************************************************************
# Autor: 	Enrique Ricardo Pablo Buendia Lozada
# 		Facultad de Cultura Física, Benemérita Universidad Autónoma de Puebla
#		23 septiembre 2022	
# 
# **********************************************************************************

# Lee un archivo CSV con la informacion de la prueba TECA
# corrige posibles errores
# crea la evaluación correspondiente en puntos y crea la evaluación correspondiente
# en percentiles

dat <- read.csv(file = 'hteca1.csv',sep=",")

# corregir captura y transcribir significado a numeros
for (j in 2:34){
dat[j]<- replace(dat[j],dat[j]=="Totalmente en desacuerdo",1)
dat[j]<- replace(dat[j],dat[j]=="totalmente en desacuerdo",1)
dat[j]<- replace(dat[j],dat[j]=="Totalmente en desaucerdo",1)
dat[j]<- replace(dat[j],dat[j]=="Totalmete en desacuerdo",1)
dat[j]<- replace(dat[j],dat[j]=="De acuerdo",2)
dat[j]<- replace(dat[j],dat[j]=="Ni de acuerdo ni en desacuerdo",3)
dat[j]<- replace(dat[j],dat[j]=="En desacuerdo",4)
dat[j]<- replace(dat[j],dat[j]=="Totalmente de acuerdo",5)
}
#dat      # mostrar para verificar funcionamiento correcto

#sexo
dat[1] <- replace(dat[1],dat[1]=="h",0)
dat[1] <- replace(dat[1],dat[1]=="m",1)
#dat[1]	# mostrar para verificar funcionamiento correcto
if (dat[1,1]==0) {sexxo="h"}
if (dat[1,1]==1) {sexxo="m"}

# evaluación individual =============================
# AP,CE,EE,AE  corresponden a 1,2,3,4 respectivamente
# AP: adopción de perspectivas
# CE: comprención emociuonal
# EE: estrés empático
# AE: alegría empática
esct <-c(2,4,3,4,3,1,2,3,4,2,1,3,2,2,1,4,1,3,4,1,4,4,3,2,4,1,2,3,1,3,2,1,2)
dat2<-dat[-1]
evaluacion=c()
suma1=0.0
suma2=0.0
suma3=0.0
suma4=0.0

for (i in 1:nrow(dat2)){
	for (j in 1:ncol(dat2)){
	    if(1 ==esct[j]) {suma1=suma1+as.numeric(dat2[i,j])}
	    if(2 ==esct[j]) {suma2=suma2+as.numeric(dat2[i,j])}
	    if(3 ==esct[j]) {suma3=suma3+as.numeric(dat2[i,j])}
	    if(4 ==esct[j]) {suma4=suma4+as.numeric(dat2[i,j])}

  }
  suma=suma1+suma2+suma3+suma4
	evaluacion <- c(evaluacion,suma1,suma2,suma3,suma4,suma)
  suma1=0.0
  suma2=0.0
  suma3=0.0
  suma4=0.0

}
cont=0
for (i in 1:length(evaluacion))  {
	cat(evaluacion[i]," ")
	cont=cont+1
	if (cont %% 5==0) {cat("=Puntuación directa PD\n")}
}





# percentiles varones, según manual
percv <- c(1,2,3,4,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,96,97,98,99)
apv <- c(-17,-1,18,19,20,-22,23,24,25,-1,-1,26,27,-1,28,-1,29,-1,30,31,-33,34,35,36,37,38,-40)
cev <- c(-14,-16,-18,19,20,21,-23,24,25,26,27,28,-1,-1,29,30,31,-1,32,-1,33,-35,36,37,-1,38,-45)
eev <- c(-11,-1,12,-1,13,-15,16,17,18,19,-1,20,21,-1,22,23,24,-1,25,26,27,28,30,31,32,33,40)
aev <- c(-19,20,21,-1,22,-25,26,27,-1,28,-1,29,30,-1,-1,31,32,33,-1,34,35,-1,36,-1,37,-1,40)
totalv<-c(-79,-81,-83,-85,-87,-92,-94,-98,99,-102,-104,-106,107,108,109,110,-112,-114,-117,-119,-122,-124,-129,130,-133,-135,-165)

# percentiles mujeres, según manual
percm <- c(1,2,3,4,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,96,97,98,99)
apm <- c(-18,-20,-1,21,22,-24,-1,25,26,-1,27,-29,-1,-1,30,-1,31,-1,32,-1,33,34,35,36,37,38,-40)
cem <- c(-18,-20,-22,23,24,-26,-1,27,28,29,-1,30,-1,31,-1,32,33,34,35,-1,36,37,38,39,-1,40,45)
eem <- c(-12,-14,15,-17,18,-20,21,-1,22,23,24,25,26,-1,27,28,-1,29,30,31,32,33,34,35,36,37,-40)
aem <- c(-19,-23,-1,24,25,-27,28,29,30,-1,31,-1,32,-1,-1,33,34,-1,35,36,-1,37,38,-1,39,-1,40)
totalm<-c(-88,-93,94,95,-97,-100,-104,-107,-110,-112,-115,-118,-120,121,-123,124,125,126,-129,-131,-133,-135,-139,140,-143,-146,-165)

dist <- function(sexo,valor,vector){
	
	vec=	evalua(vector)
	vec1=vec$v1
	vec2=vec$v2
	#print(vec1)
	#print(vec2)

	dd=c()
	for (i in vec2){
		dif=abs(valor-abs(i))
		dd=c(dd,dif)
	}
	pos <-vec1[which.min(dd)]
	if (sexo=="m"){
		return(percm[pos])
	}
	else{
		return(percv[pos])
	}
}


evalua <- function(vector){

aviso=0
indice=1
indice2=1
v1 <- c()
v2 <- c()


if (vector[indice]<0 && indice==1){
	fin=abs(vector[indice])
	for (i in 0:fin){
		v1=c(v1,indice)
		v2=c(v2,i)	
	}
#print(v1)
#print(v2)
}
if (vector[indice] > 0 && indice > 1){
	v1=c(v1,indice)
	v2=c(v2,vector[indice])	
}

long=length(vector)
limite=long
limite=abs(vector[limite])
while (indice < long){
if (aviso== 0) {
	indice=indice+1
	indice2=indice2+1
}
if (vector[indice] < 0 && vector[indice] != (-1) && indice > 1){
	aviso=0
	ini=fin+1
	fin=abs(vector[indice])
	for (i in ini:fin){
		v1=c(v1,indice) #indice2
		v2=c(v2,i)	
	}
#print(v1)
#print(v2)
}

if (vector[indice] == (-1)){
	indice=indice+1
	aviso=1
}

if (vector[indice] > 0 && indice > 1){
	aviso=0
	fin=vector[indice]
	v1=c(v1,indice) #indice2
	v2=c(v2,vector[indice])	
	#print(v1)
	#print(v2)
}


}


indice=long
if (vector[indice] < 0 && vector[indice] != (-1) && indice > 1){
	aviso=0
	ini=fin
	fin=abs(vector[indice])
	for (i in ini:fin){
		v1=c(v1,indice) #indice2
		v2=c(v2,i)	
	}
#print(v1)
#print(v2)
}
if (vector[indice] > 0 && indice > 1){
	aviso=0
	fin=vector[indice]
	v1=c(v1,indice) #indice2
	v2=c(v2,vector[indice])	
	#print(v1)
	#print(v2)
}


#length(v1)
#length(v2)
#print(v1)
#print(v2)

return(list(v1=v1, v2=v2))


}

if (sexxo=="m"){
cont=0
salto=0
while (cont < length(evaluacion))  {
	prc1=dist(sexxo,evaluacion[1+salto],apm)
	cat(prc1," ")
	prc2=dist(sexxo,evaluacion[2+salto],cem)
	cat(prc2," ")
	prc3=dist(sexxo,evaluacion[3+salto],eem)
	cat(prc3," ")
	prc4=dist(sexxo,evaluacion[4+salto],aem)
	cat(prc4," ")
	prc5=dist(sexxo,evaluacion[5+salto],totalm)
	cat(prc5," ")
	cont=cont+5
	if (cont %% 5==0) {cat("=Percentil\n")}
}
}
if (sexxo=="h"){
cont=0
salto=0
while (cont < length(evaluacion))  {
	prc1=dist(sexxo,evaluacion[1+salto],apv)
	cat(prc1," ")
	prc2=dist(sexxo,evaluacion[2+salto],cev)
	cat(prc2," ")
	prc3=dist(sexxo,evaluacion[3+salto],eev)
	cat(prc3," ")
	prc4=dist(sexxo,evaluacion[4+salto],aev)
	cat(prc4," ")
	prc5=dist(sexxo,evaluacion[5+salto],totalv)
	cat(prc5," ")
	cont=cont+5
	if (cont %% 5==0) {cat("=Percentil\n")}
}
}



