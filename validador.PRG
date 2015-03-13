FUNCTION Validador(CGPNro)
DIMENSION vecbas(10)
DIMENSION VecCta(10)
VecBas(1)=7
VecBas(2)=3
VecBas(3)=1
VecBas(4)=9
VecBas(5)=7
VecBas(6)=3
VecBas(7)=1
VecBas(8)=9
VecBas(9)=7
VecBas(10)=3

Do Cerar_Vector

CtaTxt= alltrim(str(CGPNro,11))
Ind=  Len(alltrim(CtaTxt))
CtaTxt= substr (alltrim(CtaTxt),1,Ind)
Ind= 10 - Len(alltrim(CtaTxt)) + 1
Cnt= 1


Do While Ind <= 10
	DigTxt = substr( alltrim (CtaTxt), Cnt,1)
	VecCta(Ind)= val(DigTxt)
	Ind= Ind + 1
	Cnt = Cnt + 1
ENDDO 

Tot= 0
Ind = 1

Do while Ind <= 10
	X= VecBas(Ind)
	Y=VecCta(Ind)
	Res= VecBas(Ind) * VecCta(Ind)
	If len (alltrim(str(Res,2)))>1
		DigTxt= substr(alltrim(str(Res)),2,1)
		Val=val(DigTxt)
	Else
		Val=Res
	Endif
	
	Tot= Tot + Val
	Ind= Ind + 1
ENDDO

If len (alltrim(str(Tot,2)))> 1
	DigTxt= substr(alltrim(str(Tot)),2,1)
	Val= val(DigTxt)
Else

	Val=tot
Endif

If Val =0
	DigVrf=0
Else
	DigVrf=10 - Val
Endif
RETURN DigVrf
ENDFUNC 



PROCEDURE Cerar_vector
	Ind=1
	Do while Ind <=10
		VecCta(Ind)=0
		Ind= ind + 1
	ENDDO 
ENDPROC 
