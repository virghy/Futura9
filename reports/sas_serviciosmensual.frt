  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=0
PAPERSIZE=9
COLOR=2
      Courier New      Arial      Arial      Arial      Courier New      Arial      alltrim( empresa )             Arial      )"Resumen Mensual de Servicios realizados"      Arial      m.dfecha, ' al ' , m.hfecha      Arial      	"Periodo"      Arial      "Visita Medica"      Arial      "Traslados"      Arial      "Urgencias"      Arial      "Enfermeria"      Arial      "Otros"      Arial      	"Totales"      Arial      "Fecha"      Arial      	"Adultos"      Arial      "Pediat"      Arial      	"Adultos"      Arial      "Pediat"      Arial      	"Adultos"      Arial      "Pediat"      Arial      	"Adultos"      Arial      "Pediat"      Arial      VMA      "999"      Arial      VMP      Arial      TA      "999"      Arial      TP      Arial      UA      "999"      Arial      UP      Arial      EA      "999"      Arial      EP      Arial      Otros      Arial      TotDia      Arial      FechaAgenda      Arial      "P�g. " + str( _pageno,3 )             Arial      
datetime()             Arial      VMA      "999"      Arial      VMP      Arial      TA      "999"      Arial      TP      Arial      UA      "999"      Arial      UP      Arial      EA      "999"      Arial      EP      Arial      Otros      Arial      TotDia      Arial      dataenvironment      _Top = 220
Left = 1
Width = 520
Height = 200
DataSource = .NULL.
Name = "Dataenvironment"
     vPROCEDURE Init
DO SETEO
TEXT TO CMDSQL NOSHOW
Select 
fechaAgenda=convert(datetime,CONVERT(VARCHAR (10),fechaAgenda,105)),

VMA=Sum(Case when idTipoServicio='03'
	and edad>15 then 1 else 0 end),
VMP=Sum(Case when idTipoServicio='03'
	and edad<=15 then 1 else 0 end),
TA=Sum(Case when idTipoServicio='04'
	and edad>15 then 1 else 0 end),
TP=Sum(Case when idTipoServicio='04'
	and edad<=15 then 1 else 0 end),
UA=Sum(Case when idTipoServicio in('01','02')
	and edad>15 then 1 else 0 end),
UP=Sum(Case when idTipoServicio in('01','02')
	and edad<=15 then 1 else 0 end),
EA=Sum(Case when idTipoServicio='14'
	and edad>15 then 1 else 0 end),
EP=Sum(Case when idTipoServicio='14'
	and edad<=15 then 1 else 0 end),

Otros=Sum(Case when (not idTipoServicio in ('03','01','02','04','14'))
				or edad is null then 1 else 0 end),
TOTDIA=COUNT(*)
from dbo.sas_SolicitudServ	
WHERE (convert(datetime,CONVERT(VARCHAR (10),fechaAgenda,105)) between ?m.dfecha and ?m.hfecha)
and IdEmpresa=?oApp.Empresa
group by CONVERT(VARCHAR (10),fechaAgenda,105)
order by 1
ENDTEXT

sql (cmdsql, "consulta")
SELECT CONSULTA

ENDPROC
     ����    �  �                        l�   %   �      M  #             �  U  � �  �	 M(� �� � Select �J �D fechaAgenda=convert(datetime,CONVERT(VARCHAR (10),fechaAgenda,105)),� �  �+ �% VMA=Sum(Case when idTipoServicio='03'�& �  	and edad>15 then 1 else 0 end),�+ �% VMP=Sum(Case when idTipoServicio='03'�' �! 	and edad<=15 then 1 else 0 end),�* �$ TA=Sum(Case when idTipoServicio='04'�& �  	and edad>15 then 1 else 0 end),�* �$ TP=Sum(Case when idTipoServicio='04'�' �! 	and edad<=15 then 1 else 0 end),�3 �- UA=Sum(Case when idTipoServicio in('01','02')�& �  	and edad>15 then 1 else 0 end),�3 �- UP=Sum(Case when idTipoServicio in('01','02')�' �! 	and edad<=15 then 1 else 0 end),�* �$ EA=Sum(Case when idTipoServicio='14'�& �  	and edad>15 then 1 else 0 end),�* �$ EP=Sum(Case when idTipoServicio='14'�' �! 	and edad<=15 then 1 else 0 end),� �  �L �F Otros=Sum(Case when (not idTipoServicio in ('03','01','02','04','14'))�- �' 				or edad is null then 1 else 0 end),� � TOTDIA=COUNT(*)�! � from dbo.sas_SolicitudServ	�e �_ WHERE (convert(datetime,CONVERT(VARCHAR (10),fechaAgenda,105)) between ?m.dfecha and ?m.hfecha)�! � and IdEmpresa=?oApp.Empresa�4 �. group by CONVERT(VARCHAR (10),fechaAgenda,105)� �
 order by 1� � ��C � � consulta� �� F� � U  SETEO CMDSQL SQL CONSULTA Init,     ��1 q � � �a �a�q�a�q1a1q�a�qa ��QQAA �q 2                       k      )   �                  