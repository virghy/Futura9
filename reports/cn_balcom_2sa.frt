  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=1
PAPERSIZE=5
COLOR=2
      Arial      estado      ,iif(INLIST(substr(cuenta,1,1),'1','5'),1,-1)      0      v1      Eiif( saldos.mes6 = 0 or mes7 = 0 , 0, (mes7*100/mes6) - 100) * estado      0      v2      Eiif( saldos.mes7 = 0 or mes8 = 0 , 0, (mes8*100/mes7) - 100) * estado      0      v3      Eiif( saldos.mes8 = 0 or mes9 = 0 , 0, (mes9*100/mes8) - 100) * estado      0      v4      Giif( saldos.mes9 = 0 or mes10 = 0 , 0, (mes10*100/mes9) - 100) * estado      0      v5      Hiif( saldos.mes10 = 0 or mes11= 0 , 0, (mes11*100/mes10) - 100) * estado      0      v6      Iiif( saldos.mes11 = 0 or mes12 = 0 , 0, (mes12*100/mes11) - 100) * estado      0      meses1      mes1      0      meses2      	mes1+mes2      0      meses3      mes1+mes2+mes3      0      meses4      mes1+mes2+mes3+mes4      0      meses5      mes1+mes2+mes3+mes4+mes5      0      meses6      mes1+mes2+mes3+mes4+mes5+mes6      0      meses7      "mes1+mes2+mes3+mes4+mes5+mes6+mes7      0      meses8      'mes1+mes2+mes3+mes4+mes5+mes6+mes7+mes8      0      meses9      ,mes1+mes2+mes3+mes4+mes5+mes6+mes7+mes8+mes9      0      meses10      2mes1+mes2+mes3+mes4+mes5+mes6+mes7+mes8+mes9+mes10      0      meses11      8mes1+mes2+mes3+mes4+mes5+mes6+mes7+mes8+mes9+mes10+mes11      0      meses12      >mes1+mes2+mes3+mes4+mes5+mes6+mes7+mes8+mes9+mes10+mes11+mes12      0      Unidad      6iif(m.unidadBase=1,1,iif(m.unidadBase=2,1000,1000000))      6iif(m.unidadBase=1,1,iif(m.unidadBase=2,1000,1000000))      Arial      Arial      Arial      Arial      Arial      7"Balance Comparativo Mensual Acumulado (2do. Semestre)"             Arial      empresa             Arial      )iif(m.sucursal='%', 'Todos',suc.sucursal)             Arial      	"Periodo"      Arial      (dtoc(m.dfecha) + " al " + dtoc(m.hfecha)             Arial      "Sucursal:"      Arial      (iif(m.centro='%', 'Todos',centro.centro)             Arial      ,"Nivel de Cuentas: " + alltrim(str(m.nivel))             Arial      x'Moneda: '+iif(m.Unidad=1,'',iif(m.Unidad=1000,'Miles de  ','Millones de '))+iif(m.tipoMoneda='L','Guaranies','Dolares')      Arial      "Centro Costo:"      Arial      "Cuenta"      Arial      "Descripci�n
"      Arial      "Julio"      "@J"             Arial      	"Var.
%"      "@I"             Arial      "Agosto"      "@J"             Arial      	"Var.
%"      "@I"             Arial      "Setiembre"      "@J"             Arial      	"Var.
%"      "@I"             Arial      	"Octubre"      "@J"             Arial      	"Var.
%"      "@I"             Arial      "Noviembre"      "@J"             Arial      	"Var.
%"      "@I"             Arial      "Diciembre"      "@J"             Arial      	"Var.
%"      "@I"             Arial      "Total"      "@J"             Arial      "Var.
Total %"      "@I"             Arial      replicate(' ',nivel*3)+ cuenta             Arial      #round((meses7) * estado/m.Unidad,0)      "999,999,999,999"      Arial      6iif(meses7=0 or meses6=0,0,(meses7-meses6)*100/meses6)      "99,999.99"             Arial      !round(meses8 * estado/m.Unidad,0)      "999,999,999,999"      Arial      meslimite >= 8      6iif(meses8=0 or meses7=0,0,(meses8-meses7)*100/meses7)      "99,999.99"             Arial      meslimite >= 8      !round(meses9 * estado/m.Unidad,0)      "999,999,999,999"      Arial      meslimite >= 9      6iif(meses9=0 or meses8=0,0,(meses9-meses8)*100/meses8)      "99,999.99"             Arial      meslimite >= 9      "round(meses10 * estado/m.Unidad,0)      "999,999,999,999"      Arial      meslimite >= 10      8iif(meses10=0 or meses9=0,0,(meses10-meses9)*100/meses9)      "99,999.99"             Arial      meslimite >= 10      "round(meses11 * estado/m.Unidad,0)      "999,999,999,999"      Arial      meslimite >= 11      ;iif(meses11=0 or meses10=0,0,(meses11-meses10)*100/meses10)      "99,999.99"             Arial      meslimite >= 11      "round(meses12 * estado/m.Unidad,0)      "999,999,999,999"      Arial      meslimite >= 12      ;iif(meses12=0 or meses11=0,0,(meses12-meses11)*100/meses11)      "99,999.99"             Arial      meslimite >= 12      $round((meses12) * estado/m.Unidad,0)      "999,999,999,999"      Arial      8iif(meses12=0 or meses1=0,0,(meses12-meses1)*100/meses1)      "99,999.99"             Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      dataenvironment      �Top = 64
Left = -53
Width = 792
Height = 419
InitialSelectedAlias = "saldos"
DataSource = .NULL.
Name = "Dataenvironment"
     �PROCEDURE Init
DO seteo
PUBLIC m.dec, meslimite
meslimite = MONTH(m.hfecha)
IF m.tipomoneda = 'L'
     m.decimales = 0
     m.dec = 0
ELSE
     m.decimales = 2
     m.dec = 2
ENDIF
IF EMPTY(m.sucursal)
     m.sucursal = '%'
ELSE
     = sql('Select descripci�n sucursal from sucursal where idempresa = ?oApp.Empresa and sucursal = ?m.sucursal', 'Suc')
ENDIF
IF EMPTY(m.centro)
     m.centro = '%'
ELSE
     = sql('Select descripci�n centro from centros where idempresa = ?oApp.Empresa and centro = ?m.centro', 'Centro')
ENDIF
= sql('exec cn_balance_comp ?oApp.empresa, ?m.dfecha,?m.hfecha, ?oApp.Ejercicio, ?m.sucursal, ?m.centro, ?m.TipoMoneda', 'Saldos')
SELECT saldos
SET FILTER TO nivel <= m.nivel
GOTO TOP

ENDPROC
     ����    �  �                        ��   %   �      8               �  U  j �  � 7�� � � T� �C�� H�� %��� � L��W � T�� �� �� T�� �� �� �} � T�� ���� T�� ���� � %�C�� ���� � T�� �� %�� �!�w ��C�d Select descripci�n sucursal from sucursal where idempresa = ?oApp.Empresa and sucursal = ?m.sucursal� Suc� �� � %�C�� ���F� T�� �� %�� ���s ��C�] Select descripci�n centro from centros where idempresa = ?oApp.Empresa and centro = ?m.centro� Centro� �� �� ��C�o exec cn_balance_comp ?oApp.empresa, ?m.dfecha,?m.hfecha, ?oApp.Ejercicio, ?m.sucursal, ?m.centro, ?m.TipoMoneda� Saldos� �� F�	 � G(��
 ��
 �� #)� U  SETEO DEC	 MESLIMITE HFECHA
 TIPOMONEDA	 DECIMALES SUCURSAL SQL CENTRO SALDOS NIVEL Init,     ��1 q � A� � � � � A � qA � 1A Qq Q 2                       �      )   �                  