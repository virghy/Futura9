  W                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=1
PAPERSIZE=5
COLOR=2
      Arial      estado      ,iif(INLIST(substr(cuenta,1,1),'1','5'),1,-1)      0      v2      Liif( saldos.mes1 = 0 or mes2 = 0 , 0, ((mes1+mes2)*100/mes1) - 100) * estado      0      v3      Eiif( saldos.mes2 = 0 or mes3 = 0 , 0, (mes3*100/mes2) - 100) * estado      0      v4      Eiif( saldos.mes3 = 0 or mes4 = 0 , 0, (mes4*100/mes3) - 100) * estado      0      v5      Eiif( saldos.mes4 = 0 or mes5 = 0 , 0, (mes5*100/mes4) - 100) * estado      0      v6      Eiif( saldos.mes5 = 0 or mes6 = 0 , 0, (mes6*100/mes5) - 100) * estado      0      meses1      saldos.mes1      0      meses2      saldos.mes1+saldos.mes2      0      meses3      #saldos.mes1+saldos.mes2+saldos.mes3      0      meses4      /saldos.mes1+saldos.mes2+saldos.mes3+saldos.mes4      0      meses5      ;saldos.mes1+saldos.mes2+saldos.mes3+saldos.mes4+saldos.mes5      0      meses6      Gsaldos.mes1+saldos.mes2+saldos.mes3+saldos.mes4+saldos.mes5+saldos.mes6      0      Unidad      6iif(m.unidadBase=1,1,iif(m.unidadBase=2,1000,1000000))      6iif(m.unidadBase=1,1,iif(m.unidadBase=2,1000,1000000))      Arial      Arial      Arial      Arial      Arial      -"Balance Comparativo Mensual (1er. Semestre)"             Arial      	m.empresa             Arial      )iif(m.sucursal='%', 'Todos',suc.sucursal)             Arial      	"Periodo"      Arial      (dtoc(m.dfecha) + " al " + dtoc(m.hfecha)             Arial      "Sucursal:"      Arial      x'Moneda: '+iif(m.Unidad=1,'',iif(m.Unidad=1000,'Miles de  ','Millones de '))+iif(m.tipoMoneda='L','Guaranies','Dolares')      Arial      (iif(m.centro='%', 'Todos',centro.centro)             Arial      ,"Nivel de Cuentas: " + alltrim(str(m.nivel))             Arial      "Centro Costo:"      Arial      "Cuenta"      Arial      "Descripci�n
"      Arial      "Enero"      "@J"             Arial      	"Var.
%"      "@I"             Arial      	"Febrero"      "@J"             Arial      	"Var.
%"      "@I"             Arial      "Marzo"      "@J"             Arial      	"Var.
%"      "@I"             Arial      "Abril"      "@J"             Arial      	"Var.
%"      "@I"             Arial      "Mayo"      "@J"             Arial      	"Var.
%"      "@I"             Arial      "Junio"      "@J"             Arial      	"Var.
%"      "@I"             Arial      "Total"      "@J"             Arial      "Var.
Total %"      "@I"             Arial      replicate(' ',nivel*3)+ cuenta             Arial      &round(saldos.mes1 * estado/m.Unidad,0)      "999,999,999,999"      Arial      0      "99,999.99"             Arial      (round((saldos.mes2) * estado/m.Unidad,0)      "999,999,999,999"      Arial      meslimite >=2      @iif(round(mes1,0)=0 or round(mes2,0) = 0,0,(mes2-mes1)*100/mes1)      "999999.99"             Arial      meslimite >=2      (round((saldos.mes3) * estado/m.Unidad,0)      "999,999,999,999"      Arial      meslimite >=3      @iif(round(mes2,0)=0 or round(mes3,0) = 0,0,(mes3-mes2)*100/mes2)      "999999.99"             Arial      meslimite >=3      'round((saldos.mes4)* estado/m.Unidad,0)      "999,999,999,999"      Arial      meslimite >=4      >iif(round(mes3,0)=0 or round(Mes4,0)=0,0,(mes4-mes3)*100/mes3)      "999999.99"             Arial      meslimite >=4      (round((saldos.mes5) * estado/m.Unidad,0)      "999,999,999,999"      Arial      meslimite >=5      >iif(round(mes4,0)=0 or round(mes5,0)=0,0,(mes5-mes4)*100/mes4)      "999999.99"             Arial      meslimite >=5      (round((saldos.mes6) * estado/m.Unidad,0)      "999,999,999,999"      Arial      meslimite >=6      >iif(round(mes5,0)=0 or round(mes6,0)=0,0,(mes6-mes5)*100/mes5)      "999999.99"             Arial      meslimite >=6      Dround((mes1+ mes2 + mes3 + mes4 +  mes5 + mes6) * estado/m.Unidad,0)      "999,999,999,999"      Arial      Hiif(round(meses1,0)=0 or round(meses6,0)=0,0,(meses6-meses1)*100/meses1)      "999999.99"             Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      dataenvironment      �Top = 66
Left = 20
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
     = sql('Select descripci�n sucursal from sucursal where idempresa = ?oApp.Empresa and sucursal = ?m.sucursal','Suc')
ENDIF
IF EMPTY(m.centro)
     m.centro = '%'
ELSE
     = sql('Select descripci�n centro from centros where idempresa = ?oApp.Empresa and centro = ?m.centro','Centro')
ENDIF
= sql('exec cn_balance_comp ?oApp.empresa, ?m.dfecha,?m.hfecha, ?oApp.Ejercicio, ?m.sucursal, ?m.centro, ?m.TipoMoneda','Saldos')
SELECT saldos
SET FILTER TO nivel <= m.nivel
GOTO TOP

ENDPROC
     ����    �  �                        ��   %   �      8               �  U  j �  � 7�� � � T� �C�� H�� %��� � L��W � T�� �� �� T�� �� �� �} � T�� ���� T�� ���� � %�C�� ���� � T�� �� %�� �!�w ��C�d Select descripci�n sucursal from sucursal where idempresa = ?oApp.Empresa and sucursal = ?m.sucursal� Suc� �� � %�C�� ���F� T�� �� %�� ���s ��C�] Select descripci�n centro from centros where idempresa = ?oApp.Empresa and centro = ?m.centro� Centro� �� �� ��C�o exec cn_balance_comp ?oApp.empresa, ?m.dfecha,?m.hfecha, ?oApp.Ejercicio, ?m.sucursal, ?m.centro, ?m.TipoMoneda� Saldos� �� F�	 � G(��
 ��
 �� #)� U  SETEO DEC	 MESLIMITE HFECHA
 TIPOMONEDA	 DECIMALES SUCURSAL SQL CENTRO SALDOS NIVEL Init,     ��1 q � A� � � � � A � qA � 1A Qq Q 2                       �      )   �                  