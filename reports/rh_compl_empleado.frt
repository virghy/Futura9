  &                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=0
PAPERSIZE=1
COLOR=2
      Courier New      rh_rcompl_empleado.idempleado      m.saldo      <iif( rh_rcompl_empleado.saldo>0, rh_rcompl_empleado.saldo,0)      0      Arial      Arial      Arial      Courier New      Arial      '"Deducciones y Beneficios por Empleado"             Arial      empresa             Arial      m.dfecha, ' al ' , m.hfecha             Arial      
"Periodo:"             Arial      "Legajo"      Arial      "Nombre"      Arial      "Fecha"      Arial      
"Concepto"      Arial      "Nro."      Arial      	"Importe"      Arial      "Cuota"      Arial      
"Aplicado"      Arial      "Saldo"      Arial      rh_rcompl_empleado.idempleado             Arial      Dalltrim(rh_rcompl_empleado.apellido) +" "+ rh_rcompl_empleado.nombre             Arial      rh_rcompl_empleado.fecha             Arial      rh_rcompl_empleado.idconcepto             Arial      rh_rcompl_empleado.concepto             Arial      rh_rcompl_empleado.nro             Arial      rh_rcompl_empleado.total      "@Z 999,999,999"             Arial      rh_rcompl_empleado.cuota      "999,999,999"             Arial      rh_rcompl_empleado.aplicado      "999,999,999"             Arial      m.saldo      "999,999,999"             Arial      rh_rcompl_empleado.total>0      rh_rcompl_empleado.total      "@Z 999,999,999"             Arial      rh_rcompl_empleado.aplicado      "999,999,999"             Arial      m.saldo      "999,999,999"             Arial      rh_rcompl_empleado.total>0      "Total Concepto"      Arial      
datetime()             Arial      'P�g. '+alltrim(Str(_pageno))             Arial      dataenvironment      �Top = 53
Left = 222
Width = 520
Height = 219
InitialSelectedAlias = "rsueldos"
DataSource = .NULL.
Name = "Dataenvironment"
     $PROCEDURE Init

TEXT TO cmdSQL noshow
	SELECT Rh_complemento_base.idempresa,
	  Rh_complemento_base.idcomplemento, Rh_complemento_base.fecha,
	  Rh_complemento_base.nro, Rh_complemento_base.idempleado,
	  Rh_complemento_base.idconcepto, Rh_complemento_base.total,
	  Rh_complemento_base.saldo, Rh_complemento_base.cuota,
	  Rh_complemento_base.cancelado, Rh_complemento_base.tipo,
	  Rh_complemento_base.aplicado, Rh_empleado_base.nombre,
	  Rh_empleado_base.apellido, Rh_conceptos.concepto
	 FROM rh_complemento rh_complemento_base,
	    rh_empleado rh_empleado_base,
	    rh_conceptos 
	 WHERE (  Rh_complemento_base.idempleado = Rh_empleado_base.idempleado
	   AND  Rh_complemento_base.idconcepto = Rh_conceptos.idconcepto 
	   and Rh_complemento_base.idempresa = Rh_conceptos.idempresa )
	   AND  ( ( (  Rh_complemento_base.idempresa = ( ?oApp.Empresa )
	   AND  Rh_empleado_base.idempresa = ( ?oApp.Empresa ) )
	   AND  Rh_complemento_base.idempleado = ( ?m.idempleado ) )
	   AND  Rh_complemento_base.fecha BETWEEN ?m.dfecha AND ?m.hfecha )
	 ORDER BY Rh_complemento_base.idempleado, Rh_complemento_base.fecha,
	  Rh_complemento_base.idconcepto
	  
ENDTEXT

=sql(cmdSQL,'rh_rcompl_empleado')
SELECT rh_rcompl_empleado



ENDPROC
PROCEDURE BeforeOpenTables
DO seteo

ENDPROC
     :���    !  !                        dN   %   f      �     �          �  U   	 M(�  ��, �& 	SELECT Rh_complemento_base.idempresa,�F �@ 	  Rh_complemento_base.idcomplemento, Rh_complemento_base.fecha,�A �; 	  Rh_complemento_base.nro, Rh_complemento_base.idempleado,�C �= 	  Rh_complemento_base.idconcepto, Rh_complemento_base.total,�> �8 	  Rh_complemento_base.saldo, Rh_complemento_base.cuota,�A �; 	  Rh_complemento_base.cancelado, Rh_complemento_base.tipo,�? �9 	  Rh_complemento_base.aplicado, Rh_empleado_base.nombre,�9 �3 	  Rh_empleado_base.apellido, Rh_conceptos.concepto�0 �* 	 FROM rh_complemento rh_complemento_base,�( �" 	    rh_empleado rh_empleado_base,� � 	    rh_conceptos �M �G 	 WHERE (  Rh_complemento_base.idempleado = Rh_empleado_base.idempleado�H �B 	   AND  Rh_complemento_base.idconcepto = Rh_conceptos.idconcepto �F �@ 	   and Rh_complemento_base.idempresa = Rh_conceptos.idempresa )�G �A 	   AND  ( ( (  Rh_complemento_base.idempresa = ( ?oApp.Empresa )�? �9 	   AND  Rh_empleado_base.idempresa = ( ?oApp.Empresa ) )�C �= 	   AND  Rh_complemento_base.idempleado = ( ?m.idempleado ) )�J �D 	   AND  Rh_complemento_base.fecha BETWEEN ?m.dfecha AND ?m.hfecha )�K �E 	 ORDER BY Rh_complemento_base.idempleado, Rh_complemento_base.fecha,�' �! 	  Rh_complemento_base.idconcepto�	 � 	  � �# ��C �  � rh_rcompl_empleado� �� F� � U  CMDSQL SQL RH_RCOMPL_EMPLEADO
  �  � U  SETEO Init,     �� BeforeOpenTablesQ    ��1 � �a1�������aq�1��q� A 2q 5 q 2                       �            !    )   !                  