  	e                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 %ORIENTATION=1
PAPERSIZE=1
COLOR=2
      Arial      rvalores.idmoneda      rvalores.idtipovalor      Arial      Arial      Arial      Arial      Arial      "Valores en Cartera"             Arial      empresa             Arial      "Per�odo de Cobro:"      Arial      &dtoc(m.dfecha)+ " al " +dtoc(m.hfecha)             Arial      8iif(empty(m.estado),'Todos',estado_cheque.estado_cheque)             Arial      	"Estado:"      Arial      "Fecha
Cobro
"      Arial      "Fecha
Emisi�n"      Arial      	"Importe"      Arial      "Estado"      Arial      "Banco"      Arial      "Nro. Valor"      Arial      "Emisor"      Arial      rvalores.descripcion             Arial      	"Moneda:"      Arial      tipovalor.tipovalor             Arial      "Tipo de Valor:"      Arial      rvalores.importe      "@Z 999,999,999,999.99"             Arial      estado_cheque.estado_cheque             Arial      rvalores.fchcheque             Arial      rvalores.fchemision      "@J"             Arial      bancos.banco             Arial      rvalores.nrocheque             Arial      rvalores.nombre             Arial      rvalores.importe      "@Z 999,999,999,999.99"             Arial      "Total por Tipo de Valor:"      Arial      !"Total en "+ rvalores.descripcion             Arial      rvalores.importe      "@Z 999,999,999,999.99"             Arial      'P�g. '+alltrim(Str(_pageno))             Arial      
datetime()             Arial      "Total"             Arial      rcheques.importe      "@Z 999,999,999,999.99"             Arial      dataenvironment      �Top = 79
Left = 164
Width = 519
Height = 200
InitialSelectedAlias = "rvalores"
DataSource = .NULL.
Name = "Dataenvironment"
