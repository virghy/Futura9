  �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 �DRIVER=winspool
DEVICE=HP DeskJet 692C
OUTPUT=\\DIRCO-PC2\EPSON
ORIENTATION=0
PAPERSIZE=1
DEFAULTSOURCE=7
PRINTQUALITY=600
COLOR=2
YRESOLUTION=300
TTOPTION=1
      L  !  winspool HP DeskJet 692C \\DIRCO-PC2\EPSON                                �HP DeskJet 692C                  � @ n�  ro    X  ,                                                                              @ MSUD$HP DeskJet 692C                 �             d           Arial      sucursal      sucursal+idcliente      sucursal+idcliente+idmoneda      Arial      Arial      Arial      Arial      Arial      Arial      Arial      titulo             Arial      empresa             Arial      
"Sucursal"      Arial      )iif(empty(m.sucursal),'Todos',m.sucursal)             Arial      per�odo             Arial      
"Per�odo:"      Arial      "  Cliente"      Arial      "Nombre"      Arial      "Comprobante"      Arial      "Fecha Comp.
"      Arial      
"Exenta
"      Arial      "Gravada
"      Arial      "Iva
"      Arial      	"Total
"      Arial      
"Moneda
"      Arial      alltrim( idcliente )             Arial      	razsocial             Arial      /alltrim(idcomprob)+" "+alltrim(str(nrocomprob))             Arial      exenta      "999,999,999,999"             Arial      gravada      "999,999,999,999"             Arial      iva      "999,999,999"             Arial      exenta+gravada+iva      "999,999,999,999.99"             Arial      idmoneda             Arial      fecha             Arial      idmoneda             Arial      "Saldo cliente en:"      Arial      exenta      "999,999,999,999"             Arial      gravada      "999,999,999,999"             Arial      iva      "999,999,999"             Arial      exenta+gravada+iva      "999,999,999,999.99"             Arial      "P�g. " + str( _pageno,3 )             Arial      date()             Arial      time()             Arial      dataenvironment      ILeft = 10
Top = 6
Width = 381
Height = 380
Name = "Dataenvironment"
      cursor     �Left = 10
Top = 20
Width = 90
Height = 90
Alias = "vt_clientes_base"
Database = ..\data\datos.dbc
CursorSource = "vt_clientes_base"
Name = "Cursor1"
@f�� �l�\����K�uv������m��l���6��v}���3��`��[�x˾*
!�T��zܦZM"�c���\i���������e*,���L�Oaj�K�����ۏ"+��-�Qd��ԑ��aN��Ú�/C�<~c�����M�y��M�^g�{�?Cg�iO�[�
Ö-5/�
�߬�8��Tڳ2��ҿ�Po%:u�8��}!�fm��֫|K��u|j����<�_:�����Ƚp�+3{V=]� �G��������0���9���
q����'Kc����5Rm��q9;�4�_�C��l�;�lB3!P|��_�\�7nO2Q,#zv��؊ǉ>�T>�i�L�v�����#t�c�B[�)��94غ!�y}�Mt�d�����J���z���t����o.Z��pm������!|��A��t��r!u�	��e�U_�,{�s��N�c`.�k�_zW8�M瀗*��3~�8fG�����9�L��.�ux�ɈN�j��s����      cursor      �Left = 150
Top = 20
Width = 90
Height = 90
Alias = "vt_factura"
Database = ..\data\datos.dbc
CursorSource = "vt_factura"
Name = "Cursor2"
      cursor      �Left = 10
Top = 140
Width = 90
Height = 90
Alias = "sucursal_base"
Database = ..\data\datos.dbc
CursorSource = "sucursal_base"
Name = "Cursor3"
