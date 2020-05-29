*-----------------------------------------------------------------------------
* <Rating>-77</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.V.SPOOL.PAGO.PROVEE
*-----------------------------------------------------------------------------
* Rutina para realizar autorizacion de la FT pendiente y la generacion del SPOOL
*-----------------------------------------------------------------------------
* Version	Autor		Fecha		Comentario
*-----------------------------------------------------------------------------
* 1.0	   Jorge H	   29.06.2018	 Inicial
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.EB.SLV.PAGO.PROVEEDORES
$INSERT I_F.CUSTOMER
$INSERT I_F.EB.SLV.GLOBAL.PARAM
$INSERT I_F.ACCOUNT
$INSERT I_F.FUNDS.TRANSFER
*-----------------------------------------------------------------------------

GOSUB INI
GOSUB OPENFILE
GOSUB PROCESS
RETURN

INI:
	FN.PAGO.PROVEE = 'F.EB.SLV.PAGO.PROVEEDORES'
 	F.PAGO.PROVEE = ''
 	
 	FN.PAGO.PROVEE.NAU = 'F.EB.SLV.PAGO.PROVEEDORES$NAU'
 	F.PAGO.PROVEE.NAU = ''
 	
 	FN.CUS = 'F.CUSTOMER'
 	F.CUS = ''
 	
 	FN.TABLE.PA = 'F.EB.SLV.GLOBAL.PARAM'
 	F.TABLE.PA = ''
 	
 	FN.ACC = 'F.ACCOUNT'
 	F.ACC  = ''
 	
 	FN.FT = 'F.FUNDS.TRANSFER'
 	F.FT = ''
 	
 	FN.FT.NAU = 'F.FUNDS.TRANSFER$NAU'
 	F.FT.NAU  = ''
 	;*Variables del SPOOL
	 V.FECHA.T24      	= ''
	 V.REFERENCIA.T24 	= ''
	 V.NOMBRE.PROVEEDOR = ''
	 V.CUENTA.DEPOSITO  = ''
	 V.NIT 				= ''
	 V.REFERENCIA.PAGO 	= ''
	 V.CONCEPTO         = ''
	 V.MONTO.DEPOSITADO = ''
RETURN

OPENFILE:
	CALL OPF(FN.PAGO.PROVEE, F.PAGO.PROVEE)
	CALL OPF(FN.CUS, F.CUS)
	CALL OPF(FN.TABLE.PA, F.TABLE.PA)
	CALL OPF(FN.ACC, F.ACC)
	CALL OPF(FN.FT, F.FT)
	CALL OPF(FN.FT.NAU, F.FT.NAU)
	CALL OPF(FN.PAGO.PROVEE.NAU, F.PAGO.PROVEE.NAU)
RETURN

PROCESS:
 
 Y.APP = 'FUNDS.TRANSFER'
 Y.FLD = 'LF.REF.P.PROVEE'
 CALL MULTI.GET.LOC.REF(Y.APP, Y.FLD, Y.POS)
 PosRefProveedor = Y.POS<1,1>
 
V.ID.APP.LOCAL = ID.NEW
 
 ;*Recuperando variable Record de la aplicacion local para realizar el F.WRITE y consultar variables para el SPOOL
 ;*===============================================================================================================
 CALL F.READ(FN.PAGO.PROVEE, V.ID.APP.LOCAL, RECORD.APP.LOCAL, F.PAGO.PROVEE, ERROR.APP.LOCAL)
 
 IF NOT(RECORD.APP.LOCAL) THEN
 CALL F.READ(FN.PAGO.PROVEE.NAU, V.ID.APP.LOCAL, RECORD.APP.LOCAL, F.PAGO.PROVEE.NAU, ERROR.APP.LOCAL)
 END
  ;*Consultando Registro para generar el SPOOL
 ;*==========================================
 V.FECHA.T24      	= RECORD.APP.LOCAL<EB.SLV44.DATE.TIME>
 V.REFERENCIA.T24 	= RECORD.APP.LOCAL<EB.SLV44.FT.PAGO>
 V.NOMBRE.PROVEEDOR = RECORD.APP.LOCAL<EB.SLV44.NOMBRE.PROVEEDOR>
 V.CUENTA.DEPOSITO  = RECORD.APP.LOCAL<EB.SLV44.CUENTA.CLIENTE>
 
 ;*Consultando Datos del Cliente
 ;*------------------------------------------------------------------
 CALL F.READ(FN.ACC, V.CUENTA.DEPOSITO, RECORD.ACC, F.ACC, ERROR.ACC)
 V.USUARIO = RECORD.ACC<AC.CUSTOMER>
 CALL F.READ(FN.CUS, V.USUARIO, RECORD.CUS, F.CUS, ERR.CUS)
 Y.APP = 'CUSTOMER'
 Y.FLD = 'LF.NIT'
 CALL MULTI.GET.LOC.REF(Y.APP, Y.FLD, Y.POS)
 PosNitProvee = Y.POS<1,1>
 
 V.NIT 				= RECORD.CUS<EB.CUS.LOCAL.REF><1,PosNitProvee>
 ;*------------------------------------------------------------------
 
 V.REFERENCIA.PAGO 	= V.ID.APP.LOCAL
 V.CONCEPTO         = RECORD.APP.LOCAL<EB.SLV44.DESCRIPCION>
 V.MONTO.DEPOSITADO = RECORD.APP.LOCAL<EB.SLV44.MONTO>
 
 ;*Autorizando FT NOTA DE ABONO
 GOSUB AUTH.FT.PAGO.PROVEE
 
 GOSUB SEND.APDF

RETURN

*** <region name= AUTH FT Pago de Proveedor> OFS.AUTH.FT.PAGO.PROVEE
*** <desc> </desc>
AUTH.FT.PAGO.PROVEE:

	;*Ejecutar Rutina para envio de OFS.OL
   ;*------------------------------------
    ID.PARAM.OFS 		= 'OFS.AUTH.FT.PAGO.PROVEE'
    TRANS.ID      		= V.REFERENCIA.T24
    R.FT.PAGO.PROVEEDOR = ''		
    
    CALL SLV.OFS.UTIL.OL.TRX(TRANS.ID, R.FT.PAGO.PROVEEDOR, ID.PARAM.OFS, Y.OUT) 

RETURN
*** </region>



*** <region name= Generar SPOOL>
*** <desc> </desc>
SEND.APDF:

;*Definiendo Directorio SPOOL - APDF
;*----------------------------------
	RUTA.SPOOL.ID = 'RUTA.SPOOL.FILES'
	CALL F.READ(FN.TABLE.PA, RUTA.SPOOL.ID, R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
	DIR.NAME.SPOOL = R.TABLE.PA<EB.SLV39.VALOR.PARAM> ;* ./SPOOL.FILES/
	CRT 'DIR.NAME.SPOOL>>':DIR.NAME.SPOOL

	RUTA.PDF.ID = 'RUTA.CONTRATO.PDF'
	CALL F.READ(FN.TABLE.PA, RUTA.PDF.ID, R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
	DIR.NAME.PDF = R.TABLE.PA<EB.SLV39.VALOR.PARAM>   ;* http://192.168.1.92/PDF/
	CRT 'DIR.NAME.PDF>>':DIR.NAME.PDF
	
	;*Armando el txt
	DATOS  = ';'
	
	;*Convirtiendo Fecha
	EQU LOCALZONE 			TO 'America/El_Salvador'
	IF V.FECHA.T24 THEN
		FINDSTR '.' IN V.FECHA.T24 SETTING Ap, Vp THEN
			Y.TIME = OCONV(LOCALTIME(V.FECHA.T24, LOCALZONE),"MTS")
			localZoneDate1 = OCONV(LOCALDATE(V.FECHA.T24,localZone),'D4/E') :'-':Y.TIME
		END ELSE
			Y.TIME = OCONV(V.FECHA.T24,"MT"):"00"
			localZoneDate1 = OCONV(LOCALDATE(V.FECHA.T24,localZone),'D4/E') :'-':Y.TIME
		END
	END 
	DATOS := 'V.FECHA.T24=':localZoneDate1:';'
	
	
	
	
	DATOS := 'V.REFERENCIA.T24=' :V.REFERENCIA.T24:';'	
	DATOS := 'V.NOMBRE.PROVEEDOR=':V.NOMBRE.PROVEEDOR:';'
	DATOS := 'V.NIT=':V.NIT:';'
	DATOS := 'V.REFERENCIA.PAGO=':V.REFERENCIA.PAGO:';'
	DATOS := 'V.CONCEPTO=':V.CONCEPTO:';'
	DATOS := 'V.CUENTA.DEPOSITO=':V.CUENTA.DEPOSITO:';'
	DATOS := 'V.MONTO.DEPOSITADO=':V.MONTO.DEPOSITADO:';'
    GOSUB PROCESS.PERFIL
RETURN

PROCESS.PERFIL:
*-----------------------------------------------------------------------------
    NAME.ID = 'NotaAbonoPagoProveedor':'-':V.ID.APP.LOCAL
    
    R.ID.SPOOL = NAME.ID : '.txt'
    R.ID.TXT   = NAME.ID : '.txt'
    R.ID.PDF   = NAME.ID : '.pdf'
	
	;* Codificar caracteres especiales
	DATOS = CHANGE(DATOS, 'Ñ', CHAR(465))
	DATOS = CHANGE(DATOS, 'ñ', CHAR(497))
	
	;*CRT "Ñ - ":CHAR(465)

    ;* tratamiento de letras tildadas o semejantes
	DATOS = CHANGE(DATOS, CHAR(193), 'A')
	DATOS = CHANGE(DATOS, CHAR(201), 'E')
	DATOS = CHANGE(DATOS, CHAR(205), 'I')
	DATOS = CHANGE(DATOS, CHAR(211), 'O')
	DATOS = CHANGE(DATOS, CHAR(218), 'U')
	DATOS = CHANGE(DATOS, CHAR(192), 'A')
	DATOS = CHANGE(DATOS, CHAR(200), 'E')
	DATOS = CHANGE(DATOS, CHAR(204), 'I')
	DATOS = CHANGE(DATOS, CHAR(210), 'O')
	DATOS = CHANGE(DATOS, CHAR(217), 'U')
	DATOS = CHANGE(DATOS, CHAR(220), 'U')
	DATOS = CHANGE(DATOS, CHAR(219), 'U')
	DATOS = CHANGE(DATOS, CHAR(214), 'O')
	DATOS = CHANGE(DATOS, CHAR(212), 'O')
	DATOS = CHANGE(DATOS, CHAR(207), 'I')
	DATOS = CHANGE(DATOS, CHAR(206), 'I') 
	DATOS = CHANGE(DATOS, CHAR(203), 'E')

	DATOS = CHANGE(DATOS, CHAR(225), 'A')
	DATOS = CHANGE(DATOS, CHAR(233), 'E')
	DATOS = CHANGE(DATOS, CHAR(237), 'I')
	DATOS = CHANGE(DATOS, CHAR(243), 'O')
	DATOS = CHANGE(DATOS, CHAR(250), 'U')
	
	DATOS = CHANGE(DATOS, '=;', '= ;')
	
	;* Creando el archivo principal    
    OPENSEQ DIR.NAME.SPOOL, R.ID.SPOOL TO SEQ.PTR 
   	ELSE
        CREATE SEQ.PTR 
        ELSE
          	ETEXT = 'No se puede crear el archivo.'
      		CALL STORE.END.ERROR
   		END
	END

	;* Escribiendo la cadena en el archivo principal
	;*----------------------------------------------
	WRITESEQ DATOS ON SEQ.PTR 
	ELSE
		ETEXT = 'No se pueden escribir los datos en el archivo.'
		CALL STORE.END.ERROR
	END

	;* Cerrando el archivo
	;*--------------------
	CLOSESEQ SEQ.PTR	 	
RETURN

*** </region>


END
