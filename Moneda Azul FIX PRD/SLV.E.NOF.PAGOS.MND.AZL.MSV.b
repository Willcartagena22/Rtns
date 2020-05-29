*-----------------------------------------------------------------------------
* <Rating>-133</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.E.NOF.PAGOS.MND.AZL.MSV(ENQ.DATA)
*-----------------------------------------------------------------------------
* Nombre: SLV.E.NOF.PAGOS.MND.AZL.MSV
* Descripcion: Rutina encargada de devolver informacion sobre las cargas y Pagos de Moneda Azul Masiva desde Banca Empresas.
*----------------------------------------------------------------------------------------------------
* Version	Autor		Fecha		Comentario
*----------------------------------------------------------------------------------------------------
* 1.0		iTurcios	26.04.19	Version inicial
* 1.1		eescobar	07.05.19	Agrega GOSUB MODIFICAR_ESTADO        
*----------------------------------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_ENQUIRY.COMMON 
$INSERT I_F.EB.SLV.MASTER.MONEDA.AZUL
$INSERT I_F.EB.SLV.ITEMS.MONEDA.AZUL
$INSERT I_F.CUSTOMER
*-----------------------------------------------------------------------------
GOSUB INIT
GOSUB PROCESS
RETURN

INIT:
	FN.MASTER = 'F.EB.SLV.MASTER.MONEDA.AZUL'
	F.MASTER  = ''
	CALL OPF(FN.MASTER, F.MASTER)
	
	FN.MASTER.NAU = 'F.EB.SLV.MASTER.MONEDA.AZUL$NAU'
	F.MASTER.NAU  = ''
	CALL OPF(FN.MASTER.NAU, F.MASTER.NAU)
	
	FN.ITEMS = 'F.EB.SLV.ITEMS.MONEDA.AZUL'
	F.ITEMS  = ''
	CALL OPF(FN.ITEMS, F.ITEMS)
	
	FN.CUSTOMER = 'F.CUSTOMER'
	F.CUSTOMER  = ''
	CALL OPF(FN.CUSTOMER, F.CUSTOMER)
	
	;*ID Empresa
	LOCATE 'ID.EMPRESA' IN D.FIELDS<1> SETTING ITEM.POS THEN 
		ID.EMPRESA = D.RANGE.AND.VALUE<ITEM.POS> 
    END 
    
    ;*Id Usuario de Banca Empresas
	LOCATE 'ID.USUARIO' IN D.FIELDS<1> SETTING ITEM.POS THEN 
		ID.USUARIO = D.RANGE.AND.VALUE<ITEM.POS> 
    END
    
    ;*Estado del Master: CREADO, AUTORIZADO, PENDIENTE o RECHAZADO
	LOCATE 'ESTADO' IN D.FIELDS<1> SETTING ITEM.POS THEN 
		ESTADO = D.RANGE.AND.VALUE<ITEM.POS> 
    END
   
    ;*Fecha desde
	LOCATE 'DATE.FROM' IN D.FIELDS<1> SETTING ITEM.POS THEN 
		DATE.FROM = D.RANGE.AND.VALUE<ITEM.POS> 
    END 
    
    ;*Fecha hasta
    LOCATE 'DATE.TO' IN D.FIELDS<1> SETTING ITEM.POS THEN  
		DATE.TO = D.RANGE.AND.VALUE<ITEM.POS> 
    END   
    
    ;*Constantes
    EQU ORI.PAGO TO "Pago de Planilla MonedAzul por Banca en Linea - Empresas"     
    EQU CANAL.PAGO TO "Agencia"    
    EQU USD TO "USD"
    EQU DUI TO "DUI"
    EQU VENCIDO TO "VENCIDO"
         
    ;*Debug
    ID.EMPRESA = '111827'
    ID.USUARIO = 'ENAJARRO01'
    ESTADO     = 'AUTORIZADO'
    DATE.FROM  = '20190501' 
    DATE.TO    = '20190501'
RETURN

PROCESS:
	GOSUB GET.IDS.MASTER
	
	COUNT.MASTER = DCOUNT(ID.MASTER, FM) - 1
	
	FOR MAS = 1 TO COUNT.MASTER 
		CALL F.READ(FN.MASTER, ID.MASTER<MAS>, REC.MASTER, F.MASTER, ERR.MASTER)
		IF NOT(REC.MASTER) THEN ;*Lectura a tabla INAU
			CALL F.READ(FN.MASTER.NAU, ID.MASTER<MAS>, REC.MASTER, F.MASTER.NAU, ERR.MASTER.INAU)
		END

		GOSUB READ.WRITE.INFO
	NEXT MAS
	
RETURN

READ.WRITE.INFO:
	GOSUB CLEAR.VARS
	 
	;*Informacion TFS
	Y.TFS			= ID.TFS
	Y.TT			= ID.TT
	Y.FEC.VALOR.DEB = ""
	Y.FEC.HORA		= ""
	
	;*Informacion Master
	Y.CTA.DEBITO	= REC.MASTER<EB.SLV44.CUENTA>
	Y.MONTO.MASTER	= REC.MASTER<EB.SLV44.MONTO>
	Y.USUARIO.TC    = REC.MASTER<EB.SLV44.USUARIO.CARGA>
	Y.CLIENTE.DEB   = REC.MASTER<EB.SLV44.ID.CUSTOMER>
	GOSUB GET.RAZON.SOCIAL 
	Y.NOMBRE.DEBITO = Y.OUT.RAZON.SOCIAL
	Y.FECHA.CREACION = REC.MASTER<EB.SLV44.FECHA.HORA>
	Y.FECHA.VENC 	 = REC.MASTER<EB.SLV44.FECHA.FIN>[7,9] : '/' : REC.MASTER<EB.SLV44.FECHA.FIN>[5,2] : '/' : REC.MASTER<EB.SLV44.FECHA.FIN>[0,4]
	Y.FECHA.VENC.AUX = REC.MASTER<EB.SLV44.FECHA.FIN>
	Y.NO.PAGOS		 = REC.MASTER<EB.SLV44.NO.PAGOS>
	Y.ESTADO.MASTER  = REC.MASTER<EB.SLV44.ESTADO>
	
	;*Informacion Items
	SELECT.ITEMS = "SELECT " : FN.ITEMS : " WITH @ID LIKE " : ID.MASTER<MAS> : "..."
	CALL EB.READLIST (SELECT.ITEMS, ID.ITEMS, '', NO.OF.ITEMS, ERR.ITEMS)
	
	FOR IT = 1 TO NO.OF.ITEMS 
		CALL F.READ(FN.ITEMS, ID.ITEMS<IT>, REC.ITEM, F.ITEMS, ERR.ITEM)
				
		Y.MONTO.CREDITO  = REC.ITEM<EB.IMA.MONTO>
		Y.NOMBRE.CREDITO = REC.ITEM<EB.IMA.NOMBRE>
		Y.TELEFONO	     = REC.ITEM<EB.IMA.MOVIL>
		Y.TIPO.DOCUMENTO = DUI : '-' : REC.ITEM<EB.IMA.DOCUMENTO>
		Y.TFS 			 = REC.ITEM<EB.IMA.REFERENCIA.PAGO>
		
		IF TODAY GT Y.FECHA.VENC.AUX THEN
			Y.ESTATUS = VENCIDO
		END ELSE 
			Y.ESTATUS = REC.ITEM<EB.IMA.ESTADO>
		END	
		
		GOSUB MODIFICAR_ESTADO	
		
		GOSUB SEND.TO.ENQ.DATA			
	NEXT IT  
RETURN 

SEND.TO.ENQ.DATA:
		;*Construccion de arreglo
	 	STR.PAGOS	 = ''
	 	
	 	;*Informacion del Master
		STR.PAGOS	:= 	ID.MASTER<MAS>	 : '*' 	;*1>Id del Master
	 	STR.PAGOS	:= 	Y.CTA.DEBITO	 : '*' 	;*2>Cuenta de Debito
	 	STR.PAGOS	:= 	USD				 : '*' 	;*3>Moneda
	 	STR.PAGOS	:= 	Y.MONTO.MASTER	 : '*' 	;*4>Monto Debito
	 	STR.PAGOS	:= 	USD				 : '*' 	;*5>Moneda
	 	STR.PAGOS	:= 	Y.USUARIO.TC	 : '*' 	;*6>Usuario de Banca que realiza el envio
	 	STR.PAGOS	:=  Y.CLIENTE.DEB	 : '*' 	;*7>Unico del cliente debito
	 	STR.PAGOS	:=  Y.NOMBRE.DEBITO	 : '*' 	;*8>Nombre del cliente debito
	 	STR.PAGOS	:=  Y.FECHA.CREACION : '*' 	;*9>Fecha de creacion del Pago
	 	STR.PAGOS	:=  Y.FECHA.VENC	 : '*' 	;*10>Fecha de vencimiento del Pago
	 	STR.PAGOS	:= 	ORI.PAGO		 : '*' 	;*11>Origen del Pago
	 	STR.PAGOS	:= 	Y.CANAL.PAGO	 : '*' 	;*12>Canal del Pago  CANAL.PAGO
	 	STR.PAGOS	:=  Y.NO.PAGOS  	 : '*' 	;*13>Cantidad de Pagos
	 	STR.PAGOS	:=	Y.ESTADO.MASTER  : '*'	;*14>Estado del master
	 	;*Informacion del Item 	
	 	STR.PAGOS	:= 	Y.MONTO.CREDITO	 : '*' 	;*15>Monto credito
	 	STR.PAGOS	:=  Y.NOMBRE.CREDITO : '*' 	;*16>Nombre del cliente credito  	
	 	STR.PAGOS	:=  Y.TELEFONO		 : '*' 	;*17>Telefono del Beneficiario
	 	STR.PAGOS	:=  Y.TIPO.DOCUMENTO : '*' 	;*18>Tipo de documento de identidad del beneficiario
	 	STR.PAGOS	:=  Y.ESTATUS		 : '*' 	;*19>Estatus del item
	 	
		STR.PAGOS	:=  ID.ITEMS<IT>	 : '*'  ;*20>Id del Item
	 	;*Informacion de TFS 	
	 	STR.PAGOS	:= 	Y.TFS			 : '*' 	;*21>Id TFS
	 	STR.PAGOS	:= 	Y.TT			 : '*' 	;*22>Id TT
	 	STR.PAGOS	:= 	Y.FEC.VALOR.DEB	 : '*' 	;*23>Fecha valor debito
	 	STR.PAGOS	:= 	Y.FEC.HORA		 	 	;*24>Fecha y hora del cobro del debito
	 	
	 	ENQ.DATA<-1> = STR.PAGOS
	 	
*	 	CRT ENQ.DATA
RETURN

CLEAR.VARS:	
	;*Informacion Master
	Y.CTA.DEBITO	 = ''
	Y.MONTO.MASTER	 = ''
	Y.USUARIO.TC     = ''
	Y.CLIENTE.DEB    = ''		 
	Y.NOMBRE.DEBITO  = ''
	Y.FECHA.CREACION = ''
	Y.FECHA.VENC 	 = ''	 
	Y.NO.PAGOS		 = ''	
	Y.ESTADO.MASTER	 = ''
	;*Informacion Item + TFS	
	Y.MONTO.CREDITO  = ''
	Y.NOMBRE.CREDITO = ''
	Y.TELEFONO	     = ''
	Y.TIPO.DOCUMENTO = ''
	Y.ESTATUS 		 = ''
	Y.TFS			 = ''
	Y.TT			 = ''
	Y.FEC.VALOR.DEB  = ''
	Y.FEC.HORA		 = ''
	Y.CANAL.PAGO	 = ''
	
RETURN

GET.IDS.MASTER:
	;*Obteniendo ids en LIVE
	SELECT.MASTER = "SELECT " : FN.MASTER 
	
	IF ID.EMPRESA THEN
		SELECT.MASTER := " WITH ID.CUSTOMER EQ '" : ID.EMPRESA : "'"
	END	
	
	IF ID.USUARIO THEN 
		IF ID.EMPRESA THEN
			SELECT.MASTER := " AND USUARIO.CARGA EQ '" : ID.USUARIO : "'"
		END ELSE 
			SELECT.MASTER := " WITH USUARIO.CARGA EQ '" : ID.USUARIO : "'"
		END	
	END
	
	IF ESTADO THEN
		IF ID.EMPRESA OR ID.USUARIO THEN 
			SELECT.MASTER := " AND ESTADO EQ '" : ESTADO : "'"
		END ELSE
			SELECT.MASTER := " WITH ESTADO EQ '" : ESTADO : "'"
		END
	END
	
	IF DATE.FROM AND DATE.TO THEN
		IF ID.EMPRESA OR ID.USUARIO OR ESTADO THEN
			SELECT.MASTER := " AND FECHA.CARGA GE '" : DATE.FROM : "' AND FECHA.CARGA LE '" :  DATE.TO : "'"
		END ELSE
			SELECT.MASTER := " WITH FECHA.CARGA GE '" : DATE.FROM : "' AND FECHA.CARGA LE '" :  DATE.TO : "'"		
		END	
	END
	
	CALL EB.READLIST (SELECT.MASTER, ID.MASTER.LIVE, '', NO.OF.MASTER.LIVE, ERR.MASTER.LIVE)
		
	;*Obteniendo ids en INAU
	SELECT.MASTER.INAU = "SELECT " : FN.MASTER.NAU 
	
	IF ID.EMPRESA THEN
		SELECT.MASTER.INAU := " WITH ID.CUSTOMER EQ '" : ID.EMPRESA : "'"
	END	
	
	IF ID.USUARIO THEN 
		IF ID.EMPRESA THEN
			SELECT.MASTER.INAU := " AND USUARIO.CARGA EQ '" : ID.USUARIO : "'"
		END ELSE 
			SELECT.MASTER.INAU := " WITH USUARIO.CARGA EQ '" : ID.USUARIO : "'"
		END	
	END
	
	IF ESTADO THEN
		IF ID.EMPRESA OR ID.USUARIO THEN 
			SELECT.MASTER.INAU := " AND ESTADO EQ '" : ESTADO : "'"
		END ELSE
			SELECT.MASTER.INAU := " WITH ESTADO EQ '" : ESTADO : "'"
		END
	END
	
	IF DATE.FROM AND DATE.TO THEN
		IF ID.EMPRESA OR ID.USUARIO OR ESTADO THEN
			SELECT.MASTER.INAU := " AND FECHA.CARGA GE '" : DATE.FROM : "' AND FECHA.CARGA LE '" : DATE.TO : "'"
		END ELSE
			SELECT.MASTER.INAU := " WITH FECHA.CARGA GE '" : DATE.FROM : "' AND FECHA.CARGA LE '" : DATE.TO	: "'"		
		END	
	END

	CALL EB.READLIST (SELECT.MASTER.INAU, ID.MASTER.INAU, '', NO.OF.MASTER.INAU, ERR.MASTER.INAU)
	
	ID.MASTER = ID.MASTER.LIVE : FM : ID.MASTER.INAU
RETURN

GET.RAZON.SOCIAL:
	CALL GET.LOC.REF ('CUSTOMER','LF.RAZON.SOCIAL',POS.RAZON.SOCIAL)
	CALL F.READ(FN.CUSTOMER, Y.CLIENTE.DEB, REC.CUSTOMER, F.CUSTOMER, ERR.CUSTOMER)
	Y.OUT.RAZON.SOCIAL = TRIM(REC.CUSTOMER<EB.CUS.LOCAL.REF><1,POS.RAZON.SOCIAL>)
RETURN

MODIFICAR_ESTADO:
*Y.MASTER.FIN = BULK.MASTER.REC<EB.SLV44.FECHA.FIN>
	IF Y.ESTATUS EQ 'PAGADO' THEN
		Y.ESTATUS = 'Pagado'
		Y.CANAL.PAGO = CANAL.PAGO
		Y.FEC.HORA = REC.ITEM<EB.IMA.DATE.TIME> 
		GOSUB PARSE_DATETIME ;*Se Parsea		
	END
	
	IF Y.ESTATUS EQ 'AUTORIZADO' THEN
		IF TODAY GT Y.FECHA.VENC THEN
			Y.ESTATUS = 'Vencido'
			Y.CANAL.PAGO = ''
			Y.FEC.HORA = ''
		END	
		ELSE
			Y.ESTATUS = 'Pendiente de Cobro'	
			Y.CANAL.PAGO = ''
			Y.FEC.HORA = '' 
		END
	END
	
	IF Y.ESTATUS EQ 'PENDIENTE' THEN
		Y.ESTATUS = 'Pendiente de autorizar'
		Y.FEC.HORA = ''	 
		Y.CANAL.PAGO = ''
	END
	
	IF Y.ESTATUS EQ 'LIBERADO' THEN
		Y.ESTATUS = 'Pagado'
		Y.CANAL.PAGO = CANAL.PAGO
		Y.FEC.HORA = REC.ITEM<EB.IMA.DATE.TIME> 
		GOSUB PARSE_DATETIME ;*Se Parsea	 
	END		
RETURN

PARSE_DATETIME:
	utcDateTime =  Y.FEC.HORA
    localZoneDate1 = OCONV(LOCALDATE(utcDateTime,localZone),'D4/C') ;*08/22/1970 mes dia año
*   localZoneDate2 = localZoneDate1[7,4]:localZoneDate1[1,2]:localZoneDate1[4,2]
	localZoneDate2 = localZoneDate1[4,2]:'/':localZoneDate1[1,2]:'/':localZoneDate1[7,4]
    localZoneTime1=OCONV(LOCALTIME(utcDateTime,localZone),'MTS')
    Y.FEC.HORA = localZoneDate2:' ':localZoneTime1                           
RETURN

;*Archivo para Revisión de Errores
WRITE_LOG_FILE:
	DIR.NAME = 'SIF.OUT'
	R.ID = 'LOG.PAGOS.MONEDA.AZUL' : '.txt'
 
	OPENSEQ DIR.NAME, R.ID TO SEQ.PTR
		WRITESEQ Y.TXT APPEND TO SEQ.PTR THEN
		END
	CLOSESEQ SEQ.PTR 
RETURN

END
