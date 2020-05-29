*-----------------------------------------------------------------------------------
* <Rating>-180</Rating>
*-----------------------------------------------------------------------------------
* Nombre:      SLV.V.LIQ.CTAS.PEX.VEN.b
* Descripcion: Rutina para liquidacion automatica de las cuentas internas de
*              recepcion de pagos de colectores en ventanilla de PUNTO EXPRESS.
*-----------------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------------
* Version	Autor			Fecha			Comentario
*-----------------------------------------------------------------------------------
* 1.0		CMONTERROSA		28062017		Version inicial
* 1.1       CMONTERROSA		09082017		Genera Nota Abono aunque sea monto cero
* 1.1       CMONTERROSA		04092017		Creacion de Archivo LOG en UD/COLECTORES
*-----------------------------------------------------------------------------------
    SUBROUTINE SLV.V.LIQ.CTAS.PEX.VEN
*-----------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.EB.SLV.COLECTOR
    $INSERT I_F.EB.SLV.ACCOUNT.PARAM
    $INSERT I_F.COMPANY
    $INSERT I_F.DATES
    $INSERT I_F.ACCOUNT
    $INSERT I_F.TELLER.FINANCIAL.SERVICES
    $INSERT I_F.TELLER
    $INSERT I_F.FUNDS.TRANSFER
;*-----------------------------------------------------------------------------

    GOSUB INIT
    GOSUB OPENFILE
    GOSUB PROCESS
    
    RETURN


INIT:
;*-----------------------------------------------------------------------------

   ;*Declarar variables FN y F para archivos a utilizar
   ;*--------------------------------------------------
    FN.SLV.COLECTOR  = 'F.EB.SLV.COLECTOR'
    F.SLV.COLECTOR   = ''
    
    FN.ACCOUNT.PARAM = 'F.EB.SLV.ACCOUNT.PARAM'
    F.ACCOUNT.PARAM  = ''

    FN.COMPANY       = 'F.COMPANY'
    F.COMPANY        = ''

    FN.TFS           = 'F.TELLER.FINANCIAL.SERVICES'
    F.TFS            = ''

    FN.TELLER        = 'F.TELLER'
    F.TELLER         = ''
    
    FN.DATES 	     = 'F.DATES'
    F.DATES  	     = ''
    
    FN.ACCOUNT 	     = 'F.ACCOUNT'
    F.ACCOUNT        = ''
    
    FN.FT            = 'F.FUNDS.TRANSFER'
    F.FT             = ''    

   ;*Declaracion de variables de trabajo
   ;*-----------------------------------
    Y.RUTINE              = 'SLV.V.LIQ.CTAS.PEX.VEN'
    Y.FECHA               = OCONV(DATE(),"D/")
    Y.FECHA.TRX           = TODAY    
    Y.FECHA.RPT           = Y.FECHA[7,4] : Y.FECHA[1,2] : Y.FECHA[4,2]
    Y.FECHA.PAGO          = Y.FECHA[4,2]:'/':Y.FECHA[1,2]:'/':Y.FECHA[7,4]
    Y.FECHA.HOY           = TIMEDATE()
    	
    Y.ID.COLECTOR.PEX     = '' ;*Id del Colector Punto Express
    Y.CTA.INTERNA.PEX     = '' ;*Cuenta Interna BNK Punto Express
    Y.CTA.CLIENTE.PEX     = '' ;*Cuenta Cliente Punto Express 
    Y.COMPANY.COL.PEX     = '' ;*Agencia Colector Punto Express
    Y.MONTO.AGENCIA.PEX   = 0  ;*Monto Acumulado de TFS por cuenta Agencia 
    Y.MONTO.TOTAL.PEX     = 0  ;*Total a Abonar a Cuenta Cliente Punto Express
    
    Y.CTA.INTERNA.AGENCIA = '' ;*Cuenta Interna de Agencia para recepcion de colectores en caja
    Y.LIST.CONTROL.TFS    = '' ;*TFS de las cuentas de colectores en el dia
    Y.LIST.CONTROL.CTA    = '' ;*Monto total de TFS para cada cuenta en el dia para generar OFS
    Y.LIST.SALDOS.CTA     = '' ;*Saldos de las cuentas antes de cierre
    
    Y.SALDO.CTA           = 0
    Y.SALDO.CTA.INTERNA   = 0
    Y.NUM.ABONOS.BNK      = 0
             
    
    RETURN
;*-----------------------------------------------------------------------------
;*FIN - INIT
;*-----------------------------------------------------------------------------


OPENFILE:
;*-----------------------------------------------------------------------------
    CALL OPF(FN.SLV.COLECTOR, F.SLV.COLECTOR)
    CALL OPF(FN.ACCOUNT.PARAM, F.ACCOUNT.PARAM)
    CALL OPF(FN.COMPANY, F.COMPANY)
    CALL OPF(FN.TFS, F.TFS)
    CALL OPF(FN.ACCOUNT, F.ACCOUNT)
    CALL OPF(FN.DATES, F.DATES)
    CALL OPF(FN.FT, F.FT)
    RETURN
;*-----------------------------------------------------------------------------
;* FIN - OPENFILE
;*-----------------------------------------------------------------------------


PROCESS:
;*-----------------------------------------------------------------------------    
	CRT '<<' : Y.RUTINE : ' - INICIO>>'
	
	;*Creacion de Archivo Log
	;*-----------------------
	GOSUB CREATELOG

    ;*Consulta de parametros de Punto Express
    ;*---------------------------------------    
    GOSUB OBTENER.CTAS.PEX
    
    ;*SELECT para consulta de Agencias de T24
    ;*----------------------------------------
    Y.COMPANY.SELECT   = "SELECT " : FN.COMPANY            
    CALL EB.READLIST(Y.COMPANY.SELECT, Y.COMPANY.LIST.IDS, Y.COMPANY.SELECTED, Y.COMPANY.NUM.REGS, Y.COMPANY.SYS.RESP)        
    
    ;*Recorremos todas las agencias de T24
    ;*------------------------------------ 
    CRT 'OBTENIENDO MONTO DE TRANSACCIONES EN VENTANILLA DEL DIA'
    LOOP
    REMOVE Y.COMPANY.ITEM FROM Y.COMPANY.LIST.IDS SETTING Y.COMPANY.DELIM
    WHILE Y.COMPANY.ITEM  NE ''
        
        ;*Reinicio Monto Agencia Acumulado
        Y.MONTO.AGENCIA.PEX = 0
        Y.CTA.VALIDO = 'S'
            
        ;*Construccion de ID de busqueda en  ACCOUNT.PARAMS        
        Y.ID.COMPANY       = Y.COMPANY.ITEM
        Y.ID.ACCOUNT.PARAM = Y.ID.COMPANY : '.' : Y.ID.COLECTOR.PEX : '.VEN'                    
        
        ;*Obtenemos la cuenta de agencia del registro Y.ID.ACCOUNT.PARAM        
        CALL F.READ(FN.ACCOUNT.PARAM, Y.ID.ACCOUNT.PARAM, REG.ACCOUNT.PARAM, F.ACCOUNT.PARAM, ERR.ACCOUNT.PARAM)                
        Y.CTA.INTERNA.AGENCIA = REG.ACCOUNT.PARAM<EB.SLV47.RESERVADO5>               
        
        ;*Evaluar que la cuenta no sea BNK
        ;*IF (Y.CTA.INTERNA.AGENCIA EQ Y.CTA.INTERNA.PEX ) THEN
        ;*  Y.CTA.VALIDO = 'N'
        ;*END                                               
        
        ;*Evaluar si el registro ya fue agregado en lista de control de CTAS
        FINDSTR Y.CTA.INTERNA.AGENCIA IN Y.LIST.CONTROL.CTA SETTING Ap2, Vp2 THEN
			Y.CTA.AGREGADO = 'S'
		END ELSE
			Y.CTA.AGREGADO = 'N'
		END
        
        IF Y.CTA.AGREGADO EQ 'N' AND Y.CTA.INTERNA.AGENCIA NE '' THEN        
            ;*Obtener Saldo de la Cuenta antes del cierre
            GOSUB SALDO.CTA
            ;*Inserta registro de control de Saldo de la Cta
            Y.LIST.SALDOS.CTA<-1> = Y.CTA.INTERNA.AGENCIA : '*' : Y.SALDO.CTA                                    
                                              
            ;*Obtener total de TFS por agencia            
            GOSUB TRANS.TFS        
                
            ;*Inserta registro de control de Cta Agencia          
            Y.LIST.CONTROL.CTA<-1> = Y.CTA.INTERNA.AGENCIA : '*' : Y.MONTO.AGENCIA.PEX
                                                                                                           
        END                        
    REPEAT 		
            
    ;*Obtenemos Saldo de la cuenta BNK para Cuadre
	;*---------------------------------------------
	Y.CTA.INTERNA.AGENCIA   = Y.CTA.INTERNA.PEX
	Y.ID.COMPANY            = Y.COMPANY.CTA.PEX
	GOSUB SALDO.CTA
	Y.SALDO.CTA.INTERNA.PEX = Y.SALDO.CTA
	       
	;*Traslado de saldo desde cuentas de agencias a cuenta BNK
	;*--------------------------------------------------------
	CRT 'TRANSFIRIENDO DESDE CUENTAS DE AGENCIA HACIA CUENTA INTERNA BNK'
	GOSUB TRANSF.MONTO.AGENCIAS			
	
	;*Saldo final de cuenta BNK
	Y.SALDO.CTA.INTERNA = Y.SALDO.CTA.INTERNA.PEX + Y.MONTO.OFS.AGENCIAS
	
	;*Traslado de saldo desde cuenta BNK hacia cuenta Cliente Punto Express
	;*---------------------------------------------------------------------
	CRT 'TRANSFIRIENDO DESDE CUENTA INTERNA BNK HACIA CUENTA PUNTO XPRESS'
	GOSUB TRANSF.MONTO.COLECTOR
	
	;*Generacion de Spool para reporte de cuentas internas
	;*----------------------------------------------------
	CRT 'GENERANDO REPORTE DE SALDOS DE CUENTAS DE AGENCIA'
	GOSUB RPT.SALDOS.CTAS
	
	CRT '<<' : Y.RUTINE : ' - FINALIZADO>>'
RETURN
;*-----------------------------------------------------------------------------
;* FIN - PROCESS
;*-----------------------------------------------------------------------------


OBTENER.CTAS.PEX:
;*----------------------------------------------------------------------------        
    ;*SELECT para consulta de paramatros de Punto Expres en aplicacion EB.SLV.COLECTOR
    ;*--------------------------------------------------------------------------------
    Y.SLV.COLECTOR.SELECT   = "SELECT " : FN.SLV.COLECTOR
    Y.SLV.COLECTOR.SELECT  := "  WITH NOMBRE.COLECTOR LIKE %PuntoExpress%"
    	
	;*Ejecucion de SELECT	
    CALL EB.READLIST(Y.SLV.COLECTOR.SELECT, Y.SLV.COLECTOR.LIST.IDS, Y.SLV.COLECTOR.SELECTED, Y.SLV.COLECTOR.NUM.REGS, Y.SLV.COLECTOR.SYS.RESP)
    
    ;*Id de registro de Punto Express
    ;*--------------------------------
    Y.ID.SLV.COLECTOR = Y.SLV.COLECTOR.LIST.IDS        
             
    ;*Lectura de campos del registro en aplicacion local EB.SLV.COLECTOR
	;*------------------------------------------------------------------
    CALL F.READ(FN.SLV.COLECTOR, Y.ID.SLV.COLECTOR, REG.SLV.COLECTOR, F.SLV.COLECTOR, ERR.SLV.COLECTOR)            
    
    ;*Definicion de Valores parametrizados para Punto Express
    ;*-------------------------------------------------------
    Y.ID.COLECTOR.PEX = Y.ID.SLV.COLECTOR                          ;*ID DE COLECTOR DE PUNTO EXPRESS VENTANILLA
    Y.CTA.INTERNA.PEX = REG.SLV.COLECTOR<EB.CL.NUMERO.CUENTA.TRAN> ;*CUENTA BNK     DE PUNTO EXPRESS VENTANILLA
    Y.CTA.CLIENTE.PEX = REG.SLV.COLECTOR<EB.CL.NUMERO.CUENTA>      ;*CUENTA CLIENTE DE PUNTO EXPRESS VENTANILLA
    Y.COMPANY.CTA.PEX = REG.SLV.COLECTOR<EB.CL.CO.CODE>            ;*AGENCIA COLECTORE DE PUNTO EXPRESS VENTANILLA    	

RETURN    
*-----------------------------------------------------------------------------
* FIN - OBTENER.CTA.PEX
*-----------------------------------------------------------------------------

SALDO.CTA:
;*----------------------------------------------------------------------------        
    Y.ID.ACCOUNT = Y.CTA.INTERNA.AGENCIA
    Y.ID.DATES   = Y.ID.COMPANY          
    Y.SALDO.CTA  = 0
     
    ;*Obtener la ultima fecha cerrada   
    CALL F.READ(FN.DATES, Y.ID.DATES, REG.DATES, F.DATES, ERR.DATES)            
    Y.TODAY           = REG.DATES<EB.DAT.TODAY>
    Y.LAST.PERIOD.END = REG.DATES<EB.DAT.LAST.PERIOD.END>
    
    ;*Obtener informacion de la cuenta 	 
    CALL F.READ(FN.ACCOUNT, Y.ID.ACCOUNT, REG.ACCOUNT, F.ACCOUNT, ERR.ACCOUNT)
    
    ;*Obtener el saldo de la cuenta	
	CALL EB.GET.ACCT.BALANCE(Y.ID.ACCOUNT, REG.ACCOUNT,'BOOKING', Y.TODAY,'', BALANCE, CREDIT.MVMT, DEBIT.MVMT, ERR.MSG)	
	
	Y.SALDO.CTA = ABS(BALANCE)	        	
RETURN    
*-----------------------------------------------------------------------------
* FIN - SALDO.CTA
*-----------------------------------------------------------------------------


TRANS.TFS:
;*----------------------------------------------------------------------------    
    Y.TFS.SELECT   = "SELECT " : FN.TFS
    Y.TFS.SELECT  := "  WITH CO.CODE         EQ " : "'" : Y.ID.COMPANY          : "'"    
    Y.TFS.SELECT  := "   AND PRIMARY.ACCOUNT EQ " : "'" : Y.CTA.INTERNA.AGENCIA : "'"
    Y.TFS.SELECT  := "   AND BOOKING.DATE    EQ " : "'" : Y.FECHA.TRX           : "'"
    
    CALL EB.READLIST(Y.TFS.SELECT, Y.TFS.LIST.IDS, Y.TFS.SELECTED, Y.TFS.NUM.REGS, Y.TFS.SYS.RESP)

    LOOP
    REMOVE Y.TFS.ITEM FROM Y.TFS.LIST.IDS SETTING Y.TFS.DELIM
    WHILE Y.TFS.ITEM  NE ''        
        Y.TFS.VALIDO = 'S'
        Y.ID.TFS     = Y.TFS.ITEM
         
        ;*Obtenemos los campos del registro Y.ID.TFS 
        CALL F.READ(FN.TFS, Y.ID.TFS, REG.TFS, F.TFS, ERR.TFS)
        
        Y.STATUS.TFS   = REG.TFS<TFS.RECORD.STATUS>
		Y.REV.MARK.TFS = REG.TFS<TFS.REVERSAL.MARK>
		
		;*Evaluar el estado de los registros 
		IF Y.STATUS.TFS EQ 'INAU' OR Y.STATUS.TFS EQ 'INAO' OR Y.STATUS.TFS EQ 'RNAU' OR Y.STATUS.TFS EQ 'RNAO' OR Y.STATUS.TFS EQ 'REVE' OR Y.REV.MARK.TFS EQ 'R' THEN
		   Y.TFS.VALIDO = 'N'
		END
		
		;*Evaluar si el registro ya fue agregado en lista de control de TFS
        FINDSTR Y.ID.TFS IN Y.LIST.CONTROL.TFS SETTING Ap1, Vp1 THEN
			Y.TFS.AGREGADO = 'S'
		END ELSE
			Y.TFS.AGREGADO = 'N'
		END
		
        IF Y.TFS.VALIDO EQ 'S' AND Y.TFS.AGREGADO EQ 'N' THEN           
           ;*Calcular cantidad de TT de la TFS
           ;*---------------------------------
           Y.NUM.TRX.TFS = DCOUNT(REG.TFS<TFS.UNDERLYING>, VM)
           FOR I = 1 TO Y.NUM.TRX.TFS
               IF FIELD(REG.TFS<TFS.RUNNING.TOTAL>, VM, I) GT 0 THEN                   
                   Y.MONTO.AGENCIA.PEX   += FIELD(REG.TFS<TFS.RUNNING.TOTAL>, VM, I)                   
               END     
                   Y.VAR.CONTROL.TFS      =       Y.CTA.INTERNA.AGENCIA
                   Y.VAR.CONTROL.TFS     := '*' : Y.ID.TFS
                   Y.VAR.CONTROL.TFS     := '*' : FIELD(REG.TFS<TFS.UNDERLYING>, VM, I)
                   Y.VAR.CONTROL.TFS     := '*' : FIELD(REG.TFS<TFS.AMOUNT>, VM, I)
                   ;*Y.VAR.CONTROL.TFS     := '*' : FIELD(REG.TFS<TFS.RUNNING.TOTAL>, VM, I)                   
                   
                   Y.LIST.CONTROL.TFS<-1> = Y.VAR.CONTROL.TFS                                                                                                           
           NEXT I
        END   	       		
    REPEAT
    Y.MONTO.TOTAL.PEX += Y.MONTO.AGENCIA.PEX       
RETURN    
*-----------------------------------------------------------------------------
* FIN - TRANS.TFS
*-----------------------------------------------------------------------------


TRANSF.MONTO.AGENCIAS:
;*----------------------------------------------------------------------------       
    Y.MONTO.OFS.AGENCIAS = 0
    Y.NUM.ABONOS.BNK = DCOUNT(Y.LIST.CONTROL.CTA,FM)        
    FOR J=1 TO Y.NUM.ABONOS.BNK            
        Y.CTA.INTERNA.CREDITO = Y.CTA.INTERNA.PEX
        Y.CTA.INTERNA.DEBITO  = FIELD(Y.LIST.CONTROL.CTA<J>, '*', 1)
        Y.MONTO.TRANSFERENCIA = FIELD(Y.LIST.CONTROL.CTA<J>, '*', 2)
        Y.DESCR.TRANSFERENCIA = 'ABONO.CUENTA.BNK'
        
        IF (Y.CTA.INTERNA.CREDITO NE Y.CTA.INTERNA.DEBITO) AND (Y.MONTO.TRANSFERENCIA GT 0) THEN            
            GOSUB FT.ABONO.OFS                                     
            Y.MONTO.OFS.AGENCIAS += Y.MONTO.TRANSFERENCIA
            
            ;*Escritura en archivo LOG
            ;*-------------------------            
            Y.STRING.TO.BE.WRITTEN  = 'TRANSF. DESDE: ' : Y.CTA.INTERNA.DEBITO : ' HACIA: ' : Y.CTA.INTERNA.CREDITO : '| MONTO: ' : Y.MONTO.TRANSFERENCIA ;*: CHAR(10)            
            GOSUB WRITELOG             
            ;*-------------------------
             
        END 
    NEXT J
RETURN    
*-----------------------------------------------------------------------------
* FIN - TRANSF.MONTO.AGENCIAS
*-----------------------------------------------------------------------------

TRANSF.MONTO.COLECTOR:
;*----------------------------------------------------------------------------    
    Y.CTA.INTERNA.CREDITO = Y.CTA.CLIENTE.PEX
    Y.CTA.INTERNA.DEBITO  = Y.CTA.INTERNA.PEX
    Y.MONTO.TRANSFERENCIA = Y.MONTO.TOTAL.PEX
    Y.DESCR.TRANSFERENCIA = 'ABONO.CLIENTE.PEX'
    ;*CRT 'TRANSFERENCIA A CUENTA DE PUNTO EXPRESS...'
    IF (Y.MONTO.TRANSFERENCIA GT 0) THEN                                
        GOSUB FT.ABONO.OFS
        
        ;*Escritura en archivo LOG
        ;*-------------------------    
        Y.STRING.TO.BE.WRITTEN  = '--------------------------------'  : ' ' : CHAR(10)        
        Y.STRING.TO.BE.WRITTEN := 'TRANSF. DESDE: ' : Y.CTA.INTERNA.DEBITO : ' HACIA: ' : Y.CTA.INTERNA.CREDITO : '| MONTO: ' : Y.MONTO.TRANSFERENCIA ;* : CHAR(10)
        GOSUB WRITELOG             
        ;*-------------------------
    END
    ;*IF Y.MONTO.TOTAL.PEX GT 0 THEN
        ;*Generacion de Nota de Abono
        CALL SLV.NOTA.ABONO.COL.VEN(Y.MONTO.TOTAL.PEX, Y.CTA.CLIENTE.PEX,'PUNTOXPRESS','NotaAbonoVentanillaPtoXpress','PEX')
    ;*END    
RETURN    
*-----------------------------------------------------------------------------
* FIN - TRANSF.MONTO.COLECTOR
*-----------------------------------------------------------------------------

FT.ABONO.OFS:
;*----------------------------------------------------------------------------    
    ;*Seteo de Variables a enviar en FT
    Y.DEBIT.ACCT.NO    = Y.CTA.INTERNA.DEBITO
    Y.CREDIT.ACCT.NO   = Y.CTA.INTERNA.CREDITO
    Y.DEBIT.AMOUNT     = Y.MONTO.TRANSFERENCIA
    Y.DEBIT.CURRENCY   = 'USD'
    Y.ORDERING.CUST    = Y.DESCR.TRANSFERENCIA
    Y.ORDERING.BANK    = Y.DESCR.TRANSFERENCIA
    Y.TRANSACTION.TYPE = 'AC66'
                               
    ;*Creacion de Registro de FT para envio de OFS
    REG.FT = ''
	REG.FT<FT.DEBIT.ACCT.NO>    =  Y.DEBIT.ACCT.NO
 	REG.FT<FT.CREDIT.ACCT.NO>   =  Y.CREDIT.ACCT.NO
	REG.FT<FT.DEBIT.CURRENCY>   =  Y.DEBIT.CURRENCY
	REG.FT<FT.DEBIT.AMOUNT>     =  Y.DEBIT.AMOUNT
	REG.FT<FT.ORDERING.BANK>    =  Y.ORDERING.BANK
	REG.FT<FT.ORDERING.CUST>    =  Y.ORDERING.CUST
	REG.FT<FT.TRANSACTION.TYPE> =  Y.TRANSACTION.TYPE
	
    ;*Enviando campo local que se utilizara como identificador
    CALL GET.LOC.REF('FUNDS.TRANSFER','LF.ID.OFS.COLV', LocPosIdOfsColV)
    REG.FT<FT.LOCAL.REF,LocPosIdOfsColV> = Y.ID.COLECTOR.PEX
    
    ;*Variables para envio de OFS
    ID.PARAM.OFS = 'OFS.I.FT.ABONO.COL'
    TRANS.ID     = ''
    Y.OUT        = ''
    
    ;*Envio de OFS
    CALL SLV.UTIL.OFS.TRX(TRANS.ID, REG.FT, ID.PARAM.OFS, Y.OUT)
	
RETURN    
*-----------------------------------------------------------------------------
* FIN - FT.ABONO.OFS
*-----------------------------------------------------------------------------


RPT.SALDOS.CTAS:
;*----------------------------------------------------------------------------        
    Y.NUM.CTAS  = DCOUNT(Y.LIST.SALDOS.CTA,FM)
    MONTO.TOTAL = 0
    
    FECHA = Y.FECHA.PAGO
    CALL SLV.UTIL.GET.FECHA.FORMATO.1(FECHA)
    FECHA.FORMAT = FECHA
    
    FOR K=1 TO Y.NUM.CTAS
        CUENTA       = FIELD(Y.LIST.SALDOS.CTA<K>, '*', 1)
        MONTO        = FIELD(Y.LIST.SALDOS.CTA<K>, '*', 2)
        IF CUENTA NE Y.CTA.INTERNA.PEX THEN
            
            MONTO.TOTAL += MONTO
        
            ;*Formatear el Monto        
            CALL SLV.UTIL.FORMATO.MONEDA(MONTO)
            MONTO.FORMATEADO = MONTO 
              
            ;*Agrega Registro a Spool
            SpoolData  := 'TblDatos' : K : '=' : CUENTA : '*$*' : MONTO.FORMATEADO : ';'
        END
        ELSE
            IF MONTO GT 0 THEN
                MONTO.TOTAL += MONTO
        
                ;*Formatear el Monto        
                CALL SLV.UTIL.FORMATO.MONEDA(MONTO)
                MONTO.FORMATEADO = MONTO 
              
                ;*Agrega Registro a Spool
                SpoolData  := 'TblDatos' : K : '=' : CUENTA : '*$*' : MONTO.FORMATEADO : ';'
            END
        END                                                   
    NEXT K
    
    ;*Formatear Monto Total
    CALL SLV.UTIL.FORMATO.MONEDA(MONTO.TOTAL)
    MONTO.TOTAL.FORMAT = MONTO.TOTAL
    
    SpoolData := 'MontoTotal='  : MONTO.TOTAL.FORMAT  : ';'
    SpoolData := 'NumCtaBNK='   : Y.CTA.INTERNA.PEX   : ';'
    SpoolData := 'SaldoCtaBNK=' : Y.SALDO.CTA.INTERNA : ';'
    SpoolData := 'Fecha='       : FECHA.FORMAT        : ';'
    
    ;*CRT SpoolData
    
    plantillaRpt = 'RptCtasVentanillaPtoXpress'
    idArchivo    = plantillaRpt : '-' : Y.FECHA.RPT : '.txt'
    direcArchivo = 'SPOOL.FILES'
                
    DELETESEQ direcArchivo, idArchivo
    OPENSEQ direcArchivo, idArchivo TO SEQ.PTR 
    ELSE
	    CREATE SEQ.PTR 
	    ELSE
            CRT 'No se puede crear el archivo.'
	    END
    END
       
    WRITESEQ SpoolData ON SEQ.PTR 
    ELSE 
	    CRT 'No se pueden escribir los datos en el archivo.' 		
	END 
	;*Escritura en archivo LOG
    ;*-------------------------    
    Y.STRING.TO.BE.WRITTEN  = '--------------------------------'  : CHAR(10)
    Y.STRING.TO.BE.WRITTEN := ' REPORTE DE SALDOS'                : CHAR(10)
    Y.STRING.TO.BE.WRITTEN := '--------------------------------'  : CHAR(10)        
    Y.STRING.TO.BE.WRITTEN := SpoolData : CHAR(10)
    GOSUB WRITELOG
    GOSUB CLOSELOG             
    ;*-------------------------           
RETURN    
*-----------------------------------------------------------------------------
* FIN - RPT.SALDOS.CTAS
*-----------------------------------------------------------------------------

CREATELOG:
*-----------------------------------------------------------------------------
    FILE.DATE = TODAY
    DIR.NAME  = 'COLECTORES'    
    FILE.NAME = FILE.DATE : '_LiqVentanillaPtoXpress.txt'    
    Y.STRING.TO.BE.WRITTEN   = '--------------------------------'  : ' ' : CHAR(10)
    Y.STRING.TO.BE.WRITTEN  := '<LOG_RUTINE: ' : Y.RUTINE          : '>' : CHAR(10)
    Y.STRING.TO.BE.WRITTEN  := '<FECHA     : ' : Y.FECHA.HOY       : '>' : CHAR(10)
    Y.STRING.TO.BE.WRITTEN  := '--------------------------------'  : ' ' ;*: CHAR(10)
	
    DELETESEQ DIR.NAME, FILE.NAME 
    OPENSEQ DIR.NAME, FILE.NAME TO Y.FILE.SEQ 
    ELSE
        CREATE  Y.FILE.SEQ 
        ELSE
        	CRT 'UNABLE TO CREATE RECORD IN FILE'
    	END
    END
    WRITESEQ Y.STRING.TO.BE.WRITTEN APPEND TO Y.FILE.SEQ
    ELSE
    	CRT 'UNABLE TO WRITE TO THE FILE'
    END
 RETURN
*-----------------------------------------------------------------------------
* FIN - OPENLOG
*-----------------------------------------------------------------------------

WRITELOG:
*-----------------------------------------------------------------------------
    WRITESEQ Y.STRING.TO.BE.WRITTEN APPEND TO Y.FILE.SEQ
    ELSE
    	CRT 'UNABLE TO WRITE TO THE FILE'
    END                   
RETURN
*-----------------------------------------------------------------------------
* FIN - WRITELOG
*-----------------------------------------------------------------------------


CLOSELOG:
*-----------------------------------------------------------------------------
	WEOFSEQ Y.FILE.SEQ 
	CLOSESEQ Y.FILE.SEQ
RETURN
*-----------------------------------------------------------------------------
* FIN - CLOSELOG
*-----------------------------------------------------------------------------

END
