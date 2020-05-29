*-----------------------------------------------------------------------------
* <Rating>-93</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.PAGO.ACH.SALIENTE
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.FUNDS.TRANSFER
$INSERT I_F.ACCOUNT
$INSERT I_F.CUSTOMER
$INSERT I_F.EB.SLV.CAT.COL.VEN
$INSERT I_F.EB.SLV.RANG.PARAM
$INSERT I_F.BENEFICIARY
$INSERT I_F.EB.SLV.TIPO.ACC.ACH 
$INSERT I_F.EB.SLV.TRANSACTION.ACH
$INSERT I_F.EB.SLV.BANKS.DETAILS
*-----------------------------------------------------------------------------

GOSUB INIT
GOSUB OPF

IF R.NEW(EB.SLV98.STATUS) EQ '1' AND V$FUNCTION EQ 'I' THEN
	GOSUB PROCESS
END

RETURN

INIT:
	FN.ACC	= 'F.ACCOUNT'
	F.ACC	= ''
	FN.CUS 	= 'F.CUSTOMER'
	F.CUS	= ''
	FN.CAT.COL.VEN = 'F.EB.SLV.CAT.COL.VEN'
    F.CAT.COL.VEN  = ''	
    FN.RANG.PARAM = 'F.EB.SLV.RANG.PARAM'
	F.RANG.PARAM = ''
	FN.TABLE.BEN = 'F.BENEFICIARY' 
	F.TABLE.BEN = '' 
	FN.TIPO.ACC.ACH = 'F.EB.SLV.TIPO.ACC.ACH'
	F.TIPO.ACC.ACH	= ''
	FN.TXN.ACH = 'F.EB.SLV.TRANSACTION.ACH'
	F.TXN.ACH = ''
	FN.BANKS = 'F.EB.SLV.BANKS.DETAILS'
	F.BANKS	=	''
	
	GOSUB GET.MULTI.LOC.REF
	COLECTOR.CODE = '02'
	
	EQU CTA.COR TO 'CTA.COR'
	EQU CTA.AHO TO 'CTA.AHO'	
	EQU RANGO.ACC 		TO 'SLV.CAT.CUENTAS'
	EQU ID.BANCO.AZUL TO 'BA119020W6LF5'
	EQU TIPO.MOV TO '0C'

RETURN


OPF:
	CALL OPF(FN.ACC,F.ACC)
	CALL OPF(FN.CUS,F.CUS)
	CALL OPF(FN.CAT.COL.VEN,F.CAT.COL.VEN)
	CALL OPF(FN.RANG.PARAM, F.RANG.PARAM)
	CALL OPF(FN.TABLE.BEN, F.TABLE.BEN) 
	CALL OPF(FN.TIPO.ACC.ACH, F.TIPO.ACC.ACH) 
	CALL OPF(FN.TXN.ACH, F.TXN.ACH)  	 
RETURN

PROCESS:
	 	 				 	 
	 FT.AZUL.ID.CLI			= R.NEW(EB.SLV98.CLIENTE.AZUL)	  
	 FT.AZUL.CTA.CLI		= R.NEW(EB.SLV98.CTA.CLIENTE.AZUL)
	 FT.RECEP.ID.BANCO		= R.NEW(EB.SLV98.BANCO.BENEF)
	 
	 CALL F.READ(FN.BANKS,ID.BANCO.AZUL,R.BANKS,F.BANKS,E.BANKS) 
	 FT.AZUL.ID.BANCO		= R.BANKS<EB.SLV.BANK.ID.BANK>
	 
	 GOSUB GET.ACC.INFO  ;*FT.AZUL.ACC.TYPE 
	 	 
	 ;*CALL F.READ(FN.CUS,FT.AZUL.ID.CLI,R.CUS,F.CUS,E.CUS)	 
	 FT.AZUL.NOMBRE.CLI 	=  R.BANKS<EB.SLV.BANK.NAME.BANK>
	 
	 FT.RECEP.ID.CLI  		= R.NEW(EB.SLV98.CLIENTE.BENEF)
	 FT.RECEP.CTA.CLI		= R.NEW(EB.SLV98.CTA.CLIENTE.BENEF)	 
	 FT.RECEP.NOMBRE.CLI	= R.NEW(EB.SLV98.NAME.BENEF)	  
	 FT.RECEP.ACCT.TYPE 	= R.NEW(EB.SLV98.TIPO.PROD.BENEF)
	 FT.PROPOSITO.PAGO 		= R.NEW(EB.SLV98.PROPOSITO)	 
	 FT.AZUL.ID.TRAN 		= R.NEW(EB.SLV98.ID.FT)
	 FT.MONTO				= R.NEW(EB.SLV98.MONTO)
	 FT.TIPO.ACH.TXN 		= LEFT(TIPO.MOV,1)
	 FT.TIPO.ACH.MOV		= RIGHT(TIPO.MOV,1)
	 
	 IF FT.TIPO.ACH.MOV EQ 'C' THEN 
	 	FT.TIPO.ACH.MOV = 'Credit'
	 END
	 ELSE 
	 	FT.TIPO.ACH.MOV = 'Debit'
	 END 
	 
	 TRAMA.ACH = FT.RECEP.ID.BANCO:"~" 		;*1 Id Banco receptor
	 TRAMA.ACH:= FT.PROPOSITO.PAGO:"~"		;*2 Proposito de la transaccion
	 TRAMA.ACH:= FT.AZUL.ID.BANCO:"~"		;*3 ID de banco azul
	 TRAMA.ACH:= FT.AZUL.ID.BANCO:"~"		;*4 Numero de cliente de banco azul
	 TRAMA.ACH:= FT.AZUL.CTA.CLI:"~"		;*5 Numero de cta de cliente banco azul
	 TRAMA.ACH:= FT.AZUL.ACC.TYPE:"~"		;*6 Tipo de cta cliente banco azul
	 TRAMA.ACH:= FT.AZUL.NOMBRE.CLI:"~"		;*7 Nombre Banco azul
	 TRAMA.ACH:= FT.RECEP.ID.BANCO:"~"		;*8 Numero de banco receptor
	 TRAMA.ACH:= FT.RECEP.CTA.CLI:"~"		;*9 Numero de cuenta beneficiario banco receptor
	 TRAMA.ACH:= FT.RECEP.ACCT.TYPE:"~"		;*10 Tipo de cuenta beneficiario banco receptor
	 TRAMA.ACH:= FT.RECEP.NOMBRE.CLI:"~"	;*11 Nombre de beneficiario banco receptor
	 TRAMA.ACH:= "~"						;*12 
	 TRAMA.ACH:= FT.AZUL.ID.TRAN:"~" 		;*13 ID FT
	 TRAMA.ACH:= FT.MONTO:"~"				;*14 Monto Transaccion
	 TRAMA.ACH:= FT.TIPO.ACH.TXN:"~"		;*15 Tipo de transaccion inmediata
	 TRAMA.ACH:= FT.RECEP.ID.BANCO:"~"
	 TRAMA.ACH:= "~"
	 TRAMA.ACH:= FT.TIPO.ACH.MOV
	 
	 ;*______________________________________________
	 TEXTO.ARCHIVO = TRAMA.ACH
	 GOSUB ESCRIBIR.ARCHIVO				
	 ;*______________________________________________	
	 
	 GOSUB GET.ID.COLLECTOR
	 
    ;* idTransaccion~nombreAplicacion~idColector~idCanal~idServicio~idAgencia~tipoPlantilla~idOperacion~RESTO DE DATOS PROPIOS DEL SERVICIO
	Y.PARAMETRO.ARGUMENT 	= ID.TRANSACTION:'~':'ACHS':'~':'12':'~':'TCIB':'~':'ACHS':'~':'SV0010001':'~':'XML':'~':'1':'~'
	Y.PARAMETRO.ARGUMENT   := TRAMA.ACH
	
	THIS.PACKAGE.CLASS ="com.bancoazul.collector.Collector";* "com.bancoazul.t24colecturia.ColectorPEXMWS"
    THIS.METHOD.CLT= "setConnectAsync"
    CALLJ.ARGUMENTS.CLT = Y.PARAMETRO.ARGUMENT
    CALLJ.ERROR.SMS = " "
    CALLJ.RESPONSE.CLT = " "
	;*LLAMADA AL METODO CALLJ
    CALL EB.CALLJ(THIS.PACKAGE.CLASS,THIS.METHOD.CLT,CALLJ.ARGUMENTS.CLT,CALLJ.RESPONSE.CLT,CALLJ.ERROR.CLT)   
    
	 TEXTO.ARCHIVO = CALLJ.RESPONSE.CLT
	 GOSUB ESCRIBIR.ARCHIVO				
	 ;*______________________________________________	 
	 
RETURN

GET.MULTI.LOC.REF:
    Y.APPL = "FUNDS.TRANSFER"
    Y.FIELD = "LF.TCE.NARR": VM :"LF.AMOUNT":VM:"ACH.BANCO.BENEF":VM:"ACH.PROP":VM:"ACH.BANCO.AZUL":VM:"ACH.TIPO.TXN":VM:"LF.ID.COL":VM:"ACH.BANCO.AZUL":VM:"ID.BENEF.ACH":VM:"ACCT.BENEF.ACH" 
    Y.POS = ""
    CALL MULTI.GET.LOC.REF(Y.APPL,Y.FIELD,Y.POS)

    Y.LF.TCE.NARR 			= Y.POS<1,1>
    Y.LF.AMOUNT 			= Y.POS<1,2>
    Y.ACH.BANCO.BENEF		= Y.POS<1,3>
    Y.ACH.PROP 				= Y.POS<1,4>
   	Y.ACH.BANCO.AZUL		= Y.POS<1,5>
    Y.ACH.TIPO.TXN			= Y.POS<1,6>
    Y.LF.ID.COL				= Y.POS<1,7>
    Y.ID.BANCO.AZUL			= Y.POS<1,8>
    Y.BENEF.ACH				= Y.POS<1,9>
    Y.ACCT.BENEF.ACH		= Y.POS<1,10>
      
RETURN

GET.ID.COLLECTOR:
	;* Primero hay que obtener el Id de la transacción para CINTEX
	
	;* idColector~idCanal
	Y.PARAMETRO.ARGUMENT = '12':'~':'ACHS'
	THIS.PACKAGE.CLASS 	= "com.bancoazul.collector.Collector";* "com.bancoazul.t24colecturia.ColectorPEXMWS"
    THIS.METHOD.CLT		= "getID"
    CALLJ.ARGUMENTS.CLT = Y.PARAMETRO.ARGUMENT
    CALLJ.ERROR.SMS 	= " "
    CALLJ.RESPONSE.CLT 	= " "
	;*LLAMADA AL METODO CALLJ
    CALL EB.CALLJ(THIS.PACKAGE.CLASS,THIS.METHOD.CLT,CALLJ.ARGUMENTS.CLT,CALLJ.RESPONSE.CLT,CALLJ.ERROR.CLT)
    
    ;* Asignar el Id recuperado
    ID.TRANSACTION = FIELD(CALLJ.RESPONSE.CLT,'|',3)

RETURN

GET.ACC.INFO:

	;*Account
	CALL F.READ(FN.ACC, FT.AZUL.CTA.CLI, REC.ACC.CURR, F.ACC, ERR.ACC.CURR)

	Y.CATEGORY =  REC.ACC.CURR<AC.CATEGORY>
	
	;*Obtiene Parametria de Categories para Cuentas de ahorro y corriente
	CALL F.READ(FN.RANG.PARAM, RANGO.ACC, R.PARAM, F.RANG.PARAM, ERR.RANG.PARAM)
	;*Cuentas Ahorro
    FIND CTA.AHO IN R.PARAM<EB.SLV56.ID.RANGO> SETTING Ap, Vp THEN
    	;*Validar si el Category de la cuenta se encuentra en el Rango
       	IF Y.CATEGORY GE R.PARAM<EB.SLV56.RANGO.INF><Ap, Vp> AND Y.CATEGORY LE R.PARAM<EB.SLV56.RANGO.SUP><Ap, Vp> THEN
			FT.AZUL.ACC.TYPE 		= "2"
		END
 	END 
 	;*Cuentas Corrientes
 	FIND CTA.COR IN R.PARAM<EB.SLV56.ID.RANGO> SETTING Ap, Vp THEN
    	;*Validar si el Category de la cuenta se encuentra en el Rango
       	IF Y.CATEGORY GE R.PARAM<EB.SLV56.RANGO.INF><Ap, Vp> AND Y.CATEGORY LE R.PARAM<EB.SLV56.RANGO.SUP><Ap, Vp> THEN
			FT.AZUL.ACC.TYPE 		= "1"
		END
 	END
RETURN

ESCRIBIR.ARCHIVO:
    DIR.NAME= 'ACH'
    R.ID   = 'ACH.PAGO.SALIENTE.':TODAY:'.txt'
;* hacer que escriba un archivo

    OPENSEQ DIR.NAME,R.ID TO SEQ.PTR
    WRITESEQ TEXTO.ARCHIVO APPEND TO SEQ.PTR THEN
    END
    CLOSESEQ SEQ.PTR
RETURN

END
