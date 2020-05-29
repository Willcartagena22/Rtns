*-----------------------------------------------------------------------------
* <Rating>-53</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.OPCION.ACH
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
$INSERT I_F.EB.SLV.ACH.CONNECTION
*-----------------------------------------------------------------------------
GOSUB INIT
GOSUB OPF
 OPCION.SELECT 		= R.NEW(EB.SLV78.OPCION)
 TEXTO.ARCHIVO = "OPCION.SELECT -> ":OPCION.SELECT
 GOSUB ESCRIBIR.ARCHIVO
 TRAMA.ACH = ""
 IF OPCION.SELECT EQ 'ECHO' THEN
 	OPERACION = "2"
 	GOSUB EXE.ACH
 END
  IF OPCION.SELECT EQ 'CONSULTA.LOCAL' THEN
 	OPERACION = "3"
 	TRAMA.ACH = R.NEW(EB.SLV78.ID.MENSAJE)
 	GOSUB EXE.ACH
 END
 IF OPCION.SELECT EQ 'CONSULTA.SERVIDOR' THEN
 	OPERACION = "4"
 	TRAMA.ACH = R.NEW(EB.SLV78.ID.MENSAJE)
	GOSUB EXE.ACH
 END
GOSUB PROCESS

INIT:
	FN.ACC	= 'F.ACCOUNT'
	F.ACC	= ''
	FN.CUS 	= 'F.CUSTOMER'
	F.CUS	= ''
	FN.CAT.COL.VEN = 'F.EB.SLV.CAT.COL.VEN'
    F.CAT.COL.VEN  = ''	

COLECTOR.CODE = '02'	

RETURN

OPF:
	CALL OPF(FN.ACC,F.ACC)
	CALL OPF(FN.CUS,F.CUS)
	CALL OPF(FN.CAT.COL.VEN,F.CAT.COL.VEN)
	
RETURN

PROCESS:
RETURN


EXE.ACH:
 	TEXTO.ARCHIVO = "******************* INIT ECHO.ACH *****************************"
 	GOSUB ESCRIBIR.ARCHIVO
	;* idTransaccion~nombreAplicacion~idColector~idCanal~idServicio~idAgencia~tipoPlantilla~idOperacion~RESTO DE DATOS PROPIOS DEL SERVICIO
	Y.PARAMETRO.ARGUMENT 	= ID.TRANSACTION:'~':'ACHS':'~':'12':'~':'TCIB':'~':'ACHS':'~':'SV0010001':'~':'XML':'~':OPERACION:'~'
	Y.PARAMETRO.ARGUMENT   := TRAMA.ACH
	
	THIS.PACKAGE.CLASS ="com.bancoazul.collector.Collector";* "com.bancoazul.t24colecturia.ColectorPEXMWS"
    THIS.METHOD.CLT= "setConnect"
    CALLJ.ARGUMENTS.CLT = Y.PARAMETRO.ARGUMENT
    CALLJ.ERROR.SMS = " "
    CALLJ.RESPONSE.CLT = " "
	;*LLAMADA AL METODO CALLJ
    CALL EB.CALLJ(THIS.PACKAGE.CLASS,THIS.METHOD.CLT,CALLJ.ARGUMENTS.CLT,CALLJ.RESPONSE.CLT,CALLJ.ERROR.CLT)
    
    PLECA = "\"
    CHANGE '\"' TO "" IN CALLJ.RESPONSE.CLT
    CHANGE PLECA TO "" IN CALLJ.RESPONSE.CLT
    ;*
     R.NEW(EB.SLV78.RESPUESTA) = "Respuesta: ":CALLJ.RESPONSE.CLT
	 ;*______________________________________________
	 TEXTO.ARCHIVO = CALLJ.RESPONSE.CLT
	 GOSUB ESCRIBIR.ARCHIVO				
	 ;*______________________________________________
RETURN

ESCRIBIR.ARCHIVO:
    DIR.NAME= 'ACH'
    R.ID   = 'ACH.TEST.CONNECT.':TODAY:'.txt'
;* hacer que escriba un archivo

    OPENSEQ DIR.NAME,R.ID TO SEQ.PTR
    WRITESEQ TEXTO.ARCHIVO APPEND TO SEQ.PTR THEN
    END
    CLOSESEQ SEQ.PTR
RETURN

END
