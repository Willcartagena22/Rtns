*-----------------------------------------------------------------------------
* <Rating>308</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.E.ACCOUNTS.DBCOLLECTOR(A.INFO)
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.AC.CHARGE.REQUEST
$INSERT I_F.ENQUIRY
$INSERT I_ENQUIRY.COMMON
*-----------------------------------------------------------------------------

GOSUB INIT
GOSUB PROCESS
RETURN

INIT:
RETURN

PROCESS:

LOCATE "COLECTOR.CODE" IN D.FIELDS<1> SETTING POS.FECHA.FIN THEN
   COLECTOR = D.RANGE.AND.VALUE<POS.FECHA.FIN>
END

GOSUB GET.ACCOUNTS.LIQUIDACION

LST.ACC = CHANGE(RESPUESTA.SERVICIO.EXTERNO,'|',FM)

CANTIDAD.CUENTAS = DCOUNT(LST.ACC,FM)

FOR I=1 TO CANTIDAD.CUENTAS-1
CUENTA   = LST.ACC<I>
NUM.ACC  = FIELD(CUENTA,'-',1)
DESC.ACC = FIELD(CUENTA,'-',2)

A.INFO<-1> = CUENTA:'*':NUM.ACC:'*':DESC.ACC

NEXT I

RETURN


GET.ACCOUNTS.LIQUIDACION:
    THIS.PACKAGE.CLASS ="com.bancoazul.collector.Collector";* "com.bancoazul.t24colecturia.ColectorPEXMWS"
    THIS.METHOD.CLT= "getAccountLiquidacion"
    CALLJ.ARGUMENTS.CLT = COLECTOR
    CALLJ.ERROR.SMS = " "
    CALLJ.RESPONSE.CLT = " "
;*LLAMADA AL METODO CALLJ
    CALL EB.CALLJ(THIS.PACKAGE.CLASS,THIS.METHOD.CLT,CALLJ.ARGUMENTS.CLT,CALLJ.RESPONSE.CLT,CALLJ.ERROR.CLT)
    RESPUESTA.SERVICIO.EXTERNO = CHANGE(CALLJ.RESPONSE.CLT,'"','')
RETURN

ESCRIBIR.ARCHIVO: 
DIR.NAME= 'COLECTORES'
R.ID   = 'SUBENQUIRY_LSTACCOUNT_':TODAY:'.txt'
;* hacer que escriba un archivo 
OPENSEQ DIR.NAME,R.ID TO SEQ.PTR 
        WRITESEQ TEXTO.ARCHIVO APPEND TO SEQ.PTR THEN
END 
CLOSESEQ SEQ.PTR 
RETURN

END
