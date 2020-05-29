*-----------------------------------------------------------------------------
* <Rating>-33</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE ENQ.SLV.LIST.COL.CATALOG(A.INFO)
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_ENQUIRY.COMMON
*-----------------------------------------------------------------------------

GOSUB INIT
GOSUB PROCCESS
RETURN

INIT:
RETURN

PROCCESS:
CATALOGO = "0"
THIS.PACKAGE.CLASS ="com.bancoazul.collector.Collector";* "com.bancoazul.t24colecturia.ColectorPEXMWS"
THIS.METHOD.CLT= "getListCatalog"
CALLJ.ARGUMENTS.CLT = CATALOGO
CALLJ.ERROR.SMS = " "
CALLJ.RESPONSE.CLT = " "
;*LLAMADA AL METODO CALLJ
CALL EB.CALLJ(THIS.PACKAGE.CLASS,THIS.METHOD.CLT,CALLJ.ARGUMENTS.CLT,CALLJ.RESPONSE.CLT,CALLJ.ERROR.CLT)
RESPUESTA.SERVICIO.COLECTORES = CHANGE(CALLJ.RESPONSE.CLT,'"','')

*RESPUESTA.SERVICIO = "1~MEDIOS DE PAGO|2~CANALES DE PAGO|3~IMPUESTOS|4~CALENDARIO PAGO|";

CATALOG.LST = CHANGE(RESPUESTA.SERVICIO.COLECTORES,'|',FM)

LOOP
REMOVE Y.CATALOG FROM CATALOG.LST SETTING POS.CTLG
WHILE Y.CATALOG:POS.CTLG
      CATALOGO   = FIELD(Y.CATALOG,'~',1):'-':FIELD(Y.CATALOG,'~',2)
      TEXTO.ARCHIVO = '>>>':CATALOGO
      GOSUB ESCRIBIR.ARCHIVO 
      A.INFO<-1> = CATALOGO:'*':FIELD(Y.CATALOG,'~',1):'*':FIELD(Y.CATALOG,'~',2)
REPEAT

TEXTO.ARCHIVO = '------':A.INFO 
GOSUB ESCRIBIR.ARCHIVO

RETURN

ESCRIBIR.ARCHIVO: 
DIR.NAME= 'COLECTORES'
R.ID   = 'ENQUIRY_LSTCATALOG_':TODAY:'.txt'
;* hacer que escriba un archivo 
OPENSEQ DIR.NAME,R.ID TO SEQ.PTR 
        WRITESEQ TEXTO.ARCHIVO APPEND TO SEQ.PTR THEN
END 
CLOSESEQ SEQ.PTR 
RETURN

END
