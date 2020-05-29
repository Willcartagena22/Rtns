*-----------------------------------------------------------------------------
* <Rating>68</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.GET.ACCOUNT.DBCOLLECTOR
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.FUNDS.TRANSFER
*-----------------------------------------------------------------------------

    GOSUB INIT
    GOSUB PROCESS
    RETURN

INIT:
    RETURN

PROCESS:

    COLECTOR = FIELD(COMI,'-',1)

    THIS.PACKAGE.CLASS ="com.bancoazul.collector.Collector";* "com.bancoazul.t24colecturia.ColectorPEXMWS"
    THIS.METHOD.CLT= "getAccounts"
    CALLJ.ARGUMENTS.CLT = COLECTOR
    CALLJ.ERROR.SMS = " "
    CALLJ.RESPONSE.CLT = " "
;*LLAMADA AL METODO CALLJ
    CALL EB.CALLJ(THIS.PACKAGE.CLASS,THIS.METHOD.CLT,CALLJ.ARGUMENTS.CLT,CALLJ.RESPONSE.CLT,CALLJ.ERROR.CLT)
    RESPUESTA.SERVICIO.SYNTEX = CHANGE(CALLJ.RESPONSE.CLT,'"','')

    LST.VALUES   = CHANGE(RESPUESTA.SERVICIO.SYNTEX,'|',FM)
    VALUES.COUNT = DCOUNT(LST.VALUES,FM)
    
    FOR I=3 TO VALUES.COUNT-1
        VALUE = LST.VALUES<I,1>

        FINDSTR "-" IN VALUE SETTING Ap, Vp THEN

            IF FIELD(VALUE,'-',1) EQ 'RET' THEN
               CUENTA.TRANSITORIA = FIELD(VALUE,'-',2)
            END
            ELSE IF FIELD(VALUE,'-',1) EQ 'CLI' THEN
               CUENTA.CLIENTE =FIELD(VALUE,'-',2)
        END
    END

    NEXT I
    
    TEXTO.ARCHIVO = 'CUENTA.TRANSITORIA >':CUENTA.TRANSITORIA
    GOSUB ESCRIBIR.ARCHIVO
    
    TEXTO.ARCHIVO = 'CUENTA.CLIENTE >':CUENTA.CLIENTE
    GOSUB ESCRIBIR.ARCHIVO
    
    R.NEW(FT.DEBIT.ACCT.NO) = CUENTA.TRANSITORIA
    R.NEW(FT.CREDIT.ACCT.NO) = CUENTA.CLIENTE
    
    RETURN

ESCRIBIR.ARCHIVO:
    DIR.NAME= 'COLECTORES'
    R.ID   = 'ACCOUNT_DBCOLLECTOR_':TODAY:'.txt'
;* hacer que escriba un archivo
    OPENSEQ DIR.NAME,R.ID TO SEQ.PTR
    WRITESEQ TEXTO.ARCHIVO APPEND TO SEQ.PTR THEN
    END
    CLOSESEQ SEQ.PTR
    RETURN

    END
