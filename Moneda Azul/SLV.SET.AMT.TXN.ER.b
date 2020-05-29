*-----------------------------------------------------------------------------
* <Rating>175</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.SET.AMT.TXN.ER
*-----------------------------------------------------------------------------
*
* Nombre: SLV.SET.AMT.TXN.ER
* Descripción: Rutina que evalua si la version y las txn aplican para liberacion
*-----------------------------------------------------------------------------
* Modification History :
* Autor		      Fecha		 Comentario
* JHenriquez	15.02.2017	Initial Code
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.TELLER.FINANCIAL.SERVICES
    $INSERT I_SLV.AMT.LIOF.COMMON
    $INSERT I_F.ACCOUNT

*-----------------------------------------------------------------------------

    GOSUB INI
    GOSUB OPENFILE
    GOSUB PROCESS
    GOSUB VALIDA.VERSION.TXN
    RETURN

INI:
    FN.TFS = 'F.TELLER.FINANCIAL.SERVICES'
    F.TFS = ''

    FN.ACC = 'F.ACCOUNT'
    F.ACC = ''

*** <region name= INICIALIZAR VARIABLES>
    Y.VERSION = ''
    OUT.MV.TXN = ''

    Y.WORKING.BALANCE = ''
    Y.ACC = ''
    Y.AMT.TXN = ''
    Y.FORMA.PAGO = ''
    Y.APLICA.ER = ''

    Y.NO.TXN = ''

    Y.ID = ID.NEW
*** </region name= INICIALIZAR VARIABLES>

    RETURN

OPENFILE:
    CALL OPF(FN.TFS,F.TFS)
    CALL OPF(FN.ACC,F.ACC)
    RETURN

VALIDA.VERSION.TXN:

    Y.BANDERA.CONTROL = 1
    Y.VERSION =  APPLICATION:PGM.VERSION
    CALL SLV.GET.ER.TXN.PARAM(Y.VERSION,OUT.MV.TXN)

    Y.NO.TXN = DCOUNT(R.NEW(TFS.TRANSACTION),VM)

    FOR J = 1 TO Y.NO.TXN
        RECORD.ACC = ''
        Y.WORKING.BALANCE = ''
        Y.FORMA.PAGO = ''
        Y.ACC = ''
        Y.AMT.LIOF = ''
        Y.FORMA.PAGO = ''
        Y.AMT.TXN = ''
        Y.NO.CHEQUE = ''
        Y.ACC.PRIMARY = ''
        
        Y.ACC.PRIMARY = R.NEW(TFS.PRIMARY.ACCOUNT)
        Y.FORMA.PAGO = R.NEW(TFS.TRANSACTION)<1,J>
        Y.AMT.TXN = R.NEW(TFS.AMOUNT)<1,J>
        Y.ACC = R.NEW(TFS.SURROGATE.AC)<1,J>
        Y.NO.CHEQUE =  R.NEW(TFS.CHEQUE.NUMBER)<1,J>
        
        CALL GET.LOC.REF ('TELLER.FINANCIAL.SERVICES','LF.MNT.IMP',PosMntLiof)
        Y.AMT.LIOF = R.NEW(TFS.LOCAL.REF)<1,PosMntLiof,J> ;* MONTO LIOF
        
        ;*si es ACuentaPropia es retiro ----> ACC.PRIMARY (-) **** SURROGATE (+)
        ;*----------------------------------------------------------------------
        IF Y.FORMA.PAGO EQ 'ACuentaPropia' OR Y.FORMA.PAGO EQ 'ACuentaTercero' THEN
        Y.ACC = Y.ACC.PRIMARY
        END
        
        CALL F.READ(FN.ACC,Y.ACC,RECORD.ACC,F.ACC,ERR.ACC)
        Y.WORKING.BALANCE = RECORD.ACC<AC.WORKING.BALANCE>

        STR.LCK.EVT.ER = ''
        STR.LCK.EVT.ER := Y.WORKING.BALANCE :'*'
        STR.LCK.EVT.ER := Y.FORMA.PAGO      :'*'
        STR.LCK.EVT.ER := Y.AMT.TXN         :'*'
        STR.LCK.EVT.ER := Y.ACC             :'*' 

        FIND Y.FORMA.PAGO IN OUT.MV.TXN SETTING Ap, Vp THEN
        Y.APLICA.ER = 'Y'
        STR.LCK.EVT.ER := Y.APLICA.ER       :'*'
    END
    ELSE
    Y.APLICA.ER = 'N'
    STR.LCK.EVT.ER := Y.APLICA.ER       :'*'
    END
    
    STR.LCK.EVT.ER := Y.NO.CHEQUE       :'*'
    STR.LCK.EVT.ER := Y.AMT.LIOF        :'/'

    ARR.LCK.EVT.ER := STR.LCK.EVT.ER
    STR.LCK.EVT.ER = ''
    NEXT J
;*Generando el archivo que se consultara en la Rtn Utility
;*--------------------------------------------------------
    TEXTO.ARCHIVO = ARR.LCK.EVT.ER
    GOSUB ESCRIBIR.ARCHIVO

    RETURN

PROCESS:
    TXN = R.NEW(TFS.TRANSACTION)
    Y.PARAM.ID = APPLICATION:PGM.VERSION
    CALL SLV.GET.ER.TXN.PARAM(Y.PARAM.ID, OUT.MV.TXN)

    Y.TXN.ER = DCOUNT(OUT.MV.TXN,VM)
    Y.TXN.LIST = DCOUNT(R.NEW(TFS.TRANSACTION), VM)

    CALL GET.LOC.REF ('TELLER.FINANCIAL.SERVICES','LF.MNT.TXN.ER',PosMntTxnEr)

    FOR I = 1 TO Y.TXN.LIST
        Y.AMOUNT = '0'
        Y.TXN     = R.NEW(TFS.TRANSACTION)<1,I>

        FIND Y.TXN IN OUT.MV.TXN SETTING Ap, Vp THEN
        Y.AMOUNT  = R.NEW(TFS.AMOUNT)<1,I>

        R.NEW(TFS.LOCAL.REF)<1, PosMntTxnEr, I> = DROUND(Y.AMOUNT,2)
    END
    ELSE
    R.NEW(TFS.LOCAL.REF)<1, PosMntTxnEr, I> = DROUND(Y.AMOUNT,2)
    END

    NEXT I

    RETURN

ESCRIBIR.ARCHIVO:
    DIR.NAME= 'LibCompensaLocal.Info.TFS.FT.TT'
    R.ID   = Y.ID:'.txt'
    
    DELETESEQ DIR.NAME,R.ID SETTING Seq.Saldo.Temp
    
    OPENSEQ DIR.NAME,R.ID TO SEQ.PTR
    WRITESEQ TEXTO.ARCHIVO APPEND TO SEQ.PTR THEN
    END
    CLOSESEQ SEQ.PTR
    RETURN

    END
