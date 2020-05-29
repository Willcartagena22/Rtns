*-----------------------------------------------------------------------------
* <Rating>-35</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE REV.COMISION.VISA.DEB
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
    GOSUB WRITE
    

INIT:
    FN.FT='F.FUNDS.TRANSFER'
    F.FT=''
    FN.SCT='F.EB.SLV.COMISIONES.TARJETAS'
    F.SCT=''
    FN_SLVPA 		= 'F.EB.SLV.GLOBAL.PARAM'
    F_SLVPA 		= ''
    FN.ACC = 'F.ACCOUNT'
    F.ACC = ''



PROCESS:
    STMT.FT = "SELECT ":FN.FT:" LF.IDOFS.CARD EQ '":IDOFS:"'"


    CALL EB.READLIST(STMT.FT,FT.LIST,'',NO.OF.FT,FT.ERR)

    FT2=FIELD(FT.LIST,FM,2)
*    FT1<>FT THEN
;*Traer Monto IVA
    CALL F.READ(FN.FT,FT1, R.FT,F.FT,E.FT)
    MONTO= R.FT<FT.DEBIT.AMOUNT>
;*NUEVA FT
    CALL F.READ(FN.FT,'', R.FT,F.FT,E.FT)
    R.FT<FT.DEBIT.ACCT.NO>    =  CUENTADEBITO
    R.FT<FT.CREDIT.ACCT.NO>   =  CUENTACREDITO
    R.FT<FT.DEBIT.CURRENCY>   =  'USD'
    R.FT<FT.DEBIT.AMOUNT>     =  MONTO
    R.FT<FT.ORDERING.BANK>    =  ORDENBANCO
    R.FT<FT.TRANSACTION.TYPE> =  TIPOTRANSACTION
    CALL SLV.OFS.UTIL.OL.TRX(TRANS.ID,R.FT,ID.PARAM.OFS,Y.OUT)

    FINDSTR STR.ERR IN FIELD(Y.OUT, FM, 2) SETTING Ap, Vp THEN
        ;*MANDAR UN ERROR
    END
    ELSE

    GOSUB WRITE
*
*    END


WRITE:
    STMT.SCT = "SELECT ":FN.SCT:" STATUS EQ '0' AND CARD.TYPE EQ 'D'";*AGREGAR FILTRO CON LA FT DE COMISION
    CALL EB.READLIST(STMT.FT,FT.LIST,'',NO.OF.FT,FT.ERR)
    FT1=FIELD(FT.LIST,FM,1)

*** <desc>Persistir en la App Local </desc>

    R.SCT <EB.SLV16.STATUS> = '0'

    CALL F.WRITE (FN.SCT,IDOFS, R.SCT)
    CALL JOURNAL.UPDATE(FN.SCT)

    RETURN

    RETURN


    END



    END
