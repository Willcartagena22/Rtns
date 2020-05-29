*-----------------------------------------------------------------------------
* <Rating>-37</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE BORRAR.IDS.FT
*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.COMPANY
   
*-----------------------------------------------------------------------------
    GOSUB INIT
    GOSUB OPENFILE
    GOSUB PROCESS

    RETURN

INIT:

    FN.FT='F.FUNDS.TRANSFER'
    F.FT=''
    FN.SCT='F.EB.SLV.COMISIONES.TARJETAS'
    F.SCT=''
    STR.ERR = "/-1/"
    ID.PARAM.OFS ='OFS.COM.VISA.DEB'
    TRANS.ID = ''
    CURRENCY = 'USD'
    R.FT = ''
    CANT.REQ =''
    ID.FT=''
    CALLJ.ERROR.SMS = " "
    CALLJ.RESPONSE.CLT = " "
    STR.ERR = "/-1/"
    CURRENCY = 'USD'
    DATA.EXTRME=''


    RETURN

OPENFILE:
    CALL OPF(FN.FT,F.FT)
    CALL OPF(FN.SCT,F.SCT)
    RETURN




PROCESS:

*** <desc>Barrido de FT pendientes </desc>
    STMT.SCT ="SELECT ":FN.FT:" LF.IDOFS.CARD GT 0"
;*Aplicando el Statement
    CALL EB.READLIST(STMT.SCT, SCT.LIST,'',NO.OF.SCT,SCT.ERR)
    CRT SCT.LIST
   
    COUNTER=COUNT(SCT.LIST,FM)+1
    CRT 'COUNTER : ':COUNTER
    FOR I=1 TO NO.OF.SCT
        CALL F.READ(FN.FT,SCT.LIST<I>,AR.SCT,F.FT,ERR.SCT)
		CRT 'SCT.LIST<I>': SCT.LIST<I>
*        R.FT = ''
*        CALL GET.LOC.REF('FUNDS.TRANSFER','LF.IDOFS.CARD', LocPosIdOfsCard)
*
*        AR.SCT<FT.LOCAL.REF,LocPosIdOfsCard> = ''
*        AVC=AR.SCT<FT.LOCAL.REF,LocPosIdOfsCard>
*        CALL F.WRITE (FN.FT,SCT.LIST<I>,AR.SCT)
*        CALL JOURNAL.UPDATE(FN.FT)

    NEXT I




    RETURN




    END
