*-----------------------------------------------------------------------------
* <Rating>-32</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE PruebaOFSNoEnviados
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.COMPANY
    $INSERT I_F.EB.SLV.COMISIONES.TARJETAS
*-----------------------------------------------------------------------------
    GOSUB INIT
    GOSUB OPENFILE
    GOSUB PREPROCESS




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





PREPROCESS:

*** <desc>Barrido de FT pendientes </desc>
    STMT.SCT = "SELECT ":FN.SCT:" STATUS EQ '0' AND CARD.TYPE EQ 'D'"
;*Aplicando el Statement
    CALL EB.READLIST(STMT.SCT, SCT.LIST,'',NO.OF.SCT,SCT.ERR)
    CRT NO.OF.SCT
    
    RETURN

    END
