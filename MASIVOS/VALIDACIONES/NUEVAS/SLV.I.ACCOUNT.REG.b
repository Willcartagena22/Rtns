*-----------------------------------------------------------------------------
* <Rating>-31</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.I.ACCOUNT.REG
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_F.ACCOUNT
    $INSERT I_EQUATE
    $INSERT I_F.CUSTOMER
    $INSERT I_F.EB.SLV.ITEMS.MASIVOS
    $INSERT I_AA.ACTION.CONTEXT
    $INSERT I_F.AA.ARRANGEMENT.ACTIVITY
    $INSERT I_F.AA.SETTLEMENT
    $INSERT I_AA.LOCAL.COMMON
    $INSERT I_AA.APP.COMMON
*-----------------------------------------------------------------------------
    GOSUB CONSTRUCTOR
    GOSUB ABRIR
    GOSUB ACTUALIZAR

CONSTRUCTOR:
    FN.EB.SLV.ITEMS.MASIVOS='F.EB.SLV.ITEMS.MASIVOS'
    F.EB.SLV.ITEMS.MASIVOS=''
    FN.EB.SLV.ITEMS.MASIVOS2='F.EB.SLV.ITEMS.MASIVOS'
    F.EB.SLV.ITEMS.MASIVOS2=''
    FN_CUS 			= 'F.CUSTOMER'
    F_CUS 			= ''
    FN.ACCOUNT 			= 'F.ACCOUNT'
    F.ACCOUNT 			= ''
    RETURN

ABRIR:
    CALL OPF(FN.EB.SLV.ITEMS.MASIVOS, F.EB.SLV.ITEMS.MASIVOS)
    CALL OPF(FN.EB.SLV.ITEMS.MASIVOS2, F.EB.SLV.ITEMS.MASIVOS2)

    CALL OPF(FN.ACCOUNT, F.ACCOUNT)
    CALL OPF(FN_CUS, F_CUS)
    CALL GET.LOC.REF('CUSTOMER', 'LF.CUS.MASIVOS', POSCusM)

    RETURN


ACTUALIZAR:
ID.ACC= c_aalocArrId


ID.CUSTOMER=R.NEW(AA.ARR.ACT.CUSTOMER)


*    CALL F.READ(FN.ACCOUNT,Y.ACCT,RS.ACC,F.ACCOUNT,ACC.ERR)
*    ID.CUSTOMER=RS.ACC<AC.CUSTOMER>
*    TEXTO.ARCHIVO=ID.CUSTOMER
*	GOSUB ESCRIBIR.ARCHIVO
    

    CALL F.READ(FN_CUS,ID.CUSTOMER,RS.CUS,F_CUS,CUS.ERR)
    ID.MAS		=   RS.CUS<EB.CUS.LOCAL.REF><1,POSCusM>
    

    IF ID.MAS THEN
        CALL F.READ(FN.EB.SLV.ITEMS.MASIVOS,ID.MAS,RS.MAS,F.EB.SLV.ITEMS.MASIVOS,ITEM.ERR)
        RS.MAS<EB.SLV3.ACCOUNT>=ID.ACC
        CALL F.WRITE (FN.EB.SLV.ITEMS.MASIVOS,ID.MAS, RS.MAS)

    END




    RETURN
