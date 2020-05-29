*-----------------------------------------------------------------------------
* <Rating>-34</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.GENERATE.DOCUMENTS.MASIVO
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.EB.SLV.GENERACION.DOC
    $INSERT I_F.EB.SLV.MASTER.MASIVOS
    $INSERT I_F.EB.SLV.ITEMS.MASIVOS
    $INSERT I_F.AA.ARRANGEMENT
*-----------------------------------------------------------------------------

    GOSUB INIT
    GOSUB OPEN.FILE
    GOSUB PROCESS
    RETURN

INIT:
    FN.ITEMS.MASIVOS  = 'F.EB.SLV.ITEMS.MASIVOS'
    F.ITEMS.MASIVOS   = ''
    FN.AA.ARRANGEMENT = 'F.AA.ARRANGEMENT'
    F.AA.ARRANGEMENT  = ''
    FN.MASTER.MASIVOS = 'F.EB.SLV.MASTER.MASIVOS'
    F.MASTER.MASIVOS  = ''
    RETURN

OPEN.FILE:
    CALL OPF(FN.ITEMS.MASIVOS,F.ITEMS.MASIVOS)
    CALL OPF(FN.AA.ARRANGEMENT,F.AA.ARRANGEMENT)
    CALL OPF(FN.MASTER.MASIVOS,F.MASTER.MASIVOS)
    RETURN

PROCESS:
    ID.CARGA.MASIVA = R.NEW(EB.SLV69.MASTER.MASIVO.ID)
    ;*ID.CARGA.MASIVA = 'BKML1552085584443'

    STMT.ITEMS.MASIVOS = "SELECT ":FN.ITEMS.MASIVOS:" WITH @ID LIKE '":ID.CARGA.MASIVA:"...' "
    CALL EB.READLIST (STMT.ITEMS.MASIVOS, KEY.LIST, '', ITEMS.COUNT, SYSTEM.RETURN.CODE)

    LOOP
        REMOVE ITEM.ID FROM KEY.LIST SETTING POS.ID
    WHILE ITEM.ID NE ''
        CALL F.READ(FN.ITEMS.MASIVOS,ITEM.ID,R.ITEM,F.ITEMS.MASIVOS,ITEM.ERR)
        ARRANGEMENT.ID = R.ITEM<EB.SLV3.ACCOUNT>
        CUSTOMER.ID    = R.ITEM<EB.SLV3.ID.CUSTOMER>
        ;*OBTENEMOS EL DETALLE DEL ARRANGEMENT
        CALL F.READ(FN.AA.ARRANGEMENT,ARRANGEMENT.ID,R.ARRANGEMENT,F.AA.ARRANGEMENT,ERR.ARRANGEMENT)
        ACCOUNT.NUMBER = R.ARRANGEMENT<AA.ARR.LINKED.APPL.ID>
        ;*Generación documento CONTRATO CUENTA DE AHORRO ELECTRÓNICA PERSONA NATURAL
        CALL SLV.E.NOF.ACC.CONTRATO.CRG.MSV(ACCOUNT.NUMBER,'DOCUMENTO-CONTRATO-APERTURA',ITEM.ID,ID.CARGA.MASIVA,CUSTOMER.ID)
        ;*Generación de documentos declración jurada y perfil del cliente.
        CALL SLV.E.NOF.ACC.DJ.CRG.MSV(CUSTOMER.ID,ACCOUNT.NUMBER,ITEM.ID,ID.CARGA.MASIVA)
    REPEAT

    RETURN

    END
