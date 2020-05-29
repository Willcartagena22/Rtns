*-----------------------------------------------------------------------------
* <Rating>-44</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.V.CTA.SIM.TFS
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.TELLER.FINANCIAL.SERVICES
    $INSERT I_F.ACCOUNT
    $INSERT I_F.AA.ACCOUNT
    $INSERT I_F.AA.ARRANGEMENT
*-----------------------------------------------------------------------------

    GOSUB INIT
    GOSUB OPEN.FILE
    GOSUB CAT.SIMP
    GOSUB PROCESS
    RETURN

INIT:
    ACC.FN = 'F.ACCOUNT'
    ACC.F  = ''
    RETURN

OPEN.FILE:
    CALL OPF(ACC.FN,ACC.F)
    RETURN

PROCESS:
    ;*OBTENEMOS LA CUENTA A TRANSACCIONAR
    ACC.TFS      = R.NEW(TFS.PRIMARY.ACCOUNT)
    ;*OBTENEMOS EL DETALLE DE LA CUENTA
    CALL F.READ(ACC.FN,ACC.TFS,ACC.R,ACC.F,ACC.ERR)
    ;*OBTENEMOS EL CATEGORY DE LA CUENTA
    ACC.CATEGORY = ACC.R<AC.CATEGORY>
    
    IF ACC.CATEGORY EQ CATEGORY.SIMP THEN
          AF = TFS.PRIMARY.ACCOUNT
          AV = TFS.PRIMARY.ACCOUNT
          LLAVE.BUSQUEDA = 'EB-TRX.FAILED.CTA.SIMPLIFICADA.TFS'
          ETEXT = LLAVE.BUSQUEDA
          CALL STORE.END.ERROR
    END
    
    RETURN

CAT.SIMP:
    FN.AA.PRODUCT.DESIGNER = 'F.AA.PRD.DES.ACCOUNT'
    F.AA.PRODUCT.DESIGNER = ''
    CALL OPF(FN.AA.PRODUCT.DESIGNER,F.AA.PRODUCT.DESIGNER)
    Y.PRODUCTO='CUENTA.AHORRO.SIM'
    SELECT.PROD.DES = "SELECT " : FN.AA.PRODUCT.DESIGNER : " WITH @ID LIKE '" : Y.PRODUCTO : "...'"
    CALL EB.READLIST(SELECT.PROD.DES, PROD.DES,'',NO.REC.PROD.DES, ERR.REC.PROD.DES)
    IF NO.REC.PROD.DES NE 0 THEN
        CALL F.READ(FN.AA.PRODUCT.DESIGNER,PROD.DES, PROD.DES.REC, F.AA.PRODUCT.DESIGNER, ERR.REC1)
        IF PROD.DES.REC THEN


            CALL F.READ(FN.AA.PRODUCT.DESIGNER,PROD.DES, PROD.DES.REC, F.AA.PRODUCT.DESIGNER, ERR.REC1)
            IF PROD.DES.REC THEN
                CATEGORY.SIMP = PROD.DES.REC<AA.AC.CATEGORY> ;*Obtener el parent para luego ir a traer sus propiedades
            END

        END

    END
    RETURN



    END
