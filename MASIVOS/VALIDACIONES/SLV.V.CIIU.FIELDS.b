*-----------------------------------------------------------------------------
* <Rating>69</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.V.CIIU.FIELDS
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.CUSTOMER
    $INSERT I_F.EB.SLV.CLS.CIIU
*-----------------------------------------------------------------------------


    GOSUB INIT
    GOSUB OPEN.FILE
    GOSUB PROCESS
    RETURN

INIT:

    FN.CUSTOMER = 'F.CUSTOMER'
    F.CUSTOMER  = ''


    FN.CIIU = 'F.EB.SLV.CLS.CIIU'
    F.CIIU = ''

    RETURN

OPEN.FILE:
    CALL OPF(FN.CUSTOMER,F.CUSTOMER)
    CALL OPF(FN.CIIU,F.CIIU)
    RETURN

PROCESS:

    CALL GET.LOC.REF('CUSTOMER','LF.ACTIVIT.CIIU',POS.ACT.CIIU)
    ACTIVITY.CIIU = R.NEW(EB.CUS.LOCAL.REF)<1,POS.ACT.CIIU>

    CALL F.READ(FN.CIIU,ACTIVITY.CIIU,RESPONSE.CIIU,F.CIIU,ERR.CIIU)

    ACTIVIDAD.PROHIBIDA   = RESPONSE.CIIU<EB.CIIU.AE.PROHIBIDA>
    ACTIVIDAD.ALTO.RIESGO = RESPONSE.CIIU<EB.CIIU.AE.ALTO.RIESGO>

    IF ACTIVIDAD.PROHIBIDA EQ 'SI' AND ACTIVIDAD.ALTO.RIESGO EQ 'NO' THEN
        AF = EB.CUS.LOCAL.REF
        AV = POS.ACT.CIIU
        LLAVE.BUSQUEDA = 'EB-SLV.V.ERROR.PROHIBITED.ACTIVIT'
        ETEXT = LLAVE.BUSQUEDA
        CALL STORE.END.ERROR
    END
    ELSE IF ACTIVIDAD.PROHIBIDA EQ 'NO' AND ACTIVIDAD.ALTO.RIESGO EQ 'SI' THEN
    ;*SET FLAG ACTIVIDAD ECONOMICA (ALTO RIESGO)
    CALL GET.LOC.REF ('CUSTOMER','LF.CUST.FLAG',POS.FLAG)
    R.NEW(EB.CUS.LOCAL.REF)<1,POS.FLAG> = 'ACT-OVERRIDE'
    
    CURR.NO = DCOUNT(R.NEW(EB.CUS.OVERRIDE),VM) + 1
        AF   = EB.CUS.LOCAL.REF
        AV   = POS.ACT.CIIU
        TEXT = "FTC.ACT.ALTO.RIESGO"
        CALL STORE.OVERRIDE (CURR.NO)
    END
    ELSE
        RETURN
    END
    
    RETURN


    END
