*-----------------------------------------------------------------------------
* <Rating>-20</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.V.PARAMETROS.CTA.SIMP
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.EB.SLV.GLOBAL.PARAM
*-----------------------------------------------------------------------------
    GOSUB PROCESS
	RETURN

PROCESS:
    FN.TABLE.PA = 'F.EB.SLV.GLOBAL.PARAM'
    F.TABLE.PA = ''
	CALL OPF(FN.TABLE.PA, F.TABLE.PA)
	ERROR='S'
	
    ID.PARAM=ID.NEW

    IF ID.PARAM EQ 'CTA.SIMPLIFICADA.TRX.MES' THEN
	ERROR='N'
    END

    
    IF ID.PARAM EQ 'CTA.SIMPLIFICADA.MONTO.MONTH' THEN
    ERROR='N'
    END    
        
    IF ID.PARAM EQ 'CTA.SIMPLIFICADA.MONTO.TRX' THEN
    ERROR='N'
    END

    
       
    IF ERROR EQ 'S' THEN
        V_NAME_FIELD=EB.SLV39.VALOR.PARAM
        STRERR='EB-SIMP006'
        GOSUB CRT_ERROR
    END


    RETURN



CRT_ERROR:
    AF  = V_NAME_FIELD
    AV 	= 1
    ETEXT = STRERR
    CALL STORE.END.ERROR
    RETURN

    END
