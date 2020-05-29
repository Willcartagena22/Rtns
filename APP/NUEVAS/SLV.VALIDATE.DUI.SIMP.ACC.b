*-----------------------------------------------------------------------------
* <Rating>-61</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.VALIDATE.DUI.SIMP.ACC(A.INFO)
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.EB.SLV.ITEMS.MASIVOS
    $INSERT I_F.SLV.DUI.CUSTOMER.CNT
    $INSERT I_F.EB.SLV.VALIDACIONES.MASIVOS
    $INSERT I_F.CUSTOMER

    
*-----------------------------------------------------------------------------
    GOSUB INICIAR
    GOSUB ABRIR
    GOSUB PROCESAR

INICIAR:

    FN.SLV.DUI.CUSTOMER.CNT = 'F.SLV.DUI.CUSTOMER.CNT'
    F.SLV.DUI.CUSTOMER.CNT  = ''
    FN.CUS				='F.CUSTOMER'
    F.CUS				=''
    	LOCATE "DUI" IN D.FIELDS<1> SETTING POS.1 THEN
	   DUI = D.RANGE.AND.VALUE<POS.1>
	END
    RETURN

ABRIR:
    CALL OPF(FN.SLV.DUI.CUSTOMER.CNT, F.SLV.DUI.CUSTOMER.CNT)
    CALL OPF(FN.CUS,F.CUS)

    RETURN



PROCESAR:

    V_CONTENT=DUI
    RESPUESTA='DUI CORRECTO'
    GOSUB VALIDAR.EXIST.DUI
    GOSUB VALIDA.DUI.SECUENCIAL
    GOSUB DIG.VAL.DUI
    CRT RESPUESTA

    A.INFO<-1>= 'RESPUESTA : ':'*':RESPUESTA

    RETURN


VALIDA.DUI.SECUENCIAL:
;* Número de dui secuencial no valido
    LEN_NODOC = LEN(TRIM(V_CONTENT))
    FOR N.I=1 TO LEN_NODOC
        N_VALUE+=V_CONTENT[N.I,1]
    NEXT N.I
    N_INDEX=N_VALUE/LEN_NODOC

    IF  V_CONTENT NE '' AND V_CONTENT[1,1] EQ N_INDEX THEN

    RESPUESTA ='DUI INCORRECTO'

    END
    RETURN

VALIDAR.EXIST.DUI:

    CALL F.READ(FN.SLV.DUI.CUSTOMER.CNT, V_CONTENT, R.SLV.DUI.CUSTOMER.CNT, F.SLV.DUI.CUSTOMER.CNT, SLV.DUI.CUSTOMER.CNT.ER)
	CUSTOMER=R.SLV.DUI.CUSTOMER.CNT<SLV.DUI.CUSTOMER.CODE>
    IF R.SLV.DUI.CUSTOMER.CNT THEN
        
        CALL F.READ(FN.CUS, CUSTOMER, R.CUS, F.CUS, CUS.ERR)
        EMAIL=R.CUS<EB.CUS.EMAIL.1>
        RESPUESTA ='DUI EXISTENTE,':CUSTOMER:'-':EMAIL

    END
    RETURN

DIG.VAL.DUI:

    CALL SLV.S.VALIDATE.DUI(V_CONTENT,Y.RESULT,Y.ERROR.CODE)

    IF Y.ERROR.CODE THEN
        RESPUESTA ='DUI INCORRECTO'

    END

    RETURN
    
    

    END
