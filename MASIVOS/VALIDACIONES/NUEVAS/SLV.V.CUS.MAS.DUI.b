*-----------------------------------------------------------------------------
* <Rating>-30</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.V.CUS.MAS.DUI
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.CUSTOMER
    $INSERT I_F.SLV.DUI.CUSTOMER.CNT
    $INSERT I_F.EB.SLV.ITEMS.MASIVOS
*-----------------------------------------------------------------------------
GOSUB CONSTRUCTOR
GOSUB ABRIR
GOSUB PROCESAR

CONSTRUCTOR:
FN.EB.SLV.ITEMS.MASIVOS='F.EB.SLV.ITEMS.MASIVOS'
FN.EB.SLV.ITEMS.MASIVOS=''
FN.SLV.DUI.CUSTOMER.CNT = 'F.SLV.DUI.CUSTOMER.CNT'
F.SLV.DUI.CUSTOMER.CNT  = ''
R.SLV.DUI.CUSTOMER.CNT  = ''
SLV.DUI.CUSTOMER.CNT.ER = ''
RETURN

ABRIR:
   CALL OPF(FN.SLV.DUI.CUSTOMER.CNT, F.SLV.DUI.CUSTOMER.CNT)
RETURN

PROCESAR:
        Y.DUI.NUMBER = R.NEW<EB.SLV3.LF.DUI>
        CHANGE '-' TO '' IN Y.DUI.NUMBER
        CALL F.READ(FN.SLV.DUI.CUSTOMER.CNT, Y.DUI.NUMBER, R.SLV.DUI.CUSTOMER.CNT, F.SLV.DUI.CUSTOMER.CNT, SLV.DUI.CUSTOMER.CNT.ER)

        IF R.SLV.DUI.CUSTOMER.CNT THEN           
                ERROR = 'DUI ':Y.DUI.NUMBER: ' Ya existe en el sistema' 
            END
        END
        RETURN
RETURN
		

END
