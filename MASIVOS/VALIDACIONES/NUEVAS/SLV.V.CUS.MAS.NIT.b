*-----------------------------------------------------------------------------
* <Rating>-30</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.V.CUS.MAS.NIT
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.CUSTOMER
    $INSERT I_F.SLV.NIT.CUSTOMER.CNT
    $INSERT I_F.EB.SLV.ITEMS.MASIVOS
*-----------------------------------------------------------------------------
GOSUB CONSTRUCTOR
GOSUB ABRIR
GOSUB PROCESAR


CONSTRUCTOR:
FN.EB.SLV.ITEMS.MASIVOS='F.EB.SLV.ITEMS.MASIVOS'
FN.EB.SLV.ITEMS.MASIVOS=''
FN.SLV.NIT.CUSTOMER.CNT = 'F.SLV.NIT.CUSTOMER.CNT'
F.SLV.NIT.CUSTOMER.CNT  = ''
R.SLV.NIT.CUSTOMER.CNT  = ''
SLV.NIT.CUSTOMER.CNT.ER = ''
RETURN

ABRIR:
   CALL OPF(FN.SLV.NIT.CUSTOMER.CNT, F.SLV.NIT.CUSTOMER.CNT)
RETURN

PROCESAR:
        Y.NIT.NUMBER = R.NEW<EB.SLV3.LF.NIT>
        ;*Y.NIT.NUMBER = '00000097030004'
        CHANGE '-' TO '' IN Y.NIT.NUMBER
        CALL F.READ(FN.SLV.NIT.CUSTOMER.CNT, Y.NIT.NUMBER , R.SLV.NIT.CUSTOMER.CNT, F.SLV.NIT.CUSTOMER.CNT, SLV.NIT.CUSTOMER.CNT.ER)

        IF R.SLV.NIT.CUSTOMER.CNT THEN           
                ERROR = 'NIT ':Y.NIT.NUMBER: ' Ya existe en el sistema' 
            END
        END
        RETURN
RETURN
		

END
