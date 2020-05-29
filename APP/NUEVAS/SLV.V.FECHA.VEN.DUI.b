*-----------------------------------------------------------------------------
* <Rating>-20</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.V.FECHA.VEN.DUI(A.INFO)
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.CUSTOMER
$INSERT I_ENQUIRY.COMMON
*-----------------------------------------------------------------------------
GOSUB INIT
GOSUB PROCESAR

INIT:
    FN.CUS				='F.CUSTOMER'
    F.CUS				=''
    CALL OPF(FN.CUS, F.CUS)
    LOCATE 'CUSTOMER' IN D.FIELDS<1> SETTING ITEM.POS THEN
    CUSTOMER = D.RANGE.AND.VALUE<ITEM.POS>
    END
RETURN

PROCESAR:
*CUSTOMER='101010'

    CALL F.READ(FN.CUS, CUSTOMER, R.CUS, F.CUS, F.ERR.CUS)
     FINDSTR 'DOCTO.UNICO.IDENT' IN R.CUS<EB.CUS.LEGAL.DOC.NAME> SETTING AP, VP THEN
            NUM.DUI = R.CUS<EB.CUS.LEGAL.ID><1, VP>
            FECHA.V= R.CUS<EB.CUS.LEGAL.EXP.DATE><1, VP>
            IF FECHA.V LT TODAY THEN
            MENSAJE='DUI VENCIDO'
            END
            ELSE
            MENSAJE='DUI VIGENTE'
            END
            
     END ELSE
            NUM.DUI='DUI NO ENCONTRADO'
            MENSAJE='DUI NO ENCONTRADO'
            FECHA.V='DUI NO ENCONTRADO'
     END
    
 A.INFO<-1>=  NUM.DUI:'*':MENSAJE:'*':FECHA.V
	
CRT A.INFO
RETURN



END
