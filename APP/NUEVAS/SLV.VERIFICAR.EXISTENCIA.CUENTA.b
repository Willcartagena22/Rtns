*-----------------------------------------------------------------------------
* <Rating>-40</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.VERIFICAR.EXISTENCIA.CUENTA(ENQ.DATA)
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.CUSTOMER.ACCOUNT
    $INSERT I_F.ACCOUNT
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.SLV.DUI.CUSTOMER.CNT
    $INSERT I_F.CUSTOMER
*-----------------------------------------------------------------------------


GOSUB INICIAR
GOSUB ABRIR
GOSUB CONSULTAR.CUSTOMER
GOSUB CONSULTAR.CUENTA

INICIAR:
    FN.CUS				='F.CUSTOMER'
    F.CUS				=''
    FN.CUS.AC			='F.CUSTOMER.ACCOUNT'
    F.CUS.AC			=''
    FN.ACC 	= 'F.ACCOUNT'
    F.ACC  	= ''
    FN.SLV.DUI.CUSTOMER.CNT = 'F.SLV.DUI.CUSTOMER.CNT'
    F.SLV.DUI.CUSTOMER.CNT  = ''
    
    LOCATE 'DUI' IN D.FIELDS<1> SETTING ITEM.POS THEN
    DUI = D.RANGE.AND.VALUE<ITEM.POS>
    END

    LOCATE 'CATEGORY' IN D.FIELDS<1> SETTING ITEM.POS THEN
    CATEGORY = D.RANGE.AND.VALUE<ITEM.POS>
    END
RETURN

ABRIR:
    CALL OPF(FN.CUS, F.CUS)
    CALL OPF(FN.CUS.AC, F.CUS.AC)
    CALL OPF(FN.ACC, F.ACC)
RETURN

CONSULTAR.CUSTOMER:
    CALL F.READ(FN.SLV.DUI.CUSTOMER.CNT, DUI, R.SLV.DUI.CUSTOMER.CNT, F.SLV.DUI.CUSTOMER.CNT, SLV.DUI.CUSTOMER.CNT.ER)	
    IF R.SLV.DUI.CUSTOMER.CNT THEN        
	CUSTOMER=R.SLV.DUI.CUSTOMER.CNT<SLV.DUI.CUSTOMER.CODE>

    END
RETURN

CONSULTAR.CUENTA:

    CALL F.READ(FN.CUS.AC, CUSTOMER, R.CUS.AC, F.CUS.AC, F.ERR.CUS.AC)
    NUMERO=COUNT(R.CUS.AC,FM)
    NUMERO=NUMERO+1
    FOR j=1 TO NUMERO
        CUENTA.N= FIELD(R.CUS.AC,FM,j)

        CALL F.READ(FN.ACC, CUENTA.N, R.ACCOUNT, F.ACC, Y.ERR.AC)
        CATEGORYN=R.ACCOUNT<AC.CATEGORY>

        IF CATEGORYN EQ CATEGORY THEN
		STR.ACCOUNT=""
		STR.ACCOUNT	:= CUENTA.N 			  		: '*' 	
		STR.ACCOUNT	:= CATEGORYN	  		: '*' 
		ENQ.DATA<-1> = STR.ACCOUNT  	

        END
    NEXT




    RETURN
    END
