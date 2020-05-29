*-----------------------------------------------------------------------------
* <Rating>-34</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE PruebaSaldosCuentas
*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.COMPANY
    $INSERT I_F.EB.SLV.COMISIONES.TARJETAS
    $INSERT I_F.ACCOUNT
    $INSERT I_F.EB.SLV.GLOBAL.PARAM
*-----------------------------------------------------------------------------
    GOSUB INIT
    GOSUB OPENFILE
    GOSUB CALL.J
INIT:


    FN.FT='F.FUNDS.TRANSFER'
    F.FT=''
    FN.SCT='F.EB.SLV.COMISIONES.TARJETAS'
    F.SCT=''
    STR.ERR = "/-1/"
    ID.PARAM.OFS ='OFS.COM.VISA.DEB'
    TRANS.ID = ''
    CURRENCY = 'USD'
    R.FT = ''
    CANT.REQ =''
    ID.FT=''
    CALLJ.ERROR.SMS = " "
    CALLJ.RESPONSE.CLT = " "
    STR.ERR = "/-1/"
    CURRENCY = 'USD'
    DATA.EXTRME=''
    FN_SLVPA 		= 'F.EB.SLV.GLOBAL.PARAM'
    F_SLVPA 		= ''

    FN.ACC = 'F.ACCOUNT'
    F.ACC = ''

    RETURN

OPENFILE:
    CALL OPF(FN.FT,F.FT)
    CALL OPF(FN.SCT,F.SCT)
    CALL OPF(FN.ACC,F.ACC)
    RETURN


CALL.J:

;*ASIGNACION DE PARAMETROS PARA EL CALLJ
    THIS.PACKAGE.CLASS ="com.bancoazul.collector.Collector";
    THIS.METHOD.CLT= "getDataExtreme"
    CALLJ.ARGUMENTS.CLT = "CobroComisiones"
    CALLJ.ERROR.SMS = " "
    CALLJ.RESPONSE.CLT = " "

;*LLAMADA AL METODO CALLJ
    CALL EB.CALLJ(THIS.PACKAGE.CLASS,THIS.METHOD.CLT,CALLJ.ARGUMENTS.CLT,CALLJ.RESPONSE.CLT,CALLJ.ERROR.CLT)
    DATA.EXTRME = CALLJ.RESPONSE.CLT
	
	
;*DATA.EXTRME ="1~10000000151503~USD1406600030001~1.15~AC~REV LIMITE CLASICA~2017-03-09 15:26:51~2017-03-09 15:26:51~A~2017-03-09 15:26:51~C~0|"
    Y.COUNT.FILE = DCOUNT(DATA.EXTRME,'|')-1
	STR.ARR.COMMI = ''
	
	 PARAMETRO='ACC.VISA.DEB.FISC'
	   	CALL F.READ(FN_SLVPA, PARAMETRO, R.TABLE.PA, F_SLVPA, F.ERR.PA)
	   	CUENTAIVA = R.TABLE.PA<EB.SLV39.VALOR.PARAM>
	   	
	   	
    FOR I=1 TO Y.COUNT.FILE
        YBLOQUE.1 = FIELD(DATA.EXTRME,'|',I)
        Y.CAMPOS.B1 = CHANGE(YBLOQUE.1,'~',VM)

        IDOFS = FIELD( Y.CAMPOS.B1,VM,1)
        IDOFS= CHANGE((IDOFS),'"','')
        CUENTADEBITO  = FIELD( Y.CAMPOS.B1,VM,2)
        MONTO = FIELD( Y.CAMPOS.B1,VM,4)

        VAL=LEFT(CUENTADEBITO,2)
	
        IF VAL NE 'US' AND VAL NE 'PL' THEN
       
        CRT 'CRT PARAMETRO : ':CUENTAIVA
			
            ;*Validando saldo
            CALL F.READ(FN.ACC, CUENTADEBITO,RECORD.ACC,F.ACC,ERR.ACC)
            WORKING.BALANCE = RECORD.ACC<AC.WORKING.BALANCE>

*            CRT 'CUENTA : ':CUENTADEBITO:'  SALDO:  ':WORKING.BALANCE
            MONTOCONIVA=MONTO*1.13

            IF WORKING.BALANCE EQ '' THEN
                WORKING.BALANCE = '0.00'
            END
            
            IF WORKING.BALANCE GT MONTOCONIVA  THEN
            END
            ELSE
  
            
            STR.ARR.CUENTAS := CUENTADEBITO    						 :'*';* Fecha de Registro   						|1
          	CRT  'STR.ARR.CUENTAS  :':STR.ARR.CUENTAS 		
            
           
        END
    END



    NEXT I






    END
