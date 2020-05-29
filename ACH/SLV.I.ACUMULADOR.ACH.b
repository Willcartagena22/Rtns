*-----------------------------------------------------------------------------
* <Rating>-14</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.I.ACUMULADOR.ACH
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.EB.SLV.ACUMULADOR.ACH
    $INSERT I_F.EB.SLV.TRANSACTION.ACH
    $INSERT I_F.ACCOUNT
*-----------------------------------------------------------------------------

    GOSUB INIT
    GOSUB OPEN.FILE
    GOSUB GET.VALUE.FT
    GOSUB PROCESS
    RETURN

INIT:
    FN.APP.ACUMULADOR.ACH = 'F.EB.SLV.ACUMULADOR.ACH'
    F.APP.ACUMULADOR.ACH  = ''
    FN.ACCOUNT            = 'F.ACCOUNT'
    F.ACCOUNT             = ''
    RETURN

OPEN.FILE:
    CALL OPF(FN.APP.ACUMULADOR.ACH,F.APP.ACUMULADOR.ACH)
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)
    RETURN

GET.VALUE.FT:
    CUSTOMER      = R.NEW(EB.SLV98.CLIENTE.AZUL)
    ID.FT         = ID.NEW
    DATES         = TODAY[1,4]:TODAY[5,2]
    
    RETURN

PROCESS:
    
    GOSUB GET.DATA.ACUMULADOR
    RETURN

GET.DATA.ACUMULADOR:

    CALL F.READ(FN.APP.ACUMULADOR.ACH,CUSTOMER,CUST.R,F.APP.ACUMULADOR.ACH,ACUM.ERR)

    IF ACUM.ERR NE '' THEN
        CUST.R<EB.SLV96.FECHA.MES,1> = DATES
        CUST.R<EB.SLV96.ID.FT,1>     = ID.FT
        GOSUB WRITE.APP
    END
    ELSE
    LIST.MESES.TRANSACCIONES = CUST.R<EB.SLV96.FECHA.MES>
    COUNT.MONTH = DCOUNT(LIST.MESES.TRANSACCIONES,VM)
   
    CONTADOR = 1
    LOOP
        REMOVE MONTH FROM LIST.MESES.TRANSACCIONES SETTING POS.MONTH
    WHILE MONTH NE ''

        IF MONTH EQ DATES THEN
            LIST.TRX                        = CUST.R<EB.SLV96.ID.FT,CONTADOR>
            NEW.LIST.TRX                    = LIST.TRX:SM:ID.FT
            CUST.R<EB.SLV96.ID.FT,CONTADOR> = NEW.LIST.TRX
            GOSUB WRITE.APP
            BREAK
        END ;*END IF MONTH EQ DATES THEN
        ELSE
            IF CONTADOR EQ COUNT.MONTH THEN
               CUST.R<EB.SLV96.FECHA.MES,CONTADOR+1> = DATES
               CUST.R<EB.SLV96.ID.FT,CONTADOR+1>     = ID.FT  
               GOSUB WRITE.APP 
            END ;* END IF CONTADOR EQ COUNT.MONTH THEN
        END ;*END ELSE MONTH EQ DATES THEN
        
        CONTADOR++
    REPEAT
    END

    RETURN

WRITE.APP:
    TABLE.ID   =  CUSTOMER
    CALL F.WRITE(FN.APP.ACUMULADOR.ACH, TABLE.ID,CUST.R)
    ;*CALL JOURNAL.UPDATE(CUST.R)
    RETURN
    
    END
