*-----------------------------------------------------------------------------
* <Rating>1136</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.V.NPE.COLECTOR
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.EB.SLV.COLECTOR.MAPPING.COL
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.EB.SLV.COLECTOR
    $INSERT I_TSS.COMMON
    $INSERT I_GTS.COMMON
*-----------------------------------------------------------------------------
    IF OFS$OPERATION EQ 'VALIDATE' THEN
        GOSUB INIT
        GOSUB OPEN.FILE
        GOSUB PROCESS
    END
    RETURN

INIT:
    FN.COL.MP = 'FBNK.EB.SLV.COLECTOR.MAPPING.COL'
    F.COL.MP  = ''
    RETURN

OPEN.FILE:
    CALL OPF(FN.COL.MP,F.COL.MP)
    RETURN

PROCESS:
    RECORD.ID = 'GENERIC.NPE.COLECTOR.CN'
    TRAMA     = ''
    CALL F.READ(FN.COL.MP,RECORD.ID,R.APP,F.COL.MP,ERR.APP)
    COUNT.PRM = DCOUNT(R.APP<EB.CL.MP.BDY.FIELD.RSRC>,VM)
    SEPARADOR.TRAMA = R.APP<EB.CL.MP.SEPARADOR.TRAMA>
    FOR X=1 TO COUNT.PRM
        ETIQUETA = R.APP<EB.CL.MP.BDY.FIELD.RSRC,X>

        IF X EQ 1 THEN
            BEGIN CASE
                CASE ETIQUETA EQ 'transactionId'
                    TRAMA := ETIQUETA:'=':ID.NEW
                CASE ETIQUETA EQ 'trxDateTime'
                    HORA    = TIMEDATE()[1,8]
                    FECHA   = OCONV(DATE(),"D/")
                    TIMETRXCONSULTA = FECHA[7,4]:FECHA[1,2]:FECHA[4,2]:' ': HORA
                    TRAMA := ETIQUETA:'=':TIMETRXCONSULTA
                CASE ETIQUETA EQ 'enterpriseId'
                    VALUE = R.APP<EB.CL.MP.BDY.FIELD.NAME,X>
                    TRAMA := ETIQUETA:'=':VALUE
                CASE ETIQUETA EQ 'passphrase'
                    VALUE = R.APP<EB.CL.MP.BDY.FIELD.NAME,X>
                    TRAMA := ETIQUETA:'=':VALUE
                CASE ETIQUETA EQ 'branchId'
                    TRAMA := ETIQUETA:'=':ID.COMPANY
                CASE ETIQUETA EQ 'capture'
                    CALL GET.LOC.REF ('FUNDS.TRANSFER','LF.NUM.NPE',POS.BARCODE)
                    BAR.CODE = R.NEW(FT.LOCAL.REF)<1,POS.BARCODE>
                    TRAMA := ETIQUETA:'=':BAR.CODE
                CASE ETIQUETA EQ 'terminalId'
	                TRAMA := SEPARADOR.TRAMA:ETIQUETA:'=':ID.COMPANY
	            CASE ETIQUETA EQ 'terminalUser'
	                VALUE = R.APP<EB.CL.MP.BDY.FIELD.NAME,X>
	                TRAMA := SEPARADOR.TRAMA:ETIQUETA:'=':VALUE	
	            CASE ETIQUETA EQ 'tipoColector'
	                VALUE = R.APP<EB.CL.MP.BDY.FIELD.NAME,X>
	                TRAMA := SEPARADOR.TRAMA:ETIQUETA:'=':VALUE	
            END CASE
        END
        ELSE
        BEGIN CASE
            CASE ETIQUETA EQ 'transactionId'
                TRAMA := SEPARADOR.TRAMA:ETIQUETA:'=':ID.NEW
            CASE ETIQUETA EQ 'trxDateTime'
                HORA    = TIMEDATE()[1,8]
                FECHA   = OCONV(DATE(),"D/")
                TIMETRXCONSULTA = FECHA[7,4]:FECHA[1,2]:FECHA[4,2]:' ': HORA
                TRAMA := SEPARADOR.TRAMA:ETIQUETA:'=':TIMETRXCONSULTA
            CASE ETIQUETA EQ 'enterpriseId'
                VALUE = R.APP<EB.CL.MP.BDY.FIELD.NAME,X>
                TRAMA := SEPARADOR.TRAMA:ETIQUETA:'=':VALUE
            CASE ETIQUETA EQ 'passphrase'
                VALUE = R.APP<EB.CL.MP.BDY.FIELD.NAME,X>
                TRAMA := SEPARADOR.TRAMA:ETIQUETA:'=':VALUE
            CASE ETIQUETA EQ 'branchId'
                TRAMA := SEPARADOR.TRAMA:ETIQUETA:'=':ID.COMPANY
            CASE ETIQUETA EQ 'capture'
                CALL GET.LOC.REF ('FUNDS.TRANSFER','LF.NUM.NPE',POS.BARCODE)
                BAR.CODE = R.NEW(FT.LOCAL.REF)<1,POS.BARCODE>
                TRAMA := SEPARADOR.TRAMA:ETIQUETA:'=':BAR.CODE
            CASE ETIQUETA EQ 'terminalId'
	                TRAMA := SEPARADOR.TRAMA:ETIQUETA:'=':ID.COMPANY
            CASE ETIQUETA EQ 'terminalUser'
                VALUE = R.APP<EB.CL.MP.BDY.FIELD.NAME,X>
                TRAMA := SEPARADOR.TRAMA:ETIQUETA:'=':VALUE	
            CASE ETIQUETA EQ 'tipoColector'
                VALUE = R.APP<EB.CL.MP.BDY.FIELD.NAME,X>
                TRAMA := SEPARADOR.TRAMA:ETIQUETA:'=':VALUE
        END CASE
    END
    NEXT X

    THIS.PACKAGE.CLASS = "com.bancoazul.t24colecturia.ColectorPEXMWS"
    THIS.METHOD.CLT= "pagoElectronico"
    CALLJ.ARGUMENTS.CLT =  '0||':TRAMA:'@':ID.NEW
    CALLJ.ERROR.SMS = " "
    CALLJ.RESPONSE.CLT = " "
;*Llamada el metodo.
    CALL EB.CALLJ(THIS.PACKAGE.CLASS,THIS.METHOD.CLT,CALLJ.ARGUMENTS.CLT,CALLJ.RESPONSE.CLT,CALLJ.ERROR.CLT)
    OUTPUT.VALUE = CALLJ.RESPONSE.CLT

    TRAMA.PARSE = CHANGE(OUTPUT.VALUE,'?',FM)
    PRM.COUNT = DCOUNT(TRAMA.PARSE,FM)

    FOR ITEMS=1 TO PRM.COUNT
        FIELD_PX = TRAMA.PARSE<ITEMS>
        KEY = FIELD(FIELD_PX,':',1)
        VALUE = FIELD(FIELD_PX,':',2)

        BEGIN CASE
            CASE KEY EQ 'codResult'
                COD.RESULT = VALUE
            CASE KEY EQ 'codDesc'
                COD.DESC = VALUE
            CASE KEY EQ 'collectorId'
                COLLECTOR.ID = VALUE
            CASE KEY EQ 'collectorRef'
                COLLECTOR.REF = VALUE
            CASE KEY EQ 'typeCollect'
                TYPE.COLLECTOR = VALUE
            CASE KEY EQ 'collectorName'
                COLLECTOR.NAME = VALUE
            CASE KEY EQ 'flgAmount'
                FLAG.AMOUNT = VALUE
            CASE KEY EQ 'amount1'
                AMOUNT.FMT = VALUE
                GOSUB FORMATO.MONTO
                MONTO.1 = MONTO.FORMATEADO
            CASE KEY EQ 'amount2'
                AMOUNT.FMT = VALUE
                GOSUB FORMATO.MONTO
                MONTO.2 = MONTO.FORMATEADO
            CASE KEY EQ 'amount3'
                AMOUNT.FMT = VALUE
                GOSUB FORMATO.MONTO
                MONTO.3 = MONTO.FORMATEADO
            CASE KEY EQ 'clientName'
                CLIENT.NAME = VALUE
        END CASE

    NEXT ITEMS

    IF COD.RESULT EQ '00' THEN
        BEGIN CASE
            CASE FLAG.AMOUNT EQ '100'
                MONTO.PAGO = MONTO.1
            CASE FLAG.AMOUNT EQ '110'
                MONTO.PAGO = MONTO.1 + MONTO.2
            CASE FLAG.AMOUNT EQ '111'
                MONTO.PAGO = MONTO.1 + MONTO.2 + MONTO.3
        END CASE

        CALL GET.LOC.REF('FUNDS.TRANSFER','LF.AMT.COL',POS.NC)
        R.NEW(FT.LOCAL.REF)<1,POS.NC>=SM:MONTO.1:SM:MONTO.2:SM:MONTO.3

        CALL GET.LOC.REF ('FUNDS.TRANSFER','LF.RS.PX',POS.RSP)
        R.NEW(FT.LOCAL.REF)<1,POS.RSP> = CHANGE(OUTPUT.VALUE,'?','~')

        CALL GET.LOC.REF ('FUNDS.TRANSFER','LF.COD.CL',POS.COLECTORCODE)
        R.NEW(FT.LOCAL.REF)<1,POS.COLECTORCODE>=COLLECTOR.ID

        CALL GET.LOC.REF ('FUNDS.TRANSFER','LF.REF.COL',POS.COLECTORREF)
        R.NEW(FT.LOCAL.REF)<1,POS.COLECTORREF>=COLLECTOR.REF
        
        CALL GET.LOC.REF ('FUNDS.TRANSFER','LF.COL.NAME',POS.COLNAME)
        R.NEW(FT.LOCAL.REF)<1,POS.COLNAME> = COLLECTOR.NAME
        
        CALL GET.LOC.REF ('FUNDS.TRANSFER','LF.CLI.CL.NAME',POS.CLIENTNAME)
        R.NEW(FT.LOCAL.REF)<1,POS.CLIENTNAME> = CLIENT.NAME
        
        R.NEW(FT.DEBIT.AMOUNT) = MONTO.PAGO
        ;*R.NEW(FT.ORDERING.BANK) = COLLECTOR.NAME
        
        
        
        END;*END IF COD.RESULT EQ '00'
        ELSE
        BEGIN CASE
            CASE COD.RESULT EQ '01'
                LLAVE.BUSQUEDA = 'EB-SLV.V.ERROR.PX.01'
                ETEXT = LLAVE.BUSQUEDA
                CALL STORE.END.ERROR
            CASE COD.RESULT EQ '02'
                LLAVE.BUSQUEDA = 'EB-SLV.V.ERROR.PX.02'
                ETEXT = LLAVE.BUSQUEDA
                CALL STORE.END.ERROR
            CASE COD.RESULT EQ '03'
                LLAVE.BUSQUEDA = 'EB-SLV.V.ERROR.PX.03'
                ETEXT = LLAVE.BUSQUEDA
                CALL STORE.END.ERROR
            CASE COD.RESULT EQ '04'
                LLAVE.BUSQUEDA = 'EB-SLV.V.ERROR.PX.04'
                ETEXT = LLAVE.BUSQUEDA
                CALL STORE.END.ERROR
            CASE COD.RESULT EQ '05'
                LLAVE.BUSQUEDA = 'EB-SLV.V.ERROR.PX.05'
                ETEXT = LLAVE.BUSQUEDA
                CALL STORE.END.ERROR
            CASE COD.RESULT EQ '06'
                LLAVE.BUSQUEDA = 'EB-SLV.V.ERROR.PX.06'
                ETEXT = LLAVE.BUSQUEDA
                CALL STORE.END.ERROR
            CASE COD.RESULT EQ '07'
                LLAVE.BUSQUEDA = 'EB-SLV.V.ERROR.PX.07'
                ETEXT = LLAVE.BUSQUEDA
                CALL STORE.END.ERROR
            CASE COD.RESULT EQ '08'
                LLAVE.BUSQUEDA = 'EB-SLV.V.ERROR.PX.08'
                ETEXT = LLAVE.BUSQUEDA
                CALL STORE.END.ERROR
            CASE COD.RESULT EQ '09'
                LLAVE.BUSQUEDA = 'EB-SLV.V.ERROR.PX.09'
                ETEXT = LLAVE.BUSQUEDA
                CALL STORE.END.ERROR
            CASE COD.RESULT EQ '10'
                LLAVE.BUSQUEDA = 'EB-SLV.V.ERROR.PX.10'
                ETEXT = LLAVE.BUSQUEDA
                CALL STORE.END.ERROR
            CASE COD.RESULT EQ '11'
                LLAVE.BUSQUEDA = 'EB-SLV.V.ERROR.PX.11'
                ETEXT = LLAVE.BUSQUEDA
                CALL STORE.END.ERROR
            CASE COD.RESULT EQ '12'
                LLAVE.BUSQUEDA = 'EB-SLV.V.ERROR.PX.12'
                ETEXT = LLAVE.BUSQUEDA
                CALL STORE.END.ERROR
            CASE COD.RESULT EQ '99'
                LLAVE.BUSQUEDA = 'EB-SLV.V.ERROR.PX.99'
                ETEXT = LLAVE.BUSQUEDA
                CALL STORE.END.ERROR
            CASE COD.RESULT EQ '1'
                LLAVE.BUSQUEDA = 'EB-SLV.V.ERROR.PX.00'
                ETEXT = LLAVE.BUSQUEDA
                CALL STORE.END.ERROR
            CASE 1
                LLAVE.BUSQUEDA = 'EB-SLV.V.ERROR.PX.00'
                ETEXT = LLAVE.BUSQUEDA
                CALL STORE.END.ERROR
        END CASE
    END ;*END ELSE COD.RESULT EQ '00'
        RETURN

FORMATO.MONTO:
        PARTE.ENTERO = SUBSTRINGS(AMOUNT.FMT,1,8)
        PARTE.DECIMAL = SUBSTRINGS(AMOUNT.FMT,9,2)
        MONTO.PREFORMAT = PARTE.ENTERO:'.':PARTE.DECIMAL
        MONTO.FORMATEADO = TRIM(FMT(MONTO.PREFORMAT, "R2#10"))
        RETURN
    END
