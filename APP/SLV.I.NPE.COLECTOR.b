*-----------------------------------------------------------------------------
* <Rating>955</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.I.NPE.COLECTOR
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.EB.SLV.COLECTOR.MAPPING.COL
    $INSERT I_F.EB.SLV.COLECTOR.TRX.PAGO
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_TSS.COMMON
    $INSERT I_GTS.COMMON
*-----------------------------------------------------------------------------


    IF OFS$OPERATION EQ 'PROCESS' THEN
        GOSUB INIT
        GOSUB OPEN.FILE
        GOSUB PROCESS
    END
    RETURN

INIT:
    FN.COL.MP = 'F.EB.SLV.COLECTOR.MAPPING.COL'
    F.COL.MP  = ''
    FN.COL.PG = 'F.EB.SLV.COLECTOR.TRX.PAGO'
    F.COL.PG  = ''
    RETURN

OPEN.FILE:
    CALL OPF(FN.COL.MP,F.COL.MP)
    CALL OPF(FN.COL.PG,F.COL.PG)
    RETURN

PROCESS:
    CALL GET.LOC.REF ('FUNDS.TRANSFER','LF.RS.PX',POS.RSP)
    TRAMA.CONSULTA = R.NEW(FT.LOCAL.REF)<1,POS.RSP>
    TRAMA.CONSULTA = CHANGE(TRAMA.CONSULTA,'~',FM)
*    CONVERT SM TO FM IN TRAMA.CONSULTA
    ;*TRAMA.VS.2 = SM:TRAMA.VS
    
    ;*TRAMA.PARSE = CHANGE(TRAMA.CONSULTA,'?',FM)
    PRM.COUNT     = DCOUNT(TRAMA.CONSULTA,FM)
    PRM.COUNT.VM  = DCOUNT(TRAMA.CONSULTA,VM)
    PRM.COUNT.SM  = DCOUNT(TRAMA.CONSULTA,SM)
    TEXTO.ARCHIVO = 'TRAMA.CONSULTA > ':TRAMA.CONSULTA
    GOSUB ESCRIBIR.ARCHIVO
    
    TEXTO.ARCHIVO = 'PRM.COUNT > ':PRM.COUNT:' | VM > ':PRM.COUNT.VM:' | SM > ':PRM.COUNT.SM
    GOSUB ESCRIBIR.ARCHIVO

    FOR ITM=1 TO PRM.COUNT
        FIELD_PX = TRAMA.CONSULTA<ITM>
        KEY = FIELD(FIELD_PX,':',1)
        VALUE = FIELD(FIELD_PX,':',2)

        BEGIN CASE
            CASE KEY EQ 'amount1'
                AMOUNT.1 = VALUE
            CASE KEY EQ 'amount2'
                AMOUNT.2 = VALUE
            CASE KEY EQ 'amount3'
                AMOUNT.3 = VALUE
            CASE KEY EQ 'transactionId'
                TRX.ID = VALUE
            CASE KEY EQ 'typeCollect'
                TYPE.COLLECT = VALUE
            CASE KEY EQ 'flgAmount'
                FLAG.AMOUNT = VALUE
            CASE KEY EQ 'collectorId'
                COLLECTOR.ID = VALUE
            CASE KEY EQ 'collectorRef'
                COLLECTOR.REF = VALUE
            CASE KEY EQ 'collectorName'
                COLLECTOR.NAME = VALUE
        END CASE

    NEXT ITM


    RECORD.ID = 'GENERIC.NPE.COLECTOR.PG'
    TRAMA     = ''
    CALL F.READ(FN.COL.MP,RECORD.ID,R.APP,F.COL.MP,ERR.APP)
    COUNT.PRM = DCOUNT(R.APP<EB.CL.MP.BDY.FIELD.RSRC>,VM)
    SEPARADOR.TRAMA = R.APP<EB.CL.MP.SEPARADOR.TRAMA>

    FOR LST.PRM=1 TO COUNT.PRM
        ETIQUETA = R.APP<EB.CL.MP.BDY.FIELD.RSRC,LST.PRM>
        IF LST.PRM EQ 1 THEN

            BEGIN CASE
                CASE ETIQUETA EQ 'transactionId'
                    TRAMA := ETIQUETA:'=':TRX.ID
*                CASE ETIQUETA EQ 'typeCollect'
*                    TRAMA := ETIQUETA:'=':TYPE.COLLECT
*                CASE ETIQUETA EQ 'flgAmount'
*                    TRAMA := ETIQUETA:'=':FLAG.AMOUNT
                CASE ETIQUETA EQ 'trxDateTime'
                    HORA    = TIMEDATE()[1,8]
                    FECHA   = OCONV(DATE(),"D/")
                    TIMETRXPAGO = FECHA[7,4]:FECHA[1,2]:FECHA[4,2]:' ': HORA
                    TRAMA := ETIQUETA:'=':TIMETRXPAGO
                CASE ETIQUETA EQ 'amount1'
                    TRAMA := ETIQUETA:'=':AMOUNT.1
                    ;*TRAMA := ETIQUETA:'=0000011111'
                CASE ETIQUETA EQ 'amount2'
                    TRAMA := ETIQUETA:'=':AMOUNT.2
                CASE ETIQUETA EQ 'amount3'
                    TRAMA := ETIQUETA:'=':AMOUNT.3
                CASE ETIQUETA EQ 'collectorId'
                    TRAMA := ETIQUETA:'=':COLLECTOR.ID
*                CASE ETIQUETA EQ 'collectorRef'
*                    TRAMA := ETIQUETA:'=':COLLECTOR.REF
*                CASE ETIQUETA EQ 'enterpriseId'
*                    VALUE = R.APP<EB.CL.MP.BDY.FIELD.NAME,LST.PRM>
*                    TRAMA := ETIQUETA:'=':VALUE
*                CASE ETIQUETA EQ 'passphrase'
*                    VALUE = R.APP<EB.CL.MP.BDY.FIELD.NAME,LST.PRM>
*                    TRAMA := ETIQUETA:'=':VALUE
                CASE ETIQUETA EQ 'branchId'
                    TRAMA := ETIQUETA:'=':ID.COMPANY
                CASE ETIQUETA EQ 'capture'
                    CALL GET.LOC.REF ('FUNDS.TRANSFER','LF.NUM.NPE',POS.READBAND)
                    READ.BAND.PX = R.NEW(FT.LOCAL.REF)<1,POS.READBAND>
                    TRAMA := ETIQUETA:'=':READ.BAND.PX
            END CASE

            END;*END IF
            ELSE

            BEGIN CASE
                CASE ETIQUETA EQ 'transactionId'
                    TRAMA := SEPARADOR.TRAMA:ETIQUETA:'=':TRX.ID
*                CASE ETIQUETA EQ 'typeCollect'
*                    TRAMA := SEPARADOR.TRAMA:ETIQUETA:'=':TYPE.COLLECT
*                CASE ETIQUETA EQ 'flgAmount'
*                    TRAMA := SEPARADOR.TRAMA:ETIQUETA:'=':FLAG.AMOUNT
                CASE ETIQUETA EQ 'trxDateTime'
                    HORA    = TIMEDATE()[1,8]
                    FECHA   = OCONV(DATE(),"D/")
                    TIMETRXPAGO = FECHA[7,4]:FECHA[1,2]:FECHA[4,2]:' ': HORA
                    TRAMA := SEPARADOR.TRAMA:ETIQUETA:'=':TIMETRXPAGO
                CASE ETIQUETA EQ 'amount1'
                    TRAMA := SEPARADOR.TRAMA:ETIQUETA:'=':AMOUNT.1
                    ;*TRAMA := SEPARADOR.TRAMA:ETIQUETA:'=0000011111'
                CASE ETIQUETA EQ 'amount2'
                    TRAMA := SEPARADOR.TRAMA:ETIQUETA:'=':AMOUNT.2
                CASE ETIQUETA EQ 'amount3'
                    TRAMA := SEPARADOR.TRAMA:ETIQUETA:'=':AMOUNT.3
                CASE ETIQUETA EQ 'collectorId'
                    TRAMA := SEPARADOR.TRAMA:ETIQUETA:'=':COLLECTOR.ID
*                CASE ETIQUETA EQ 'collectorRef'
*                    TRAMA := SEPARADOR.TRAMA:ETIQUETA:'=':COLLECTOR.REF
*                CASE ETIQUETA EQ 'enterpriseId'
*                    VALUE = R.APP<EB.CL.MP.BDY.FIELD.NAME,LST.PRM>
*                    TRAMA := SEPARADOR.TRAMA:ETIQUETA:'=':VALUE
*                CASE ETIQUETA EQ 'passphrase'
*                    VALUE = R.APP<EB.CL.MP.BDY.FIELD.NAME,LST.PRM>
*                    TRAMA := SEPARADOR.TRAMA:ETIQUETA:'=':VALUE
                CASE ETIQUETA EQ 'branchId'
                    TRAMA := SEPARADOR.TRAMA:ETIQUETA:'=':ID.COMPANY
                CASE ETIQUETA EQ 'capture'
                    CALL GET.LOC.REF ('FUNDS.TRANSFER','LF.NUM.NPE',POS.READBAND)
                    READ.BAND.PX = R.NEW(FT.LOCAL.REF)<1,POS.READBAND>
                    TRAMA := SEPARADOR.TRAMA:ETIQUETA:'=':READ.BAND.PX
            END CASE
				
            END;*END ELSE

        NEXT LST.PRM ;*END FOR

        STRING.PEX = '1||':TRAMA:'&&tipoColector=NPE@':ID.NEW
        TEXTO.ARCHIVO = 'STRING.PEX > ':STRING.PEX
        GOSUB ESCRIBIR.ARCHIVO
        THIS.PACKAGE.CLASS = "com.bancoazul.t24colecturia.ColectorPEXMWS"
        THIS.METHOD.CLT = "pagoElectronico"
        CALLJ.ARGUMENTS.CLT =  STRING.PEX
        CALLJ.ERROR.SMS = " "
        CALLJ.RESPONSE.CLT = " " 
        ;*Llamada el metodo.
        CALL EB.CALLJ(THIS.PACKAGE.CLASS,THIS.METHOD.CLT,CALLJ.ARGUMENTS.CLT,CALLJ.RESPONSE.CLT,CALLJ.ERROR.CLT)
        PX.RESPONSE = CALLJ.RESPONSE.CLT
        TEXTO.ARCHIVO = 'PX.RESPONSE > ':PX.RESPONSE
        GOSUB ESCRIBIR.ARCHIVO
        ;*SIMULANDO UN PAGO EXITOSO
        ;*SET.PX.RESPONSE = "codDesc:Transaccion Exitosa.?codResult:00?receiptNumber:7915706?transactionId:355898010?trxDateTime:20180412 11:37:55"
        RESPONSE.PARSE = CHANGE(PX.RESPONSE,'?',FM)
        PRM.RS.COUNT = DCOUNT(RESPONSE.PARSE,FM)

        FOR RS.X=1 TO PRM.RS.COUNT
            FIELDS_RS = RESPONSE.PARSE<RS.X>
            RS_KY = FIELD(FIELDS_RS,':',1)
            RS_VL = FIELD(FIELDS_RS,':',2)

            BEGIN CASE
                CASE RS_KY EQ 'codResult'
                    COD.RS = RS_VL
                CASE RS_KY EQ 'codDesc'
                    DESC.RS = RS_VL
                CASE RS_KY EQ 'receiptNumber'
                    ID.PEX = RS_VL
            END CASE

        NEXT RS.X

        IF COD.RS EQ '00' THEN
            GOSUB APPLICATION.WRITE
        END ;*END IF COD.RS EQ '00'
        ELSE
        BEGIN CASE
            CASE COD.RS EQ '01'
                LLAVE.BUSQUEDA = 'EB-SLV.V.ERROR.PX.01'
                ETEXT = LLAVE.BUSQUEDA
                CALL STORE.END.ERROR
            CASE COD.RS EQ '02'
                LLAVE.BUSQUEDA = 'EB-SLV.V.ERROR.PX.02'
                E = LLAVE.BUSQUEDA
                CALL ERR
            CASE COD.RS EQ '03'
                LLAVE.BUSQUEDA = 'EB-SLV.V.ERROR.PX.03'
                ETEXT = LLAVE.BUSQUEDA
                CALL STORE.END.ERROR
            CASE COD.RS EQ '04'
                LLAVE.BUSQUEDA = 'EB-SLV.V.ERROR.PX.04'
                E = LLAVE.BUSQUEDA
                CALL ERR
            CASE COD.RS EQ '05'
                LLAVE.BUSQUEDA = 'EB-SLV.V.ERROR.PX.05'
                E = LLAVE.BUSQUEDA
                CALL ERR
            CASE COD.RS EQ '06'
                LLAVE.BUSQUEDA = 'EB-SLV.V.ERROR.PX.06'
                E = LLAVE.BUSQUEDA
                CALL ERR
            CASE COD.RS EQ '07'
                LLAVE.BUSQUEDA = 'EB-SLV.V.ERROR.PX.07'
                E = LLAVE.BUSQUEDA
                CALL ERR
            CASE COD.RS EQ '08'
                LLAVE.BUSQUEDA = 'EB-SLV.V.ERROR.PX.08'
                E = LLAVE.BUSQUEDA
                CALL ERR
            CASE COD.RS EQ '09'
                LLAVE.BUSQUEDA = 'EB-SLV.V.ERROR.PX.09'
                E = LLAVE.BUSQUEDA
                CALL ERR
            CASE COD.RS EQ '10'
                LLAVE.BUSQUEDA = 'EB-SLV.V.ERROR.PX.10'
                E = LLAVE.BUSQUEDA
                CALL ERR
            CASE COD.RS EQ '11'
                LLAVE.BUSQUEDA = 'EB-SLV.V.ERROR.PX.11'
                E = LLAVE.BUSQUEDA
                CALL ERR
            CASE COD.RS EQ '12'
                LLAVE.BUSQUEDA = 'EB-SLV.V.ERROR.PX.12'
                E = LLAVE.BUSQUEDA
                CALL ERR
            CASE COD.RS EQ '99'
                ;*LLAVE.BUSQUEDA = 'EB-SLV.V.ERROR.PX.99'
                E = DESC.RS
                CALL ERR
            CASE 1
                LLAVE.BUSQUEDA = 'EB-SLV.V.ERROR.PX.00'
                E = LLAVE.BUSQUEDA
                CALL ERR
        END CASE
    END ;*END ELSE COD.RS EQ '00'


        RETURN


APPLICATION.WRITE:
        TABLE.NAME = 'F.EB.SLV.COLECTOR.TRX.PAGO'
        TABLE.ID   = ID.NEW:TODAY:COLLECTOR.ID
        APPLICATION.RS<EB.SLV54.ID.TXN>           = TRX.ID
        APPLICATION.RS<EB.SLV54.ID.COLECTOR>      = COLLECTOR.ID
        APPLICATION.RS<EB.SLV54.MONTO>            = R.NEW(FT.DEBIT.AMOUNT)
        ;*APPLICATION.RS<EB.SLV54.FECHA>            = FECHA
        APPLICATION.RS<EB.SLV54.FECHA>            = TODAY
        APPLICATION.RS<EB.SLV54.AGENCIA>          = ID.COMPANY
        APPLICATION.RS<EB.SLV54.USUARIO>          = OPERATOR
        APPLICATION.RS<EB.SLV54.CODIGO.RS>        = COD.RS
        APPLICATION.RS<EB.SLV54.DESCRIPCION.RS>   = DESC.RS
        APPLICATION.RS<EB.SLV54.ID.T24>           = ID.NEW
        APPLICATION.RS<EB.SLV54.ID.PUNTO.EXPRESS> = ID.PEX
        APPLICATION.RS<EB.SLV54.ID.TRX.COLECTOR>  = COLLECTOR.REF
        APPLICATION.RS<EB.SLV54.COLECTOR>         = COLLECTOR.NAME
        APPLICATION.RS<EB.SLV54.TIPO.CONSULTA>    = 'PEX'
        APPLICATION.RS<EB.SLV54.CANAL.PAGO>       = 'GENERICO'
        APPLICATION.RS<EB.SLV54.CMP.RESERVA.11>   = 'RECIBIDO'
        APPLICATION.RS<EB.SLV54.CMP.RESERVA.10>   = '0.00'
        APPLICATION.RS<EB.SLV54.CMP.RESERVA.9>    = READ.BAND.PX
        APPLICATION.RS<EB.SLV54.CMP.RESERVA.8>    = 'PENDIENTE'
        CALL F.WRITE(TABLE.NAME,TABLE.ID,APPLICATION.RS)
        RETURN

ESCRIBIR.ARCHIVO:
    DIR.NAME= 'COLECTORES'
    R.ID   = 'PAGO_CODIGO_NPE_':TODAY:'.txt'
;* hacer que escriba un archivo

    OPENSEQ DIR.NAME,R.ID TO SEQ.PTR
    WRITESEQ TEXTO.ARCHIVO APPEND TO SEQ.PTR THEN
    END
    CLOSESEQ SEQ.PTR
    RETURN




END
