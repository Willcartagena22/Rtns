*-----------------------------------------------------------------------------
* <Rating>-15</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.E.NOF.CONT.MONEDA.AZUL(A.INFO)
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_System
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.AC.LOCKED.EVENTS
    $INSERT I_F.EB.SLV.STM.FAVORITES
    $INSERT I_F.EB.EXTERNAL.USER
    $INSERT I_F.EB.SLV.LIMITES.MONEDA.AZUL
    $INSERT I_F.EB.SLV.PAGOS.MONEDA.AZUL
    $INSERT I_F.EB.EXTERNAL.USER
    $INSERT I_F.CUSTOMER
    $INSERT I_F.AA.PRODUCT.ACCESS
	$INSERT I_F.AA.ARRANGEMENT.ACTIVITY
	$INSERT I_F.AA.ARRANGEMENT
*-----------------------------------------------------------------------------

    GOSUB INIT
    GOSUB OPENFILE
    GOSUB PROCESS


INIT:
    FN.MONEDA.AZUL='F.EB.SLV.PAGOS.MONEDA.AZUL'
    F.MONEDA.AZUL=''
    FN_LIMITES= 'F.EB.SLV.LIMITES.MONEDA.AZUL'
    F_LIMITES=''
    FN.LOCKED.EVENTS	= 'F.AC.LOCKED.EVENTS'
    F.LOCKED.EVENTS= ''
    FN.EXTERNAL.USER='F.EB.EXTERNAL.USER'
    F.EXTERNAL.USER=''
    FN_CUS 		= 'F.CUSTOMER'
    F_CUS 			= ''

    APPL.ARR ='AA.PRD.DES.PRODUCT.ACCESS'
    FIELDNAME.ARR	='LF.MONEDA.AZUL':VM:'LF.MINIMUM.AZUL':VM:'LF.MAXIMUM.AZUL':VM:'LF.PERCENT.AZUL':VM:'LF.DL.AZUL'
    CALL MULTI.GET.LOC.REF(APPL.ARR,FIELDNAME.ARR,POS.ARR)



    LOCATE 'ID.EXT.USER' IN D.FIELDS<1> SETTING ITEM.POS THEN
    SMS.CUSTOMERS = D.RANGE.AND.VALUE<ITEM.POS>
    END


    RETURN

OPENFILE:
    CALL OPF(FN.MONEDA.AZUL,F.MONEDA.AZUL)
    CALL OPF(FN_LIMITES, F_LIMITES)
    CALL OPF(FN.LOCKED.EVENTS, F.LOCKED.EVENTS)
    CALL OPF(FN_CUS, F_CUS)
    RETURN

LIMITES:
    SELECT.LIMITES.MAZUL = "SELECT " : FN_LIMITES : " WITH @ID EQ MDAZUL.BP1"
    ID.MAZUL="MDAZUL.BP1"
    CALL F.READ(FN_LIMITES,ID.MAZUL, LIMITES.MAZUL.REC,F_LIMITES, ERR.BI)
    TRANDIARIO=LIMITES.MAZUL.REC<EB.LTMAZUL.TRANDIARIO>;* Traer de la DJ
    TRANMENSUAL=LIMITES.MAZUL.REC<EB.LTMAZUL.TRANMENSUAL>
    PORCDIARIO=LIMITES.MAZUL.REC<EB.LTMAZUL.PORCDIARIO>;* cambiar por l�mite 300 en declaraci�n jurada
    PORCMENSUAL=LIMITES.MAZUL.REC<EB.LTMAZUL.PORCMENSUAL>;* cambiar por l�mite declaraci�n jurada

    CALL F.READ( FN.EXTERNAL.USER,SMS.CUSTOMERS, EXTERNAL.USER.REC, F.EXTERNAL.USER, ERR.EXT.USER)
    EU.CUSTOMER     = EXTERNAL.USER.REC<EB.XU.CUSTOMER>
    Y.ID.ISA = EXTERNAL.USER.REC<EB.XU.ARRANGEMENT>
    CALL AA.GET.ARRANGEMENT.CONDITIONS(Y.ID.ISA, 'PRODUCT.ACCESS', '', TODAY, R.ID.PROD.ACC, R.PRODUCT.ACCESS , ERR.PRODUCT.ACCESS)
    REC.PRODUCT.ACCESS = RAISE(R.PRODUCT.ACCESS) 
    Y.LF.MONEDA.AZUL=REC.PRODUCT.ACCESS<AA.PRODA.LOCAL.REF><1,POS.ARR<1,5>>
    PORCDIARIO=Y.LF.MONEDA.AZUL

    RETURN

RESERVAS:


;*Consulta Eventos AC.LOCKED.EVENT a partir del usuario Banca

    CALL GET.LOC.REF('AC.LOCKED.EVENTS','LF.CHQ.BENEFICI',POS.CR)
    CALL GET.LOC.REF('AC.LOCKED.EVENTS','LF.USER.BANCA',POS.NS)
SMS.CUSTOMERS='SDFG'

    IF SMS.CUSTOMERS THEN
        FECHAHOY=TODAY
        NO.OF.RECS.LK.EVENTS.DIA=0
        MES.ACTUAL=SUBSTRINGS(FECHAHOY,1,6)

        SELECT.LOCKED.EVENTS.MES = "SELECT " : FN.LOCKED.EVENTS : " WITH LF.USER.BANCA EQ " : EU.CUSTOMER	: " AND (DESCRIPTION EQ 'RESERVA HALCASH' OR DESCRIPTION EQ 'RESERVAMONEDAZULMOVIL') AND FROM.DATE LIKE %":MES.ACTUAL:"%"
        CALL EB.READLIST(SELECT.LOCKED.EVENTS.MES, ID.AC.LOCKED.EVEN.MES, '' , NO.OF.RECS.LK.EVENTS.MES, ERR.LK.EVENTS.MES)
        IF NO.OF.RECS.LK.EVENTS.MES NE 0 THEN

            TOTAL.AMOUNT=0
            FOR J = 1 TO NO.OF.RECS.LK.EVENTS.MES
                CALL F.READ(FN.LOCKED.EVENTS,ID.AC.LOCKED.EVEN.MES<J>, LOCKED.EVENTS.REC.MES,F.LOCKED.EVENTS, ERR.BI.MES)
                Y.ID= ID.AC.LOCKED.EVEN.MES<J>
                Y.FROMDATE     = LOCKED.EVENTS.REC.MES<AC.LCK.FROM.DATE>
                Y.CUSTOMER     = LOCKED.EVENTS.REC.MES<AC.LCK.LOCAL.REF><1,POS.NS>
                Y.ID.FAVORITES = LOCKED.EVENTS.REC.MES<AC.LCK.LOCAL.REF><1,POS.CR>
                Y.AMOUNT       = LOCKED.EVENTS.REC.MES<AC.LCK.LOCKED.AMOUNT>
                Y.TODATE       = LOCKED.EVENTS.REC.MES<AC.LCK.TO.DATE>
                RESERVA.TOTAL.AMOUNT.MES=Y.AMOUNT+RESERVA.TOTAL.AMOUNT.MES

                IF Y.FROMDATE EQ FECHAHOY THEN
                    NO.OF.RECS.LK.EVENTS.DIA=NO.OF.RECS.LK.EVENTS.DIA+1
                    RESERVA.TOTAL.AMOUNT.DIA=RESERVA.TOTAL.AMOUNT.DIA+Y.AMOUNT

                END

            NEXT J
        END
    END

    RESERVA.MES=NO.OF.RECS.LK.EVENTS.MES
    RESERVA.MONTO.MES=RESERVA.TOTAL.AMOUNT.MES
    RESERVA.DIA=NO.OF.RECS.LK.EVENTS.DIA
    RESERVA.MONTO.DIA=RESERVA.TOTAL.AMOUNT.DIA



    RETURN


PAGOS:
	MONTO.PAGO.MES=0
	MONTO.PAGO.DIA=0
    NO.OF.RECS.PAGOS=0
    SELECT.PAGOS= "SELECT " :FN.MONEDA.AZUL:" WITH CUSTOMER EQ ":EU.CUSTOMER:" AND FECHA.PAGO LIKE %":MES.ACTUAL:"%"
    CALL EB.READLIST(SELECT.PAGOS, ID.PAGO, '' , NO.OF.RECS.PAGOS, ERR.PAGOS)
    IF NO.OF.RECS.PAGOS NE 0 THEN
        FOR H = 1 TO NO.OF.RECS.PAGOS
            CALL F.READ(FN.MONEDA.AZUL,ID.PAGO<H>, PAGOS.MES,F.MONEDA.AZUL, ERR.PAGOS.MES)
            P.ID= ID.PAGO<H>
            P.CUSTOMER     = PAGOS.MES<EB.PMA.CUSTOMER>
            P.FECHA		   = PAGOS.MES<EB.PMA.FECHA.PAGO>
            PAGO.ACTUAL=PAGOS.MES<EB.PMA.MONTO>
            MONTO.PAGO.MES=MONTO.PAGO.MES+ PAGO.ACTUAL
            IF P.FECHA EQ FECHAHOY THEN
                MONTO.PAGO.DIA=MONTO.PAGO.DIA+ PAGO.ACTUAL
                NO.OF.RECS.PAGOS.DIA=NO.OF.RECS.PAGOS.DIA+1

            END

        NEXT H
    END

    PAGOS.DIA=NO.OF.RECS.PAGOS.DIA
    PAGOS.MES=NO.OF.RECS.PAGOS

    RETURN

PROCESS:
    GOSUB LIMITES
    GOSUB RESERVAS
    GOSUB PAGOS


    CONTADOR.DIA=RESERVA.DIA+PAGOS.DIA
    CONTADOR.MES=RESERVA.MES+PAGOS.MES


    TOTAL.MONTO.DIA=RESERVA.MONTO.DIA+MONTO.PAGO.DIA
    TOTAL.MONTO.MES=RESERVA.MONTO.MES+MONTO.PAGO.MES

;*Extraer Ingresos mensuales del cliente
    CALL F.READ(FN_CUS, EU.CUSTOMER, R_CUS, F_CUS, F_CUS_ERR)
    CALL GET.LOC.REF('CUSTOMER', 'LF.AML.AMT.TCIB', POSMontoIng)
    INGRESOS = R_CUS<EB.CUS.LOCAL.REF, POSMontoIng>
    PORCDIARIO=PORCDIARIO
    PORCMENSUAL=PORCMENSUAL

    PORCMENSUAL=INGRESOS


    A.INFO<-1>=  CONTADOR.DIA:'*':CONTADOR.MES:'*':TOTAL.MONTO.DIA:'*':TOTAL.MONTO.MES:'*':RESERVA.DIA:'*':RESERVA.MES:'*':RESERVA.MONTO.DIA:'*':RESERVA.MONTO.MES:'*':PAGOS.DIA:'*':PAGOS.MES:'*':MONTO.PAGO.DIA:'*':MONTO.PAGO.MES:'*':TRANDIARIO:'*':TRANMENSUAL:'*':PORCDIARIO:'*':PORCMENSUAL


    RETURN

    PRINT 'A.INFO  : ' :  A.INFO<-1>


    END
