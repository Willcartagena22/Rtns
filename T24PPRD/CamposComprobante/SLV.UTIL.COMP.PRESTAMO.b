*-----------------------------------------------------------------------------
* <Rating>-71</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.UTIL.COMP.PRESTAMO
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.EB.SLV.PAGOS.PRESTAMOS
    $INSERT I_F.FUNDS.TRANSFER



    GOSUB INIT
    GOSUB OPENFILE
    GOSUB PROCESS

INIT:
    FN.PAGOS.PRESTAMOS='F.EB.SLV.PAGOS.PRESTAMOS'
    F.PAGOS.PRESTAMOS= ''

    FN.FUNDS.TRANSFER='F.FUNDS.TRANSFER'
    F.FUNDS.TRANSFER=''

    FN.FT.BULK.MAS	= 'F.FT.BULK.MASTER'
    F.FT.BULK.MAS	= ''

    FN.CUS 	    = 'F.CUSTOMER'
    F.CUS		= ''

    RETURN


OPENFILE:
    CALL OPF(FN.PAGOS.PRESTAMOS, F.PAGOS.PRESTAMOS)
    CALL OPF(FN.FUNDS.TRANSFER, F.FUNDS.TRANSFER)


    RETURN


PROCESS:

    TYPE.TRAN = R.NEW(FT.TRANSACTION.TYPE)
    FECHA.VALOR = R.NEW(FT.CREDIT.VALUE.DATE)


    IF TYPE.TRAN EQ 'ACRP' OR TYPE.TRAN EQ 'ACPT' THEN
        IDAPP = ID.NEW
        R.SCT<EB.PAGPR.FUNDS.TRANSFER> =IDAPP
        CUENTACRED = R.NEW(FT.CREDIT.ACCT.NO)
        TIPOCUENTA=CUENTACRED[1,3]

        IF TIPOCUENTA NE 'USD' THEN

*            V.S.VIDA = CUENTACRED
*            CALL SLV.UTIL.IMP.P.GET.S.VIDA(V.S.VIDA) ;*Seg. Vida
*            R.SCT<EB.PAGPR.SEGURO.DEUDA> = V.S.VIDA


*            V.S.DANIO = CUENTACRED
*            CALL SLV.UTIL.IMP.P.GET.S.DANIO(V.S.DANIO) ;*Seg. Daño
*            R.SCT<EB.PAGPR.SEGURO.DANIO> =V.S.DANIO
*
*
*
*            V.IVA.SEG.DANIO = CUENTACRED
*            CALL SLV.UTIL.IMP.P.GET.IVA.S.D(V.IVA.SEG.DANIO)  ;*IVA Seg. Daño
*            R.SCT<EB.PAGPR.IVA.SEGURO.DANIO> = V.IVA.SEG.DANIO
*
*
*            V.INT.MORA = CUENTACRED
*            CALL SLV.UTIL.IMP.P.GET.I.MOR(V.INT.MORA) ;*Int Moratorio
*            R.SCT<EB.PAGPR.INTERES.MORATORIO> =V.INT.MORA
*
*
*
*            V.INT.CORR = CUENTACRED
*            CALL SLV.UTIL.IMP.P.GET.I.CORR(V.INT.CORR) ;*Int Corriente
*            R.SCT< EB.PAGPR.INTERES.CORRIENTE> =V.INT.CORR
*
*
*            V.ABONO.CAPITAL = CUENTACRED
*            CALL SLV.UTIL.IMP.P.GET.CAP(V.ABONO.CAPITAL) ;*Abono a Capital
*            R.SCT<EB.PAGPR.ABONO.CAPITAL> =V.ABONO.CAPITAL
*
*
*            V.S.OTROS = CUENTACRED
*            CALL SLV.UTIL.IMP.P.GET.OTROS.C(V.S.OTROS) ;*Otros Cargos
*            R.SCT<EB.PAGPR.OTROS.CARGOS> =V.S.OTROS
*
*
*            V.TASA.INT = CUENTACRED
*            CALL SLV.UTIL.IMP.P.GET.IP(V.TASA.INT) ;*Tasa de Interes
*            R.SCT<EB.PAGPR.TASA.INTERES> =V.TASA.INT
*
*
*
*            V.TASA.MORA = CUENTACRED
*            CALL SLV.UTIL.IMP.P.GET.IM(V.TASA.MORA) ;* Tasa Mora
*            R.SCT<EB.PAGPR.TASA.MORA> = V.TASA.MORA
*
*
*            V.SAL.ANT = CUENTACRED
*            CALL SLV.UTIL.IMP.P.GET.SAL.ANT(V.SAL.ANT) ;* Salario Anterior
*            R.SCT<EB.PAGPR.SALDO.ANTERIOR> =V.SAL.ANT
*
*
*
*            V.SAL.ACT = CUENTACRED
*            CALL SLV.UTIL.IMP.P.GET.SAL.ACT(V.SAL.ACT)    ;* Salario Actual
*            R.SCT<EB.PAGPR.SALDO.ACTUAL> =V.SAL.ACT
*            
*            
*            MONTO.PRESTAMO=CUENTACRED
*            CALL SLV.UTIL.IMP.P.GET.MON(MONTO.PRESTAMO) ;*MONTO PRESTAMO
*            R.SCT<EB.PAGPR.MONTO.PRESTAMO> =MONTO.PRESTAMO
			R.SCT<EB.PAGPR.ESTADO.COMP>='PENDIENTE'
            INWARD.PAY = R.NEW(FT.INWARD.PAY.TYPE)
            BULK.M= FIELD(FIELD(INWARD.PAY,'.',1),'-',2)
            BULK.I= FIELD(INWARD.PAY,'-',2)
			R.SCT<EB.PAGPR.FECHA.CREACION>=FECHA.VALOR
			R.SCT<EB.PAGPR.FECHA.SISTEMA>=TODAY
            R.SCT<EB.PAGPR.ID.BULK.MASTER>=BULK.M
            R.SCT<EB.PAGPR.ID.BULK.ITEM>=BULK.I
            R.SCT<EB.PAGPR.INPUTTER> = OPERATOR
            R.SCT<EB.PAGPR.AUTHORISER>= OPERATOR
            R.SCT<EB.PAGPR.CURR.NO> += 1
            X = OCONV(DATE(),"D-")
            V$TIMEDATE = TIMEDATE()
            V$TIMEDATE = V$TIMEDATE[1,5]
            CONVERT ":" TO "" IN V$TIMEDATE
            X = X[9,2] : X[1,2] : X[4,2] : V$TIMEDATE
            R.SCT<EB.PAGPR.DATE.TIME>		   = X
            R.SCT<EB.PAGPR.RECORD.STATUS>	   = 'AUTH'

            CALL F.WRITE (FN.PAGOS.PRESTAMOS,IDAPP, R.SCT)




        END



    END


    RETURN


    END
