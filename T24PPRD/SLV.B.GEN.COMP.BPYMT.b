*-----------------------------------------------------------------------------
* <Rating>1361</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.B.GEN.COMP.BPYMT
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
* Vburgos      06022019    Se agrega LF.AMOUNT al total del comprobante 
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.EB.SLV.PAGOS.PRESTAMOS
    $INSERT I_F.CUSTOMER
    $INSERT I_F.FT.BULK.MASTER
    $INSERT I_F.EB.SLV.GLOBAL.PARAM
*-----------------------------------------------------------------------------

    GOSUB INIT
    GOSUB OPENFILE
    GOSUB OBTENER.DATOS



INIT:
    FN.PAGOS.PRESTAMOS='F.EB.SLV.PAGOS.PRESTAMOS'
    F.PAGOS.PRESTAMOS= ''

    FN.FUNDS.TRANSFER='F.FUNDS.TRANSFER'
    F.FUNDS.TRANSFER=''

    FN.FT.BULK.MAS	= 'F.FT.BULK.MASTER'
    F.FT.BULK.MAS	= ''

    FN.CUS 	    = 'F.CUSTOMER'
    F.CUS		= ''

    FN.FT.BULK.MAS	= 'F.FT.BULK.MASTER'
    F.FT.BULK.MAS	= ''

    FN.PARAM 		= 'F.EB.SLV.GLOBAL.PARAM'
    F.PARAM  		= ''

    RETURN


OPENFILE:
    CALL OPF(FN.PAGOS.PRESTAMOS, F.PAGOS.PRESTAMOS)
    CALL OPF(FN.FUNDS.TRANSFER, F.FUNDS.TRANSFER)
    CALL OPF(FN.CUS, F.CUS)
    CALL OPF(FN.FT.BULK.MAS, F.FT.BULK.MAS)
    CALL OPF(FN.PARAM, F.PARAM)


    RETURN


OBTENER.DATOS:

    SELECT.PRESTAMOS = "SELECT " : FN.PAGOS.PRESTAMOS : " WITH FECHA.SISTEMA EQ '": TODAY:"' AND ESTADO.COMP EQ 'PENDIENTE'"
    CALL EB.READLIST(SELECT.PRESTAMOS, LIST.PREST, '', NO.OF.PREST, ERR.PREST)

IF LIST.PREST THEN

    FOR I = 1 TO NO.OF.PREST

        PRESTAMO=LIST.PREST<I>
        CALL F.READ(FN.PAGOS.PRESTAMOS, PRESTAMO, R.PREST, F.PAGOS.PRESTAMOS, ERR.PRESTID)
        NUMTTFT = R.PREST<EB.PAGPR.FUNDS.TRANSFER>

        CALL F.READ(FN.FUNDS.TRANSFER, NUMTTFT, R.FT, F.FUNDS.TRANSFER, ERR.FT)
        CUENTADEB=R.FT<FT.DEBIT.ACCT.NO>
        CUSTOMERID = R.FT<FT.CREDIT.CUSTOMER>
        INWARD.PAY = R.FT<FT.INWARD.PAY.TYPE>
        FTBULKID = FIELD(FIELD(INWARD.PAY,'.',1),'-',2)
        CALL F.READ(FN.FT.BULK.MAS, FTBULKID, R.FT.BULK.MASTER, F.FT.BULK.MAS, ERR.FT.BULK.MAS)
        FILENAME = R.FT.BULK.MASTER<FT.BLK.MAS.UPLOAD.REFERENCE>
        TRANSACTIONTYPE= R.FT<FT.TRANSACTION.TYPE>

        IF TRANSACTIONTYPE EQ 'ACPT' THEN
            ;*Datos Empresa si la planilla viene desde banca en linea
            IDEMPRESA= R.FT.BULK.MASTER<FT.BLK.MAS.CUSTOMER>
            CALL F.READ(FN.CUS, IDEMPRESA, R.CUSTOMERE, F.CUS, F.CUSTOMER.ERRE)
            CALL GET.LOC.REF('CUSTOMER', 'LF.RAZON.SOCIAL', POS.RAZON.SOCIAL)
            NOMBREEMPRESA = R.CUSTOMERE<EB.CUS.LOCAL.REF, POS.RAZON.SOCIAL>
            FILENAME=NOMBREEMPRESA
            USUARIO = '  '
           

        END
        
        ELSE
        USUARIO = FIELD(R.FT<FT.INPUTTER>,'_',2)
        END


        ;*Datos cliente
        CALL F.READ(FN.CUS, CUSTOMERID, R.CUSTOMER, F.CUS, F.CUSTOMER.ERR)
        PRIMERNOMBRE   = R.CUSTOMER<EB.CUS.NAME.1>
        PRIMERAPELLIDO = R.CUSTOMER<EB.CUS.TEXT>
        NUMTTFTFILE = FILENAME:'-':PRIMERNOMBRE:'_':PRIMERAPELLIDO:'_':'_':Y.DATE.FILE.NAME

        CONT = LEN(CUENTADEB)

        IF  CONT NE 0 THEN
            
             R.PREST<EB.PAGPR.ESTADO.COMP>='PROCESANDOSE'
		     CALL F.WRITE (FN.PAGOS.PRESTAMOS,PRESTAMO, R.PREST)
            AGENCIA = R.FT<FT.CO.CODE>
            
            CUENTACRED=R.FT<FT.CREDIT.ACCT.NO>
            
        	V.TASA.INT = CUENTACRED
        	CALL SLV.UTIL.IMP.P.GET.IP(V.TASA.INT) ;*Tasa de Interes
            TASAINTERES= V.TASA.INT
            R.PREST<EB.PAGPR.TASA.INTERES> =V.TASA.INT
            
            V.TASA.MORA = CUENTACRED
            CALL SLV.UTIL.IMP.P.GET.IM(V.TASA.MORA) ;* Tasa Mora         
            TASAMORA=V.TASA.MORA
            R.PREST<EB.PAGPR.TASA.MORA> = V.TASA.MORA

            
            FECHASISTEMA = R.PREST<EB.PAGPR.FECHA.SISTEMA>
		    FECHACREACION = R.PREST<EB.PAGPR.FECHA.CREACION>
            BULKITEM= R.PREST<EB.PAGPR.ID.BULK.ITEM>
            BULKMASTER = R.PREST<EB.PAGPR.ID.BULK.MASTER>
            
            
            V.S.VIDA = CUENTACRED
            CALL SLV.UTIL.IMP.P.GET.S.VIDA(V.S.VIDA) ;*Seg. Vida
            SegVida = V.S.VIDA
            R.PREST<EB.PAGPR.SEGURO.DEUDA> = V.S.VIDA
            
                        
            V.S.DANIO = CUENTACRED
            CALL SLV.UTIL.IMP.P.GET.S.DANIO(V.S.DANIO) ;*Seg. Daño
            SegDanio = V.S.DANIO
            R.PREST<EB.PAGPR.SEGURO.DANIO> =V.S.DANIO
            
            
            V.IVA.SEG.DANIO = CUENTACRED
            CALL SLV.UTIL.IMP.P.GET.IVA.S.D(V.IVA.SEG.DANIO)  ;*IVA Seg. Daño
			IvaSegDanio = V.IVA.SEG.DANIO
			R.PREST<EB.PAGPR.IVA.SEGURO.DANIO> = V.IVA.SEG.DANIO
			
			
			V.INT.MORA = CUENTACRED
            CALL SLV.UTIL.IMP.P.GET.I.MOR(V.INT.MORA) ;*Int Moratorio
            IntMoratorio = V.INT.MORA
            R.PREST<EB.PAGPR.INTERES.MORATORIO> =V.INT.MORA
            
            V.INT.CORR = CUENTACRED
            CALL SLV.UTIL.IMP.P.GET.I.CORR(V.INT.CORR) ;*Int Corriente
            R.PREST< EB.PAGPR.INTERES.CORRIENTE> =V.INT.CORR
            IntCorriente = V.INT.CORR
            
            
            V.ABONO.CAPITAL = CUENTACRED
            CALL SLV.UTIL.IMP.P.GET.CAP(V.ABONO.CAPITAL) ;*Abono a Capital
            R.PREST<EB.PAGPR.ABONO.CAPITAL> =V.ABONO.CAPITAL
            Capital = V.ABONO.CAPITAL
            
            
            V.S.OTROS = CUENTACRED
            CALL SLV.UTIL.IMP.P.GET.OTROS.C(V.S.OTROS) ;*Otros Cargos
            R.PREST<EB.PAGPR.OTROS.CARGOS> =V.S.OTROS
            OtrosCargos = V.S.OTROS
            
            
            V.SAL.ANT = CUENTACRED
            CALL SLV.UTIL.IMP.P.GET.SAL.ANT(V.SAL.ANT) ;* Saldo Anterior
            R.PREST<EB.PAGPR.SALDO.ANTERIOR> =V.SAL.ANT
            SaldoAnterior = V.SAL.ANT
            
            V.SAL.ACT = CUENTACRED
            CALL SLV.UTIL.IMP.P.GET.SAL.ACT(V.SAL.ACT)    ;* Salario Actual
            R.PREST<EB.PAGPR.SALDO.ACTUAL> =V.SAL.ACT
            SaldoActual =V.SAL.ACT
            
            MONTO.PRESTAMO=CUENTACRED
            CALL SLV.UTIL.IMP.P.GET.MON(MONTO.PRESTAMO) ;*MONTO PRESTAMO
            R.PREST<EB.PAGPR.MONTO.PRESTAMO> =MONTO.PRESTAMO
            MontoPrestamo=MONTO.PRESTAMO
                        
            
            Y.DATETIME = R.FT<FT.DATE.TIME>
            CLIENTE = R.FT<FT.CREDIT.CUSTOMER>
            CALL SLV.UTIL.GET.NOM.CLIENTE(CLIENTE)
            NumPrestamo=R.FT<FT.CREDIT.ACCT.NO>
            
            CALL GET.LOC.REF ('FUNDS.TRANSFER', 'LF.AMOUNT', LF.AMOUNT)
	    	Y.LF.AMOUNT = R.FT<FT.LOCAL.REF><1, LF.AMOUNT> 
	    	
	    	IF Y.LF.AMOUNT THEN
	    		TotalPagado = Y.LF.AMOUNT
	    	END
	    	ELSE
	    		TotalPagado = R.FT<FT.DEBIT.AMOUNT>
	            IF TotalPagado THEN
	           		TotalPagado=TotalPagado
	            END            
	            ELSE
	            	TotalPagado=R.FT<FT.CREDIT.AMOUNT>
	            END
	    	END
           

            GOSUB PARSE_DATETIME
            FECHAHORA = Y.DATETIME
		GOSUB GENERAR_ARCHIVO
        GOSUB GENERAR_SPOOL
        
        R.PREST<EB.PAGPR.ESTADO.COMP>='GENERADO'
		CALL F.WRITE (FN.PAGOS.PRESTAMOS,PRESTAMO, R.PREST)
		
        END


    NEXT I

END
    RETURN

GENERAR_ARCHIVO:
*-----------------------------------------------------------------------------

    GOSUB FORMATO_VALOR
    stringData := 'Agencia=':AGENCIA:';'
    stringData := 'Usuario=':USUARIO:';'
    stringData := 'FechaHora=':FECHAHORA:';'
    stringData := 'TasaInteres=':TASAINTERES:';'
    stringData := 'NumTTFT=':NUMTTFT:';'
    stringData := 'TasaMora=':TASAMORA:';'
    stringData := 'Cliente=':CLIENTE:';'
    stringData := 'MontoPrestamo=':MontoPrestamo:';'
    stringData := 'NumPrestamo=':NumPrestamo:';'
    stringData := 'SegVida=':SegVida:';'
    stringData := 'SegDanio=':SegDanio:';'
    stringData := 'IvaSegDanio=':IvaSegDanio:';'
    stringData := 'SaldoAnterior=':SaldoAnterior:';'
    stringData := 'IntMoratorio=':IntMoratorio:';'
    stringData := 'SaldoActual=':SaldoActual:';'
    stringData := 'IntCorriente=':IntCorriente:';'
    stringData := 'Capital=':Capital:';'
    stringData := 'OtrosCargos=':OtrosCargos:';'
    stringData := 'TotalPagado=':TotalPagado:';'
    stringData := 'inward.pay=':INWARD.PAY:';';*asociar el bulkPayment
    stringData := 'fileName=':FILENAME:';'

    idArchivo = 'ReciboPago-':CHANGE(NUMTTFTFILE," ",""):'-':NUMTTFT:'.txt'

    GOSUB REINICIO_VARIABLE

    RETURN
*-----------------------------------------------------------------------------
GENERAR_SPOOL:
    Y.RUTA.SPOOL = 'RUTA.SPOOL.FILES'
    direcArchivo ='SPOOL.FILES'
    CALL F.READ(FN.PARAM, Y.RUTA.SPOOL, R.PARAM.SPOOL, F.PARAM, ERR.PAR.SPOOL)
    IF R.PARAM.SPOOL THEN
        direcArchivo = R.PARAM.SPOOL<EB.SLV39.VALOR.PARAM><1>
    END

    OPENSEQ direcArchivo, idArchivo TO SEQ.PTR ELSE
        CREATE SEQ.PTR ELSE
        ETEXT = 'No se puede crear el archivo.'
        CALL STORE.END.ERROR
    END
;* Escribiendo los datos en el archivo
    WRITESEQ stringData ON SEQ.PTR ELSE
    ETEXT = 'No se pueden escribir los datos en el archivo.'
    CALL STORE.END.ERROR
    END

;* Cerrando el archivo
    CLOSESEQ SEQ.PTR

    RETURN


FORMATO_VALOR:
*-----------------------------------------------------------------------------
    CALL SLV.UTIL.FORMATO.MONEDA(MontoPrestamo)
    CALL SLV.UTIL.FORMATO.MONEDA(SegVida)
    CALL SLV.UTIL.FORMATO.MONEDA(SegDanio)
    CALL SLV.UTIL.FORMATO.MONEDA(IvaSegDanio)
    CALL SLV.UTIL.FORMATO.MONEDA(SaldoAnterior)
    CALL SLV.UTIL.FORMATO.MONEDA(IntMoratorio)
    CALL SLV.UTIL.FORMATO.MONEDA(SaldoActual)
    CALL SLV.UTIL.FORMATO.MONEDA(IntCorriente)
    CALL SLV.UTIL.FORMATO.MONEDA(Capital)
    CALL SLV.UTIL.FORMATO.MONEDA(OtrosCargos)
    CALL SLV.UTIL.FORMATO.MONEDA(TotalPagado)
    RETURN

REINICIO_VARIABLE:
*-----------------------------------------------------------------------------
    SegVida 	  = 0.00
    SegDanio 	  = 0.00
    IvaSegDanio   = 0.00
    SaldoAnterior = 0.00
    IntMoratorio  = 0.00
    SaldoActual   = 0.00
    IntCorriente  = 0.00
    Capital 	  = 0.00
    OtrosCargos   = 0.00
    TotalPagado   = 0.00
    Y.TOT.PAGO	  = 0.00
    INWARD.PAY	  = ''
    AGENCIA 	  = ''
    USUARIO 	  = ''
    FECHAHORA 	  = ''
    TASAINTERES 	  = ''
    NUMTTFT 	  = ''
    TASAMORA 	  = ''
    CLIENTE 	  = ''
    MontoPrestamo 	  = ''
    NumPrestamo 	  = ''
    INWARD.PAY 	  = ''
    FILENAME 	  = ''
    CLIENTE 	  = ''
    RETURN

PARSE_DATETIME:
*-----------------------------------------------------------------------------
    UTECDATETIME   = Y.DATETIME
    LOCALZONEDATE1 = OCONV(LOCALDATE(UTECDATETIME,LOCALZONE),'D4/C')
;*Del formato MMDDYYYY (03/20/2017) obtener dia(20), mes(03) y año(2017)
    LOCALDAY       = OCONV(LOCALZONEDATE1,'G1/1') ;*Obtener dias en formato 2 digitos
    LOCALMON	   = OCONV(LOCALZONEDATE1,'G0/1') ;*Obtener mes en formato 2 digitos
    LOCALYEAR	   = OCONV(LOCALZONEDATE1,'G2/1') ;*Obtener año en formato 4 digitos
    LOCALZONETIME1 = OCONV(LOCALTIME(UTECDATETIME, LOCALZONE),'MTS')
    Y.DATETIME 	   = LOCALDAY:'/':LOCALMON:'/':LOCALYEAR:' ':LOCALZONETIME1
    Y.DATE.FILE.NAME = CHANGE(LOCALZONEDATE1,"/","")
    RETURN



    END
