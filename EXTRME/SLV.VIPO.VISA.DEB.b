*-----------------------------------------------------------------------------
* <Rating>-46</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.VIPO.VISA.DEB(TOTAL,SUBTOT,CAJERO,GIROCOME,DIRECCION,NIT,NOMBRE,ARCHIVO,CANTLETRAS,DET,DET2,DET3,GRAV)
*-----------------------------------------------------------------------------
* <Rating>3918</Rating>

*-----------------------------------------------------------------------------

    $INSERT I_F.EB.SLV.GLOBAL.PARAM
     $INSERT I_COMMON
    $INSERT I_EQUATE
*------------------------------------------------------------------------------
    GOSUB INIT
    GOSUB OPENFILES
    GOSUB COMPOSE_DATA

    RETURN
*------------------------------------------------------------------------------
INIT:
;* Aplicaciones a utilizar
    FN_CUS 			= 'F.CUSTOMER'
    F_CUS 			= ''
    FN_INDUST		= 'F.INDUSTRY'
    F_INDUST 		= ''
    FN_SLVPA 		= 'F.EB.SLV.GLOBAL.PARAM'
    F_SLVPA 		= ''
  
;*variables de trabajo
    BANDERA			= ''
    DATOS 			= ''
    CADENA 			= ''
    TIPO_FISCAL 	= ''
    RUTA.ARCHIVO 	= ''
    NOMBRE.PLANTILLA = ''
   
    FECHA.FORMATEADA = TODAY[7,2]:TODAY[5,2]:TODAY[1,4]
    FECHA_EMISION 	= TODAY[7,2]: '/' : TODAY[5,2] : '/' : TODAY[1,4]
    DESCRIPCION_STATIC='Ingresos varios'


;*Variables de cadena de datos para docuemento
  
    CORP='SV0010001'
  
    EMPRESAS=CORP
 
    AGENCIAS=CORP

    TIPOFISC ='FACTURA'
    CODTRANS =''
    CORRELAT =''
    FECHAEMI =FECHA_EMISION
    
    CODCUSTO =''
    NOMBRERA =NOMBRE
    DIRECCIO =DIRECCION
    NITCLIEN =NIT
    NUMERNRC =''
    GRAVADAS=GRAV
   ;* IF TIPOCOBRO EQ 'MEMBRESIA' THEN
   ;* DETALLE  ='DETALLES!COMISIONES MEMBRESIA!':'0.00!0.00!':GRAVADAS:'!'
    DETALLE=DET:GRAVADAS:'!'
    DETALLE2=DET2
    DETALLE3=DET3
   ;* DETALLE2 ='DETALLES!TARJETA DE DEBITO ':TARJETA:'!':'!':'!'
    ;*DETALLE3=''
   ;* END
    ;*IF TIPOCOBRO EQ 'TRANSACCION' THEN
   ;* DETALLE  ='DETALLES!COMISIONES ATM ':CAJERO:'!':'0.00!0.00!':GRAVADAS:'!'
    ;*DETALLE2 ='DETALLES!COMISION DEL DIA ':FECHA_EMISION:'!':'!':'!'
    ;*DETALLE3 ='DETALLES!TARJETA DE DEBITO ':TARJETA:'!':'!':'!'
    ;*END
    VENTAGRA =TOTAL
    IMPUESTO =''
    SUBTOTAL =SUBTOT
    VENTANOS =''
    VENTAEXE =''
    IVARETEN =''
    IVAPERCI =''
    VENTANET =''
    VENTAIMP =''
    VENTATOT =TOTAL
    CANTILET =CANTLETRAS



    RETURN

*-------------------------------------------------------------------------------------
*Apertura de areas de trabajo
*-------------------------------------------------------------------------------------
OPENFILES:
;* Apertura de archivos a utilizar

    CALL OPF(FN_SLVPA, F_SLVPA)

	
RETURN

COMPOSE_DATA:

    DATOS := 'EMPRESAS!' : EMPRESAS : CHAR(10)
    DATOS := 'TIPOFISC!' : TIPOFISC : CHAR(10)
    DATOS := 'CODTRANS!' : CODTRANS : CHAR(10)
    DATOS := 'CORRELAT!' : CHAR(10)
    DATOS := 'FECHAEMI!' : FECHAEMI : CHAR(10)
    DATOS := 'AGENCIAS!' : AGENCIA	: CHAR(10)
    DATOS := 'CODCUSTO!' : CODCUSTO : CHAR(10)
    DATOS := 'NOMBRERA!' : NOMBRERA	 : CHAR(10)
    DATOS := 'DIRECCIO!' : DIRECCIO : CHAR(10)
    DATOS := 'GIROCOME!' : GIROCOME : CHAR(10)
    DATOS := 'NITCLIEN!' : NITCLIEN : CHAR(10)
    DATOS := 'NUMERNRC!' : NUMERNRC : CHAR(10)
    DATOS :=  DETALLE : CHAR(10)
    DATOS :=  DETALLE2 : CHAR(10)
    DATOS :=  DETALLE3 : CHAR(10)
    DATOS := 'VENTAGRA!' : FMT(VENTAGRA, 'R2') : CHAR(10)
    DATOS := 'IMPUESTO!' : FMT(IMPUESTO, 'R2') : CHAR(10)
    DATOS := 'SUBTOTAL!' : FMT(SUBTOTAL, 'R2') : CHAR(10)
    DATOS := 'VENTANOS!' : FMT(VENTANOS, 'R2') : CHAR(10)
    DATOS := 'VENTAEXE!' : FMT(VENTAEXE, 'R2') : CHAR(10)
    DATOS := 'IVARETEN!' : '(':FMT(IVARETEN, 'R2'):')' : CHAR(10)
    DATOS := 'IVAPERCI!' : FMT(IVAPERCI, 'R2') : CHAR(10)
    DATOS := 'VENTANET!' : FMT(VENTANET, 'R2') : CHAR(10)
    DATOS := 'VENTAIMP!' : FMT(VENTAIMP, 'R2') : CHAR(10)
    DATOS := 'VENTATOT!' : FMT(VENTATOT, 'R2') : CHAR(10)
    DATOS := 'CANTILET!' : CANTILET : CHAR(10)

    A_DATA<-1> = DATOS

*------------------------------------------------------------------------------------------

*generar archivo para documento
    CALL F.READ(FN_SLVPA, 'RUTA.FACT.COM', R.TABLE.PA, F_SLVPA, F.ERR.PA)
   DIR.NAME = R.TABLE.PA<EB.SLV39.VALOR.PARAM>
		;*DIR.NAME = 'C:\temp'
    NOMBREF=ARCHIVO
    R.ID = NOMBREF : '.txt'

;* Plantilla a utilizar
    CALL F.READ(FN_SLVPA, 'PLANTILLA.FCF', R.TABLE.PA, F_SLVPA, F.ERR.PA)
    PLANTILLA = '(' : R.TABLE.PA<EB.SLV39.VALOR.PARAM> : ') STARTLM'

;* Armando la cadena para el archivo
    CADENA := '%!' : CHAR(10)
    CADENA := PLANTILLA : CHAR(10)
    CADENA := UPCASE(DATOS)
    CADENA := '%%EOF': CHAR(10)
;* Creando el archivo
    OPENSEQ DIR.NAME, R.ID TO SEQ.PTR ELSE

    

;* Escribiendo la cadena en el archivo

    WRITESEQ CADENA ON SEQ.PTR ELSE
    E = 'EB-SLV.DAT.WRT.FAIL'
    CALL ERR
    END

;* Cerrando el archivo
    CLOSESEQ SEQ.PTR
END
    RETURN


END
