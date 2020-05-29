*-----------------------------------------------------------------------------
* <Rating>-43</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.STMT.NARR.VISA(APPLIC.ID,APPLIC.REC,STMT.ID,STMT.REC,OUT.TEXT)
*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.FUNDS.TRANSFER
	$INSERT I_F.EB.SLV.GLOBAL.PARAM
	$INSERT I_F.EB.SLV.COMISIONES.TARJETAS
    $INSERT I_F.EB.SLV.GLOBAL.PARAM
*-----------------------------------------------------------------------------

    GOSUB INIT 
    GOSUB PROCESS 

*-----------------------------------------------------------------------------

INIT:

    FN.FUNDS.TRANSFER = 'F.FUNDS.TRANSFER'
    F.FUNDS.TRANSFER = ''
    FN.SCT='F.EB.SLV.COMISIONES.TARJETAS'
    F.SCT=''
  

	FN_SLVPA= 'F.EB.SLV.GLOBAL.PARAM'
    F_SLVPA = ''
    
    


    RETURN
    
OPEN:

  CALL OPF(FN.SCT,F.SCT)
  CALL OPF(FN.FUNDS.TRANSFER,F.FUNDS.TRANSFER)
  CALL OPF(FN_SLVPA,F_SLVPA)
RETURN


PROCESS:
     CALL F.READ(FN_SLVPA, 'ACC.VISA.CARGO.CLAS', R.TABLE.PA, F_SLVPA, F.ERR.PA)
        CARGOCLASICA =R.TABLE.PA<EB.SLV39.VALOR.PARAM>

        CALL F.READ(FN_SLVPA, 'ACC.VISA.CARGO.ORO', R.TABLE.PA, F_SLVPA, F.ERR.PA)
        CARGOORO =R.TABLE.PA<EB.SLV39.VALOR.PARAM>

        CALL F.READ(FN_SLVPA, 'ACC.VISA.CARGO.PLATINO', R.TABLE.PA, F_SLVPA, F.ERR.PA)
        CARGOPLATNO =R.TABLE.PA<EB.SLV39.VALOR.PARAM>
		
		CALL F.READ(FN_SLVPA, 'ACC.VISA.DEB.MEM', R.TABLE.PA, F_SLVPA, F.ERR.PA)
        CUENTAMEMBRESIA =R.TABLE.PA<EB.SLV39.VALOR.PARAM>
        
    ID.FT = FIELD(APPLIC.ID, ";", 1)
   
        GOSUB READ.FT 

        CALL GET.LOC.REF('FUNDS.TRANSFER','LF.IDOFS.CARD', LocPosIdOfsCard)
        IDOFS=R.FT<FT.LOCAL.REF,LocPosIdOfsCard>
    	
    	CALL GET.LOC.REF('FUNDS.TRANSFER','LF.ACC.ATM.COM', LocPosIdOfsCard2)
        ATM=R.FT<FT.LOCAL.REF,LocPosIdOfsCard2>
    	
    	
;*STMT.SCT = "SELECT ":FN.SCT:" STATUS EQ '0' AND CARD.TYPE EQ 'D' AND LF.IDOFS.CARD EQ":IDOFS
   
   
    R.FT = ''
    CALL F.READ(FN.SCT,IDOFS,AR.SCT,F.SCT,ERR.SCT)

		CUENTACLIENTE=AR.SCT<EB.SLV16.ACC.CUSTOMER>
        CUENTADEBITO1=AR.SCT<EB.SLV16.ACC.DEBITO>
        CUENTACREDITO1=AR.SCT<EB.SLV16.ACC.CREDITO>
        MONTO1=AR.SCT<EB.SLV16.AMOUNT>
        ORDENBANCO=AR.SCT<EB.SLV16.ORDERING.BANK>
      
        UBICACION=AR.SCT<EB.SLV16.UBICATION>
     
        ORIGEN=AR.SCT<EB.SLV16.ORIGIN>
      
        
    IF ORDENBANCO EQ 'Comision Visa Deb'    THEN
    
    	IF UBICACION EQ 'NACIONAL' THEN
    	
		    IF ORIGEN EQ 'SWITCH' THEN
		    OUT.TEXT = 'Transaccion en cajero AZUL' 
		    END
		    
		    IF ORIGEN EQ 'ATH' THEN
		    OUT.TEXT = 'Transaccion en Cajeros de terceros nacionales' 
		    END

        END
        
        IF UBICACION EQ 'INTERNACIONAL' THEN
    	
		    OUT.TEXT = 'Transaccion en Cajeros internacionales' 

        END
        
        
        
    	
    END
        
    IF ORDENBANCO EQ 'Membresia Visa Deb'    THEN
    
	    IF CUENTACREDITO1 EQ CARGOCLASICA THEN
	    OUT.TEXT = 'Membresia tarjeta de débito Clásica' 
	    END
	    
	    IF CUENTACREDITO1 EQ CARGOORO THEN
	    OUT.TEXT = 'Membresia tarjeta de debito Oro' 
	    END
	    
	    IF CUENTACREDITO1 EQ CARGOPLATNO THEN
	    OUT.TEXT = 'Membresia tarjeta de debito Platino' 
	    END
	    
    
    END
    
    IF ORDENBANCO EQ 'Reversion Visa Deb'    THEN
        OUT.TEXT = 'Reintegro de comision por transaccion' 
	    IF ATM EQ CUENTAMEMBRESIA THEN
	    OUT.TEXT = 'Reintegro de comision por membresia' 
	    END 
    
    END
    
    IF ORDENBANCO EQ 'CARGO IVA'    THEN
        OUT.TEXT = 'Cargo por IVA' 
	       
    END
 

    RETURN
*** </region>

*-----------------------------------------------------------------------------

*** <region name= READ.FT>
READ.FT:
*** <desc> </desc>
R.FT = ''
ERR.FT = ''
Y.ERR= ''
 CALL F.READ(FN.FUNDS.TRANSFER,ID.FT,R.FT,F.FUNDS.TRANSFER,ERR.FT)
    IF NOT(R.FT) THEN
        FN.FUNDS.TRANSFER$HIS='F.FUNDS.TRANSFER$HIS'
        F.FUNDS.TRANSFER$HIS=''
        Y.ERR = ''
        CALL OPF(FN.FUNDS.TRANSFER$HIS,F.FUNDS.TRANSFER$HIS)
        CALL EB.READ.HISTORY.REC (F.FUNDS.TRANSFER$HIS,ID.FT,R.FT,Y.ERR)
    END
    
RETURN
*** </region>

END





