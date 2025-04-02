CREATE OR REPLACE PACKAGE pkg_mtg_srv_st IS

  -- Module Name : pkg_mtg_srv_st
  -- Purpose : The primary purpose of this package is to Create Intial PMT Rate States and Create Loan applied financial States
  -- Stream : Mortgage
  -- Substream : Servicing
  -- File Name : pkg_mtg_srv_st.pkb
  -------------------------------------------------------------------------
  --Module       p_val_ltst_st
  --Purpose     This component is responsible for all types of loans.
  --            Checks for loans latest applied state and update the working set table with new applied state
  --Parameters:
  -------------------------------------------------------------------------
  PROCEDURE p_val_ltst_st;
 
  -------------------------------------------------------------------------
  --Module      p_invk _cflw_st_calc
  --Purpose     This component is responsible for all Step/Fixed/ARM Type loans.
  --            This component invokes Cash flow state calculator to create new cash flow states.
  --            The Cash Flow State calculator calculates new cash flow states for a loan based on cash flow definitions of the loan for a specific time period.  The time period aligns with a loan payment cycle. The calculator then calculates all cash flow states based on all effective (un-expired) cash flow definitions by the passed in EFF_DT.
  --Parameters: i_batch_id,i_pcsg_dt
  -------------------------------------------------------------------------
  PROCEDURE p_invk_cflw_st_calc;
 

END pkg_mtg_srv_st;
/