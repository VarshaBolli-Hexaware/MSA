CREATE OR REPLACE PACKAGE BODY pkg_mtg_srv_load_bhvr_attr AS

  -- Module Name    : pkg_mtg_srv_load_bhvr_attr
  -- Purpose        : Reads data from working set tables and populates 
  -- stream      : Mortgage
  -- substream      : Servicing
  -- File Name       : pkg_mtg_srv_load_bhvr_attr.pkb


  mtg_log_ctx pkg_log_util.gt_log_first := pkg_log_util.f_log_start_first(i_bus_fnctn_cd => pkg_fnctn_typ.gl_ln_attr_chg_mts_cramdw,
                                                                  i_logger_nme   => 'pkg_mtg_srv_load_bhvr_attr');

  gl_exception_date CONSTANT DATE := to_date('01/19/1863', pkg_bhvr_chg_svc.gl_dt_cvrn_fmt);
  gl_ln_aqsn_cd CONSTANT pkg_core.gt_cd := 5; -- future feature code

  -- Exception thrown in f_add_cycles
  ge_invalid_parameter EXCEPTION;
  gl_invalid_parameter_ex CONSTANT number := pkg_sir_exceptions.gl_invalid_parameter;
  PRAGMA EXCEPTION_INIT (ge_invalid_parameter, -20003);

  ----------------------------------------------------------------------------
  -- Module    : f_get_mtg_srv_attr_nme
  --Purpose    : Get mtg_srv_attr_nme for the given tbl_nme, col_nme 
  -------------------------------------------------------------------------
  FUNCTION f_get_mtg_srv_attr_nme (i_tbl_nme IN VARCHAR2, 
                               i_col_nme IN VARCHAR2,
                               i_mtg_srv_attr_tbl_typ_cd IN NUMBER DEFAULT NULL )
  RETURN VARCHAR2
  IS
    v_mtg_srv_attr_nme mtg_srv_attr_mdta.mtg_srv_attr_nme%TYPE;
  BEGIN 
    SELECT mtg_srv_attr_nme INTO v_mtg_srv_attr_nme
      FROM mtg_srv_attr_mdta
     WHERE tbl_nme = i_tbl_nme
       AND col_nme = i_col_nme 
       AND lgcl_deld_ind = pkg_core.gl_FALSE
       AND NVL(mtg_srv_attr_tbl_typ_cd, -999) = NVL(i_mtg_srv_attr_tbl_typ_cd, -999);
    RETURN v_mtg_srv_attr_nme ;
  END f_get_mtg_srv_attr_nme;

  -------------------------------------------------------------------------------------------
  --Module      p_mtg_srv_populate_loan
  --Purpose     Create new loan behavior attributes, populate table  with all
  --            required attribute names and their values (Refer Mapping Table) and populate
  --            MTG_TRAN_ACTN table with new bank_ast_id and their corresponding new and old
  --            attribute values with action code as UPDATE.
  --Parameter   i_pcsg_dt
  -------------------------------------------------------------------------------------------

  PROCEDURE p_mtg_srv_populate_loan(i_pcsg_dt IN DATE) IS
    
    --Getting the switchover date to start populating SFEE/GFEE reimbursement for pdmods.
    l_lqdn_smplcn_cutover_date CONSTANT DATE := pkg_bch_func_parm.f_get_bch_parm_date_val(i_bch_funcl_parm_nme => 'LQDN_SIMPLIFICATION_CUTOVER_DATE');
    
  BEGIN
  
    -- Set Log step name
    pkg_log_util.p_set_tx_step_nme('Populate mtg_srv_tran_actn and mtg_srv_actn_attr table');
  
    pkg_log_util.p_fine(mtg_log_ctx,
                       'Executing procedure pkg_mtg_srv_load_bhvr_attr.p_mtg_srv_populate_loan');
  
    pkg_performance_util.p_start('p_mtg_srv_populate_loan');
  
    -- Populates  table  with all required attribute names and their values and
    -- populates  table with new and their corresponding new and old
    -- attribute values with action code as UPDATE.
  
    INSERT ALL 
    WHEN 1 = 1 THEN INTO tmp_bhvr_svc_actn_attr
      (mtg_srv_actn_attr_id,
       tbl_nme,
       col_nme,
       mtg_srv_actn_attr_pcsg_dt,
       rptd_vld_attr_new_val,
       mtg_srv_attr_nme,
       mtg_srv_tran_actn_typ_cd,
       obj_id,
       mtg_tran_id,
       mtg_srv_tran_actn_dt,
       mtg_srv_tran_actn_id)
    VALUES
      (pkg_bcs_util.f_get_mtg_srv_actn_attr_id_seq,
       'MTG',
       'MTG_CHG_TYP_DT',
       i_pcsg_dt,
       CASE
        WHEN (lar_apld_ind=pkg_core.gl_true AND lar_mvmnt_ind=pkg_core.gl_true) --In case of LPI and upb movement due to current period lar
        THEN to_char(pmt_acvy_nxt_per_strt_dt,pkg_bhvr_chg_svc.gl_dt_cvrn_fmt)  -- Next period payment activity start date
        ELSE to_char(mtg_srv_chg_typ_dt, pkg_bhvr_chg_svc.gl_dt_cvrn_fmt) -- tran_eff_dt from tmp_mtg_srv_tran
       END,        
       'MTG.MTG_CHG_TYP_DT',
       pkg_zmtg_srv_tran_actn_typ.gl_mtg_srv_upd_actn,
       bank_ast_id,
       mtg_tran_id,
       mtg_srv_chg_typ_dt,
       mtg_srv_tran_actn_id) 
    WHEN ((tran_typ_cd = pkg_ztran_typ.gl_mtg_srv AND ln_aft_update_amrt_typ_cd IN (pkg_zln_amrt_typ.GL_FRM_FRM_,
                                     pkg_zln_amrt_typ.GL_STEP_RT)) OR
         (tran_typ_cd = pkg_ztran_typ.gl_bnkr_cram_dw AND ln_aft_update_amrt_typ_cd IN (pkg_zln_amrt_typ.GL_FRM_FRM_)))    
       AND  ln_int_only_end_dt >= ln_new_lpi_dt
       AND  ln_mtg_srv_update_cncln_ind = pkg_core.gl_false
    THEN
    INTO tmp_bhvr_svc_actn_attr
      (mtg_srv_actn_attr_id,
       tbl_nme,
       col_nme,
       mtg_srv_actn_attr_pcsg_dt,
       rptd_vld_attr_new_val,
       mtg_srv_attr_nme,
       mtg_srv_tran_actn_typ_cd,
       obj_id,
       mtg_tran_id,
       mtg_srv_tran_actn_dt,
       mtg_srv_tran_actn_id)
    VALUES
      (pkg_bcs_util.f_get_mtg_srv_actn_attr_id_seq,
       'MTG',
       'LN_INT_ONLY_END_DT',
       i_pcsg_dt,
       to_char(ln_bef_lpi_dt, pkg_bhvr_chg_svc.gl_dt_cvrn_fmt),
       'Loan Interest Only End Date',
       pkg_zmtg_srv_tran_actn_typ.gl_mtg_srv_upd_actn,
       bank_ast_id,
       mtg_tran_id,
       mtg_srv_chg_typ_dt,
       mtg_srv_tran_actn_id)    
          WHEN ln_pdp_ind = pkg_core.gl_true
           AND i_pcsg_dt >= l_lqdn_smplcn_cutover_date THEN
           INTO tmp_bhvr_svc_actn_attr
             (mtg_srv_actn_attr_id,
              tbl_nme,
              col_nme,
              mtg_srv_actn_attr_pcsg_dt,
              rptd_vld_attr_new_val,
              mtg_srv_attr_nme,
              mtg_srv_tran_actn_typ_cd,
              obj_id,
              mtg_tran_id,
              mtg_srv_tran_actn_dt,
              mtg_srv_tran_actn_id)
           VALUES
             (pkg_bcs_util.f_get_mtg_srv_actn_attr_id_seq,
              'MTG',
              'LN_CUM_GFEE_FRBRN_AMT',
              i_pcsg_dt,
              ln_cum_gfee_frbrn_amt,
              'Loan Cumulative Guarantee Fee Forbearance Amount',
              pkg_zmtg_srv_tran_actn_typ.gl_mtg_srv_upd_actn,
              bank_ast_id,
              mtg_tran_id,
              mtg_srv_chg_typ_dt,
              mtg_srv_tran_actn_id)
      SELECT tdt.bank_ast_id,
              tdt.mtg_tran_id,
              tdt.tran_eff_dt mtg_srv_chg_typ_dt,
              tdt.ln_upb_cplzd_amt ln_aqsn_actl_upb_amt,
              tdt.LN_NEW_UPB_AMT ln_wrkot_upb_amt,
              tdt.lpi_dt ln_wrkot_lpi_dt,
              tdt.rptg_frqy_cd,
              tdt.ln_max_term,
              tdt.ln_extd_term,
              tdt.prcs_dt_clctn_id,
              tdt.pmt_cyl_id,
              (CASE
                WHEN ln_bwkly_to_mthy_ind = pkg_core.gl_true THEN
                 pkg_zfrqy.gl_mthy
                ELSE
                 tdt.ln_rptg_mthd_cd
              END) ln_rptg_mthd_cd,
              tdt.ln_srvg_fee_rt,
              tdt.ln_old_srvg_fee_rt,
              tdt.tran_typ_cd,
              tdt.pmt_rt_st_eff_dt,
              tdt.ln_core_cvrn_dt,
              null mtg_srv_tran_actn_id,
              tdt.ln_bwkly_to_mthy_ind,
              tdt.ln_int_only_end_dt,
              tdt.ln_new_lpi_dt,
              tdt.ln_aft_update_amrt_typ_cd,
              tdt.ln_bef_lpi_dt,
              tdt.ln_mtg_srv_update_cncln_ind,
              tdt.ln_disaster_ind,
              tdt.ln_wrkot_new_typ_cd,
              tdt.ln_prin_frbrn_new_amt,
              tdt.ln_fst_inlm_due_dt,
              tdt.ln_odd_due_cnvr_ind,
              tdt.brwr_mdfd_mthy_exp_amt,
              tdt.brwr_mdfd_mthy_incm_amt,
              tdt.ln_update_typ_cd,
              tdt.lar_apld_ind,
              tdt.lar_mvmnt_ind,
              tdt.pmt_acvy_nxt_per_strt_dt,
              tdt.ln_pdp_ind,
              tdt.ln_new_cum_sfee_frbrn_amt ln_cum_sfee_frbrn_amt,
              tdt.ln_new_cum_gfee_frbrn_amt ln_cum_gfee_frbrn_amt,
              tdt.ln_old_cum_sfee_frbrn_amt,
              tdt.ln_old_cum_gfee_frbrn_amt
         FROM tmp_mtg_srv_tran tdt
        WHERE tdt.mtg_srv_tran_st_cd = pkg_zmtg_srv_tran_st.gl_cpld;

    pkg_performance_util.p_stop('p_mtg_srv_populate_loan');
  
    pkg_log_util.p_fine(mtg_log_ctx,
                       'Completed procedure pkg_mtg_srv_load_bhvr_attr.p_mtg_srv_populate_loan');
  
  END p_mtg_srv_populate_loan;

  FUNCTION f_add_cycles(i_starting_date   IN DATE,
                        i_num_periods     IN PLS_INTEGER,
                        i_pmt_cyl_id      IN pmt_cyl.pmt_cyl_id%TYPE,
                        i_pmt_cyl_frqy_cd IN pmt_cyl.pmt_cyl_frqy_cd%TYPE) RETURN DATE DETERMINISTIC IS

    v_first_day_of_end_month DATE;
    v_pcs_start_date         DATE;
    v_result                 DATE;
  BEGIN

    IF i_pmt_cyl_frqy_cd = pkg_zfrqy.gl_mthy
    THEN
      -- Find the start date of the PCS this date is in
      SELECT pmt_cyl_strt_dt
        INTO v_pcs_start_date
        FROM pmt_cyl_st
       WHERE pmt_cyl_id = i_pmt_cyl_id
         AND i_starting_date BETWEEN pmt_cyl_strt_dt AND pmt_cyl_end_dt;

      -- Go to the first of the month, and add/subtract the number of periods as months
      v_first_day_of_end_month := add_months(trunc(v_pcs_start_date, 'MONTH'), i_num_periods);

      -- Find the PCS start date that falls in this month
      SELECT pmt_cyl_strt_dt
        INTO v_result
        FROM pmt_cyl_st
       WHERE pmt_cyl_id = i_pmt_cyl_id
         AND pmt_cyl_strt_dt BETWEEN v_first_day_of_end_month AND last_day(v_first_day_of_end_month);

    ELSIF i_pmt_cyl_frqy_cd = pkg_zfrqy.gl_bwkly
    THEN
      v_result := i_starting_date + (14 * i_num_periods);
    ELSE
      RAISE ge_invalid_parameter;
    END IF;

    RETURN v_result;

  END f_add_cycles;

END pkg_mtg_srv_load_bhvr_attr;
/