CREATE OR REPLACE PACKAGE BODY pkg_mtg_srv_st AS

  -- Module Name : pkg_mtg_srv_st
  -- Purpose : The primary purpose of this package is to Create Intial PMT Rate States and Create Loan applied financial States
  -- Stream : Mortgage
  -- Substream : Servicing
  -- File Name : pkg_mtg_srv_st.pks
  

  SUBTYPE gt_obj_nme IS VARCHAR2(30);

  gv_pkg_nme gt_obj_nme := 'pkg_mtg_srv_st';
  mtg_logger_ctxt pkg_log_util.gt_log_ctx := pkg_log_util.f_init_log_ctx(i_bus_fnctn_cd => pkg_zbus_fnctn_typ.gl_ln_attr_chg_dms_cramdw,
                                                              i_logger_nme   => gv_pkg_nme);

  -------------------------------------------------------------------------
  --Module      p_val_ltst_st
  --Purpose     Validate Latest loan applied financial state after reversals
  --            set old advanced amounts, and set the advanced indicator
  --            now that the cash flow defs have been loaded 
  --
  --Parameters  None
  -------------------------------------------------------------------------
  PROCEDURE p_val_ltst_st AS
        v_row_cnt_ni PLS_INTEGER := 0;
  BEGIN
  
    -- Set Log step name
    pkg_log_util.p_set_tx_step_nme('Validate Latest loan applied financial state after reversals');
  
    -- Log the start of the procedure
  
    pkg_log_util.p_fine(mtg_logger_ctxt, 'p_val_ltst_st - Has started');
    pkg_performance_util.p_start('pkg_mtg_srv_st.p_val_ltst_st');
  
    MERGE INTO tmp_mtg_srv_tran tdt
    USING (SELECT lcr.ltst_ln_apld_bank_st_id,
                  tdt.bank_ast_id,
                  lcr.ltst_ln_apld_pdc_acvy_st_id,
                  lcr.ltst_lpc_tran_seq_no,
                  lafs.ln_fctrd_upb_amt - lafs.ln_fctrd_schd_upb_amt - nvl(lafs.ln_tot_stop_adv_prin_amt,0) ln_tot_prin_advd_amt,
                  lafs.ln_tot_lpt_int_advd_amt,
                  lcr.ltst_ln_pmt_advng_stat_id,
                  tdfacd.cflw_adv_per,
                  tdfacd.cflw_adv_typ_cd,
                  (CASE WHEN tdt.ln_rmtt_typ_cd = pkg_zln_rmtt_typ.GL_S_A
                        THEN pkg_lar_mortgage .f_calc_cflw_adv_stop_typ(
                                       tdfacd.cflw_adv_typ_cd
                                      ,tdfacd.cflw_adv_per
                                      ,tdfacd.cflw_rcvy_cd
                                      ,tdfacd.cflw_rcvy_per
                                      ,tdt.ln_per_mtg_srv_cnt
                                      ,tdt.ln_rmtt_typ_cd) 
                        ELSE lafs.ln_lpt_int_advng_stop_typ_cd
                   END) ln_lpt_int_advng_stop_typ_cd
             FROM tmp_mtg_srv_tran tdt
             JOIN ln_cmn_ref lcr ON (tdt.bank_ast_id = lcr.bank_ast_id)
             JOIN ln_apld_bank_st lafs 
               ON (lafs.ln_apld_bank_st_id = lcr.ltst_ln_apld_bank_st_id
              AND  lafs.prcs_dt_clctn_st_acvy_id = lcr.ltst_ln_apld_pdc_acvy_st_id)
             JOIN tmp_mtg_srv_bank_ast_cflw_def tdfacd
               ON tdt.bank_ast_id = tdfacd.bank_ast_id
              AND tdfacd.bank_ast_cflw_def_expt_dt IS NULL
              AND tdfacd.cflw_typ_cd IN (pkg_zcflw_typ.gl_lpt_int)
            WHERE tdt.acp_tran_st_cd = pkg_zacp_tran_st.gl_cpld  ) src
     ON (tdt.bank_ast_id = src.bank_ast_id )
    WHEN MATCHED THEN
      UPDATE
         SET tdt.ltst_ln_apld_bank_st_id      = src.ltst_ln_apld_bank_st_id,
             tdt.ltst_ln_apld_pdc_acvy_st_id  = src.ltst_ln_apld_pdc_acvy_st_id,
             tdt.ltst_lpc_tran_seq_no         = src.ltst_lpc_tran_seq_no,
             tdt.old_ln_rimb_prin_amt         = src.ln_tot_prin_advd_amt,
             tdt.old_ln_rimb_lpt_amt          = src.ln_tot_lpt_int_advd_amt,
             tdt.ln_lpt_int_advng_stop_typ_cd = src.ln_lpt_int_advng_stop_typ_cd,
             tdt.pr_ln_apld_bank_st_id        = src.ltst_ln_apld_bank_st_id,
             tdt.ltst_ln_pmt_advng_stat_id    = src.ltst_ln_pmt_advng_stat_id,
             tdt.cflw_adv_per                 = src.cflw_adv_per,
             tdt.cflw_adv_typ_cd              = src.cflw_adv_typ_cd;

    v_row_cnt_ni := SQL%ROWCOUNT;

    -- Log end of the API call
  
    pkg_log_util.p_fine(mtg_logger_ctxt, 'p_val_ltst_st - Has Ended');
    pkg_performance_util.p_stop_timer('pkg_mtg_srv_st.p_val_ltst_st');
  
  END p_val_ltst_st;

  -------------------------------------------------------------------------
  --Module      p_invk _cflw_st_calc
  --Purpose     This component is responsible for all Step/Fixed/ARM Type loans.
  --            This component invokes Cash flow state calculator to create new cash flow states.
  --            The Cash Flow State calculator calculates new cash flow states for a loan based on cash flow definitions of the loan for a specific time period.  The time period aligns with a loan payment cycle. The calculator then calculates all cash flow states based on all effective (un-expired) cash flow definitions by the passed in EFF_DT.
  --Parameters: i_batch_id,i_pcsg_dt
  -------------------------------------------------------------------------
  PROCEDURE p_invk_cflw_st_calc AS
  BEGIN
    -- Set Log step name
    pkg_log_util.p_set_tx_step_nme('Lock Financial Assets');
  
    -- Log the start of the procedure
  
    pkg_log_util.p_fine(mtg_logger_ctxt, 'p_invk_cflw_st_calc - Has started');
    pkg_performance_util.p_start('pkg_mtg_srv_st.p_invk_cflw_st_calc');
  
    DELETE FROM tmp_cfst_calc_inpt;
    
    INSERT INTO tmp_cfst_calc_inpt
      (bank_ast_id,
       eff_dt,
       lptr,
       gsf,
       cflw_rt_prjd_ind)
      SELECT tdt.bank_ast_id,
             tdt.cflw_eff_dt,
             tdt.ln_lndr_ptr,
             tdt.ln_srvg_fee_rt + tdt.ln_excs_yld_rt gsf,
             NULL cflw_rt_prjd_ind
        FROM tmp_mtg_srv_tran tdt
       WHERE tdt.acp_tran_st_cd = pkg_zacp_tran_st.gl_cpld
         AND (   tdt.ln_aft_update_amrt_typ_cd IN (pkg_zln_amrt_typ.gl_step_rt,
                                                 pkg_zln_amrt_typ.gl_frm_frm_)
              OR (    tdt.tran_typ_cd = pkg_ztran_typ.gl_bnkr_cram_dw
                  AND tdt.ln_aft_update_amrt_typ_cd = pkg_zln_amrt_typ.gl_arm_arm_
                  AND tdt.ln_lndr_ptr = 0
                  AND tdt.ln_srvg_fee_rt = 0
                  AND tdt.ln_excs_yld_rt = 0
                 )
             )
         AND tdt.ln_pmt_rt_prjtn_ind = pkg_core.gl_true
    ;

    pkg_ln_calc_cfst.p_calc_cfst(4);
  
    -- Log end of the API call
  
    pkg_log_util.p_fine(mtg_logger_ctxt, 'p_invk_cflw_st_calc - Has Ended');
    pkg_performance_util.p_stop_timer('pkg_mtg_srv_st.p_invk_cflw_st_calc');
  END p_invk_cflw_st_calc;
  

END pkg_mtg_srv_st;
/