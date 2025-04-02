CREATE OR REPLACE PACKAGE BODY pkg_mtg_srv_upload_update_tx IS

  -- initialize logging context
  mtg_logger_ctxt pkg_log_util.gt_log_first := pkg_log_util.f_log_start_first(i_bus_fnctn_cd => pkg_fnctn_typ.gl_ln_attr_chg_mts_cramdw,
                                                              i_logger_nme   => 'pkg_mtg_srv_upload_update_tx');

  gv_int_rec_ext pkg_core.gt_no := 0;
  gv_tran_typ_cd mtg_srv_tran.tran_typ_cd%TYPE;

  -- Module Name    : pkg_mtg_srv_upload_update_tx
  -- Purpose        : This Package is the main wrapper for File Mortgage  Loan Update upload PL/SQL Component.
  -- stream      : Mortgage
  -- substream      : Servicing 
  -- File Name       : pkg_mtg_srv_upload_update_tx.pks
  --
  -----------------------------------------------------------------------------------------------
  -- Module : p_batch_execution_tx
  -- Purpose : This package contains procedures and functions supporting transaction processing stage
  -- of Mortgage  Loan Update PL/SQL Component, including p_batch_execution_tx() procedure
  -- mandated by the Batch Controller.

  -- Parameters
  -- i_batch_id : Unique identifier for this run of the batch.
  -- i_run_part_id : The unique identifier of a run partition within the staging
  -- table. This id will be used to retrieve the proper staging
  -- data, this key is required so that the query plan can
  -- effectively partition prune to quickly extract staged data
  -- i_tx_id : The unique identifier for this transaction set within the
  -- given batch_id
  -- i_pcsg_dt : The effective processing date for the batch.
  -- Exceptions : Any exceptions thrown by called services will be allowed to propagate to the
  -- Unrecoverable exception may be thrown if data corruption is encountered.
  -------------------------------------------------------------------------------
  PROCEDURE p_batch_execution_tx(i_batch_id    IN NUMBER,
                               i_run_part_id IN NUMBER,
                               i_tx_id       IN NUMBER,
                               i_pcsg_dt     IN DATE) AS

  BEGIN

    pkg_log_util.p_set_tx_step_nme('Mortgage  Loan Update Auto Cancellation PL/SQL Component- Controller');

    -- log the start of the procedure
    pkg_log_util.p_fine(mtg_logger_ctxt,
                       'Procedure p_batch_execution_tx - Has started With Input Parameters:' ||
                       ' Batch Id: ' || i_batch_id || ' Run Part Id:' ||
                       i_run_part_id || ' Tx id :' || i_tx_id ||
                       ' Processing Dt:' ||
                       to_char(i_pcsg_dt, 'MM/DD/YYYY'));

    pkg_mtg_srv_update_upld_wkset.p_mtg_srv_populate_mtg_srv_tran(i_batch_id    => i_batch_id,
                                              i_run_part_id => i_run_part_id,
                                              i_tx_id       => i_tx_id,
                                              i_pcsg_dt     => i_pcsg_dt);

    -- Validate MTG UPDATE Business Rules against all the transaction.
    p_mtg_srv_business_validation(i_pcsg_dt => i_pcsg_dt);

    -- populate all MTG Hierarchy Tables and Cash flow temporary table (Cash flows reqq for Mtg Srv)
    p_mtg_srv_populate_mtg_srv_tran(i_batch_id => i_batch_id, i_pcsg_dt => i_pcsg_dt);

    pkg_mtg_srv_update_cash_tran.p_mtg_srv_prfm_cash_tran(i_pcsg_dt);

    -- Populate TMP_SRVG_EVNT_INPT GTT from TMP_DM_TRAN and Invokes pkg_srvg_evnt_cptr_svc.p_cptr_srvg_evnt
    pkg_mtg_srv_cptr_svc.p_execute_mtg_srv_cptr(i_pcsg_dt => i_pcsg_dt);

    --pkg_performance_util.stop('pkg_mtg_srv_upload_update_tx.p_batch_execution_tx');
    pkg_log_util.p_fine(mtg_logger_ctxt, 'p_batch_execution_tx - Has Ended');

  END;

 ---------------------------------------------------------------------------
  --Module      p_mtg_srv_business_validation
  --Purpose     This procedure is for running MTG SRV UPD business rules
  --            against all the Srv Upd Transactions.
  -------------------------------------------------------------------------
  PROCEDURE p_mtg_srv_business_validation(i_pcsg_dt IN DATE) AS

  BEGIN

    -- Set Log step name
    pkg_log_util.p_set_tx_step_nme('Invoke MTG SRV Business Rules');

    -- Log the start of the procedure

    pkg_log_util.p_fine(mtg_logger_ctxt, 'p_mtg_srv_business_validation - Has started');
    --pkg_performance_util.start('pkg_mtg_srv_upload_update_tx.p_mtg_srv_business_validation');

    INSERT ALL
    WHEN     mtg_srv_tran_st_cd = pkg_zmtg_srv_tran_st.GL_VLDN_FAILED
         AND mtg_srv_rule_id = pkg_mtg_srv_const.br_106 THEN
      INTO tmp_mtg_srv_rule_rslt
        (mtg_tran_id,
         mtg_srv_rule_id,
         bnk_sys_sstr_typ_cd,
         mtg_srv_rule_rslt_fnl_cd
       )
      VALUES
        (mtg_tran_id,
         pkg_mtg_srv_const.BR_106,
         bnk_sys_sstr_typ_cd,
         pkg_zmtg_srv_tran_st.gl_vldn_failed
       )

    SELECT tdt.mtg_tran_id,
           pkg_zbnk_sys_sstr_typ.GL_ACP bnk_sys_sstr_typ_cd,
           tdmus.cancl_dt,
           tdmus.ln_stat_cd,
           tdmus.mtg_srv_tran_orig_stat_cd,
           tdmus.ltst_darts_id,
           tdmus.ln_darts_id,
           tdmus.mtg_srv_tran_st_cd,
           tdmus.mtg_srv_rule_id,
           tdmus.ltst_update_cncln_ind
      FROM tmp_mtg_srv_update_upld_stgg tdmus
      JOIN loan ln ON ln.bank_ast_id = tdmus.bank_ast_id
      LEFT OUTER JOIN tmp_mtg_srv_tran tdt ON (tdt.bank_ast_id = tdmus.bank_ast_id);

    MERGE INTO tmp_mtg_srv_tran tdt
    USING (SELECT DISTINCT mtg_tran_id
             FROM tmp_mtg_srv_rule_rslt
            WHERE mtg_srv_rule_rslt_fnl_cd = pkg_zmtg_srv_tran_st.gl_vldn_failed) tdarr
    ON (tdt.mtg_tran_id = tdarr.mtg_tran_id)
    WHEN MATCHED THEN
      UPDATE SET tdt.mtg_srv_tran_st_cd = pkg_zmtg_srv_tran_st.gl_vldn_failed;


    -- Log end of the API call
    --pkg_performance_util.stop('pkg_mtg_srv_upload_update_tx.p_mtg_srv_business_validation');
    pkg_log_util.p_fine(mtg_logger_ctxt, 'p_mtg_srv_business_validation - Has Ended');

  END p_mtg_srv_business_validation;


  -------------------------------------------------------------------------------------------
  --Module p_mtg_srv_populate_mtg_srv_tran
  --Purpose Inserts data into table from tmp_mtg_srv_tran or updates the changes in
  -- completed).
  --Parameter i_batch_id, i_pcsg_dt
  -------------------------------------------------------------------------------------------

  PROCEDURE p_mtg_srv_populate_mtg_srv_tran(i_batch_id IN NUMBER, i_pcsg_dt IN DATE) IS
   v_mtg_srv_tran_rec_cnt pkg_core.gt_cnt := 0;
   v_mtg_srv_tran_pcsg_dttm DATE;
   
  BEGIN

    -- Set Log step name
    pkg_log_util.p_set_tx_step_nme('Populate mtg_srv_tran table');

    pkg_log_util.p_fine(mtg_logger_ctxt,
                       'Executing procedure pkg_mtg_srv_upload_update_tx.p_mtg_srv_populate_mtg_srv_tran');

    --pkg_performance_util.start('p_mtg_srv_populate_mtg_srv_tran');
    --21 Define and populate new column MTG_TRAN_PCSG_DTTM in MTG_TRAN table.
    BEGIN
       SELECT artf_dt
       INTO v_mtg_srv_tran_pcsg_dttm
       FROM artf_dt;
    EXCEPTION
     WHEN no_data_found THEN
         v_mtg_srv_tran_pcsg_dttm := pkg_mtg_srv_util.gv_upd_dt;
    END;
    
    MERGE INTO mtg_srv_tran at
    USING (SELECT tdt.mtg_tran_id,
                  tdt.bank_ast_id,
                  tdt.tran_eff_dt,
                  NULL mtg_srv_tran_cmnt,
                  tdt.tran_typ_cd,
                  tdt.mtg_srv_tran_grp_id,
                  pkg_core.gl_false tran_wkfl_usr_actn_reqd_ind,
                  tdt.srvr_prty_id srvr_prty_id,
                  tdt.bch_id,
                  tdt.mtg_srv_tran_st_cd,
                  tdt.bnk_sys_sstr_typ_cd,
                  tdt.tran_smss_src_cd,
                  tdt.mtg_srv_tran_orig_st_cd,
                  tdt.mtg_srv_tran_orig_id
             FROM tmp_mtg_srv_tran tdt) pop_tran
    ON (at.mtg_tran_id = pop_tran.mtg_tran_id)
    WHEN MATCHED THEN
      UPDATE
         SET at.bank_ast_id                = pop_tran.bank_ast_id,
             at.tran_eff_dt                = pop_tran.tran_eff_dt,
             at.tran_typ_cd                = pop_tran.tran_typ_cd,
             at.mtg_srv_tran_frcd_lgts_off_ind = pop_tran.tran_wkfl_usr_actn_reqd_ind,
             at.tran_prcds_prty_id         = pop_tran.srvr_prty_id,
             at.bch_id                     = i_batch_id,
             at.mtg_srv_tran_st_cd             = pop_tran.mtg_srv_tran_st_cd,
             at.last_upd_dt                = pkg_mtg_srv_util.gv_upd_dt,
             at.pcsg_dt                    = i_pcsg_dt,
             at.mtg_srv_tran_orig_st_cd        = pop_tran.mtg_srv_tran_orig_st_cd,
             at.mtg_srv_tran_orig_id           = pop_tran.mtg_srv_tran_orig_id,
             at.mtg_srv_tran_grp_id            = pop_tran.mtg_srv_tran_grp_id,
             at.mtg_srv_tran_pcsg_dttm         = v_mtg_srv_tran_pcsg_dttm
    WHEN NOT MATCHED THEN
      INSERT
        (mtg_tran_id,
         bank_ast_id,
         tran_eff_dt,
         tran_typ_cd,
         mtg_srv_tran_grp_id,
         mtg_srv_tran_frcd_lgts_off_ind,
         tran_prcds_prty_id,
         bch_id,
         mtg_srv_tran_st_cd,
         tran_smss_src_cd,
         pcsg_dt,
         mtg_srv_tran_orig_st_cd,
         mtg_srv_tran_orig_id,
         cren_dt,
         last_upd_dt,
         mtg_srv_tran_pcsg_dttm)
      VALUES
        (pop_tran.mtg_tran_id,
         pop_tran.bank_ast_id,
         pop_tran.tran_eff_dt,
         pop_tran.tran_typ_cd,
         pop_tran.mtg_srv_tran_grp_id,
         pop_tran.tran_wkfl_usr_actn_reqd_ind,
         pop_tran.srvr_prty_id,
         i_batch_id,
         pop_tran.mtg_srv_tran_st_cd,
         pop_tran.tran_smss_src_cd,
         i_pcsg_dt,
         pop_tran.mtg_srv_tran_orig_st_cd,
         pop_tran.mtg_srv_tran_orig_id,
         pkg_mtg_srv_util.gv_upd_dt,
         pkg_mtg_srv_util.gv_upd_dt,
         v_mtg_srv_tran_pcsg_dttm);

     v_mtg_srv_tran_rec_cnt := v_mtg_srv_tran_rec_cnt + SQL%ROWCOUNT;
     pkg_log_util.p_fine ( mtg_logger_ctxt, 'Records inserted into mtg_srv_tran '||v_mtg_srv_tran_rec_cnt );
    
    pkg_log_util.p_fine(mtg_logger_ctxt,
                       'Completed procedure pkg_mtg_srv_upload_update_tx.p_mtg_srv_populate_mtg_srv_tran');

  END p_mtg_srv_populate_mtg_srv_tran;

END pkg_mtg_srv_upload_update_tx;
/
