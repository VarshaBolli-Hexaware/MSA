CREATE OR REPLACE PACKAGE BODY pkg_mtg_srv_tx IS

  mtg_logger_ctxt pkg_log_util.gt_log_first := pkg_log_util.f_log_start_first(i_bus_fnctn_cd => pkg_fnctn_typ.gl_ln_attr_chg_mts_cramdw,
                                                              i_logger_nme => 'pkg_mtg_srv_tx');

  gv_tran_typ_cd mtg_srv_tran.tran_typ_cd%TYPE;

  -- Module Name    : pkg_mtg_srv_tx
  -- Purpose        : This Package is the main wrapper for Mortgage Update PL/SQL Component.
  -- Stream : Mortgage
  -- Substream : Servicing
  -- File Name       : pkg_mtg_srv_tx.pkb
  
  -- Module : p_batch_execution_tx
  -- Purpose : This package contains procedures and functions supporting transaction processing stage
  -- of MTG  UPDATE PL/SQL Component, including p_batch_execution_tx() procedure
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
  -------------------------------------------------------------------------------
  PROCEDURE p_batch_execution_tx(i_batch_id    IN NUMBER,
                               i_run_part_id IN NUMBER,
                               i_tx_id       IN NUMBER,
                               i_pcsg_dt     IN DATE)
  AS
    v_trcy_tbl_nme bch_config.PRCS_TRCY_STG_TBL_NME%TYPE;
    v_rec_cnt_ni PLS_INTEGER := 0;
  BEGIN
    pkg_log_util.p_set_tx_step_nme('Mortgage Update PL/SQL Component- Controller');

    -- log the start of the procedure
    pkg_log_util.p_fine(mtg_logger_ctxt,
                       'Procedure p_batch_execution_tx - Has started With Input Parameters:' || ' Batch Id:' || i_batch_id ||
                        ' Run Part Id:' || i_run_part_id || ' Tx id :' || i_tx_id || ' Processing Dt:' ||
                        to_char(i_pcsg_dt, 'MM/DD/YYYY'));
    pkg_performance_util.p_start('pkg_mtg_srv_tx.p_batch_execution_tx');

    -- lock loans which are not submitted for Cancellation only
    p_mtg_srv_loans_locked(i_batch_id => i_batch_id,
                    i_run_part_id => i_run_part_id,
                    i_tx_id => i_tx_id,
                    o_rec_cnt_ni => v_rec_cnt_ni);

    IF v_rec_cnt_ni > 0
    THEN

          -- Validate Mtg Srv Business Rules against all the transaction.
          p_mtg_srv_business_validation(i_pcsg_dt => i_pcsg_dt);
                                  
          --Populate TMP_LN_CASH_TRAN_STGG and invoke LN_CASH_TRAN_SRVC process to populate LN_CASH_TRAN table

          pkg_mtg_srv_update_cash_tran.p_mtg_srv_prfm_cash_tran(i_pcsg_dt);         

          pkg_mtg_srv_cptr_svc.p_execute_mtg_srv_cptr(i_pcsg_dt => i_pcsg_dt);


      -- We should never run into NO_DATA_FOUND here
      -- or we would have failed before getting to this
      -- point but just to be safe we will log and not
      -- fail in case some Unit test cases are calling
      -- without setting up data in batch control table
      BEGIN
        SELECT bc.PRCS_TRCY_STG_TBL_NME
          INTO v_trcy_tbl_nme
          FROM bch_config bc,
             tran_bch_cntl tbc
          WHERE bc.bch_nme = tbc.bch_nme
            AND tbc.bch_id = i_batch_id;
             EXCEPTION
          WHEN NO_DATA_FOUND THEN
          pkg_log_util.p_fine(mtg_logger_ctxt, 'p_batch_execution_tx - Traceability staging table is not defined in bch_config table');
      END;
    END IF;
    -- log end of transaction processing
    pkg_performance_util.p_stop('pkg_mtg_srv_tx.p_batch_execution_tx');
    pkg_log_util.p_fine(mtg_logger_ctxt, 'p_batch_execution_tx - Has Ended');

  END p_batch_execution_tx;
  -------------------------------------------------------------------------
  --Module      p_mtg_srv_loans_locked
  --Purpose     This procedure will lock all the loans retrieved from the staging table identified
  --            by financial asset ids, involved in the given input transaction
  --            (which are not submitted for cancellation into an array).
  -------------------------------------------------------------------------
  PROCEDURE p_mtg_srv_loans_locked(i_batch_id    IN NUMBER,
                            i_run_part_id IN NUMBER,
                            i_tx_id       IN NUMBER,
                            o_rec_cnt_ni  OUT PLS_INTEGER)
  AS
    v_bank_ast_id_nt number_var := number_var();
  BEGIN

    -- Set Log step name
    pkg_log_util.p_set_tx_step_nme(' Mortage Assets');

    -- Log the start of the procedure

    pkg_log_util.p_fine(mtg_logger_ctxt, 'p_mtg_srv_loans_locked - Has started');
    pkg_performance_util.p_start('p_mtg_srv_loans_locked.p_mtg_srv_loans_locked');

    -- select all unique financial asset ids for the specified transaction
    -- that is, loan asset ids and packet asset ids
    SELECT ldms.bank_ast_id BULK COLLECT
      INTO v_bank_ast_id_nt
      FROM ln_mtg_srv_update_stgg ldms
     WHERE ldms.run_ptit_id = i_run_part_id
       AND ldms.tran_id = i_tx_id
       AND ldms.bch_id = i_batch_id;

    o_rec_cnt_ni := v_bank_ast_id_nt.COUNT;

    IF o_rec_cnt_ni > 0 THEN
      -- call the concurrency service
      pkg_logical_lock.p_acquire_bank_ast_locks(i_bank_ast_id_nt => v_bank_ast_id_nt,
                                                i_wait_bhvr_cd => pkg_zwait_bhvr.gl_limited_wait);
    END IF;

    -- Log end of the API call

    pkg_performance_util.p_stop('p_mtg_srv_loans_locked.p_mtg_srv_loans_locked');
    pkg_log_util.p_fine(mtg_logger_ctxt, 'p_mtg_srv_loans_locked - Has Ended');

  END p_mtg_srv_loans_locked;

  ---------------------------------------------------------------------------
  --Module      p_mtg_srv_business_validation
  --Purpose            This procedure is for running MTG SRV UPD business rules against all the SRV UPD Transactions.
  -------------------------------------------------------------------------
  PROCEDURE p_mtg_srv_business_validation(i_pcsg_dt IN DATE) AS
 
   l_mtg_srv_rule_retirement_dt CONSTANT DATE := pkg_bch_func_parm.f_get_bch_parm_date_val(i_bch_funcl_parm_nme => 'DM_RULE_784_568_569_RETIREMENT_DATE');

  BEGIN

    -- Set Log step name
    pkg_log_util.p_set_tx_step_nme('Invoke Mtg Srv Business Rules');

    -- Log the start of the procedure

    pkg_log_util.p_fine(mtg_logger_ctxt, 'p_mtg_srv_business_validation - Has started');
    pkg_performance_util.p_start('pkg_mtg_srv_tx.p_mtg_srv_business_validation');
    
       INSERT ALL
    ----BR_101 BEGIN----MTG STATUS
       WHEN ln_stat_cd <> pkg_zln_stat.gl_actv THEN
       INTO tmp_mtg_srv_rule_rslt
        (mtg_tran_id,
         mtg_srv_rule_id,
         bnk_sys_sstr_typ_cd,
         mtg_srv_rule_rslt_fnl_cd)
       VALUES
        (mtg_tran_id,
         pkg_mtg_srv_const.BR_101,
         bnk_sys_sstr_typ_cd,
         pkg_zmtg_srv_tran_st.gl_vldn_failed)
    ---- BR_101 END------
       ---- BR_102 BEGIN--
       WHEN ln_pdp_ind = pkg_core.gl_false AND 
            ln_new_upb_amt <>
         (ln_upb_amt + nvl(ln_prin_frbrn_old_amt,0.0) - nvl(ln_upb_frgv_amt, 0.0) + nvl(ln_upb_cplzd_amt, 0.0)- ln_prin_frbrn_new_amt) THEN INTO tmp_mtg_srv_rule_rslt
         (mtg_tran_id,
          mtg_srv_rule_id,
          bnk_sys_sstr_typ_cd,
          mtg_srv_rule_rslt_fnl_cd)
       VALUES
         (mtg_tran_id,
          pkg_mtg_srv_const.BR_102,
          bnk_sys_sstr_typ_cd,
          pkg_zmtg_srv_tran_st.gl_vldn_failed)
       ---- BR_102 END-----
       ----  BR_103 BEGIN ------
       WHEN (tran_eff_dt < nvl(mtg_srv_chg_typ_dt,tran_eff_dt) AND
             NOT(ln_mtg_srv_update_cncln_ind = pkg_core.gl_true AND
                 ln_tran_typ_cd =tran_typ_cd))
        THEN INTO tmp_mtg_srv_rule_rslt
         (mtg_tran_id,
          mtg_srv_rule_id,
          bnk_sys_sstr_typ_cd,
          mtg_srv_rule_rslt_fnl_cd)
       VALUES
         (mtg_tran_id,
          pkg_mtg_srv_const.BR_103,
          bnk_sys_sstr_typ_cd,
          pkg_zmtg_srv_tran_st.gl_vldn_failed)
       ---- BR_103 END-----
       ----  BR_104 BEGIN ------
       WHEN (   (    ln_odd_due_cnvr_ind  = pkg_core.gl_true
                 AND TRUNC(ln_lpi_dt,'MM') <> TRUNC(ln_bef_lpi_dt,'MM')
                )
             OR (    ln_odd_due_cnvr_ind  = pkg_core.gl_false
                 AND ln_lpi_dt <> ln_bef_lpi_dt
                )
           )
       THEN INTO tmp_mtg_srv_rule_rslt
         (mtg_tran_id,
          mtg_srv_rule_id,
          bnk_sys_sstr_typ_cd,
          mtg_srv_rule_rslt_fnl_cd)
       VALUES
         (mtg_tran_id,
          pkg_mtg_srv_const.BR_104,
          bnk_sys_sstr_typ_cd,
          pkg_zmtg_srv_tran_st.gl_vldn_failed)
       ---- BR_104 END------
       ----  BR_105 BEGIN ----  
              WHEN ln_pdp_ind = pkg_core.gl_false AND 
                   (ln_prin_frbrn_new_amt < 0
                OR ln_prin_frbrn_new_amt >= ROUND(((ln_upb_amt + nvl(ln_prin_frbrn_old_amt,0) + ln_upb_cplzd_amt) * .50),pkg_core.gl_amt))  THEN
                          INTO tmp_mtg_srv_rule_rslt
                (mtg_tran_id,
                 mtg_srv_rule_id,
                 bnk_sys_sstr_typ_cd,
                 mtg_srv_rule_rslt_fnl_cd)
              VALUES
                (mtg_tran_id,
                 pkg_mtg_srv_const.BR_105,
                 bnk_sys_sstr_typ_cd,
                 pkg_zmtg_srv_tran_st.gl_vldn_failed)
       ---- BR_105 END---------
         SELECT tdt.mtg_tran_id,
                tdt.bnk_sys_sstr_typ_cd,
                tdt.ln_int_rt,
                tdt.tran_eff_dt,
                tdt.ln_extd_term,
                tdt.ln_lndr_ptr,
                tdt.ln_srvg_fee_rt,
                tdt.ln_mtg_srv_update_dt,
                tdt.ln_bank_acvy_archv_dt,
                tdt.ln_core_cvrn_dt,
                tdt.ln_sle_typ_cd,
                tdt.ln_days_mtg_srv_cnt,
                tdt.ln_upb_bef_update_amt,
                tdt.ln_lpi_dt,
                tdt.lpi_dt,
                tdt.ln_upb_amt,
                tdt.ln_aft_update_amrt_typ_cd,
                tdt.ln_stat_cd,
                tdt.ln_int_cplzd_amt,
                tdt.ln_upb_cplzd_amt,
                tdt.ln_excs_yld_rt,
                tdt.mtg_srv_chg_typ_dt,
                tdt.pmt_chg_eff_dt,
                tdt.ln_bef_lpi_dt,
                tdt.ln_bef_update_amrt_typ_cd,
                tdt.ln_upb_frgv_amt,
                tdt.ln_pi_amt,
                tdt.ln_srvg_fee_amt,
                tdt.ln_fcl_ls_rsk_cd,
                tdt.tran_typ_cd,
                tdt.ln_new_upb_amt,
                tdt.rptg_frqy_cd,
                tdt.ln_bwkly_to_mthy_ind,
                tdt.ln_new_lpi_dt,
                tdt.ln_mtg_srv_update_cncln_ind,
                tdt.pmt_rt_st_eff_Dt,
                tdt.ln_addl_term,
                tdt.ln_mtry_dt,
                tdt.ln_prin_frbrn_new_amt,
                tdt.ln_prin_frbrn_old_amt,
                tdt.ln_wrkot_new_typ_cd,
                tdt.ln_bnk_acqrd_pct,
                tdt.ln_prdc_lbl_typ_cd,
                tdt.ln_int_accrl_frqy_mthd_cd,
                tdt.ln_rmtt_typ_cd,
                tdt.failed_bus_rl_rslvd_ind,
                tdt.lar_apld_ind,
                tdt.ln_odd_due_cnvr_ind,
                tdt.ln_min_int_rt,
                tdt.lar_mvmnt_ind,
                tdt.ln_tran_typ_cd,
                tdt.ln_pdp_ind,
                tdt.max_pmt_chg_dt
           FROM tmp_mtg_srv_tran tdt
       WHERE tdt.mtg_srv_tran_st_cd = pkg_zmtg_srv_tran_st.gl_cpld;

    -- Flip failed_bus_rl_rslvd_ind to N for any trans
    -- failed any rule validations
    MERGE INTO tmp_mtg_srv_tran tdt
    USING (SELECT DISTINCT mtg_tran_id
             FROM tmp_mtg_srv_rule_rslt
            WHERE mtg_srv_rule_rslt_fnl_cd = pkg_zmtg_srv_tran_st.gl_vldn_failed) tdarr
    ON (tdt.mtg_tran_id = tdarr.mtg_tran_id)
    WHEN MATCHED THEN
      UPDATE
         SET tdt.mtg_srv_tran_st_cd = pkg_zmtg_srv_tran_st.gl_vldn_failed,
       tdt.failed_bus_rl_rslvd_ind = pkg_core.gl_false;

    -- Flip failed_bus_rl_rslvd_ind to Y for any trans
    -- failed BR_107 ONLY
    MERGE INTO tmp_mtg_srv_tran tdt
    USING (SELECT rlst.mtg_tran_id
       FROM(SELECT COUNT(mtg_srv_rule_id) OVER (PARTITION BY mtg_tran_id) AS rc,
      mtg_tran_id, mtg_srv_rule_id
              FROM tmp_mtg_srv_rule_rslt
            WHERE mtg_srv_rule_rslt_fnl_cd = pkg_zmtg_srv_tran_st.gl_vldn_failed) rlst
     WHERE rc = 1
       AND mtg_srv_rule_id = pkg_mtg_srv_const.BR_107) tdarr
    ON (tdt.mtg_tran_id = tdarr.mtg_tran_id)
    WHEN MATCHED THEN
      UPDATE
         SET tdt.failed_bus_rl_rslvd_ind = pkg_core.gl_true;

    -- Log end of the API call

    pkg_performance_util.p_stop('pkg_mtg_srv_tx.p_mtg_srv_business_validation');
    pkg_log_util.p_fine(mtg_logger_ctxt, 'p_mtg_srv_business_validation - Has Ended');

  END p_mtg_srv_business_validation;


  -------------------------------------------------------------------------
  --Module      p_execute_mtg_srv_ben_notification
  --Purpose     This component is responsible for creating BEN Notification for all the loans processed by Loan update.
  -------------------------------------------------------------------------
  PROCEDURE p_execute_mtg_srv_ben_notification AS

    CURSOR c_notif_cv IS
      SELECT lpc_bus_ntfn_typ(decode(tdt.tran_typ_cd, 26, pkg_zbus_ntfn_typ.getdms, pkg_zbus_ntfn_typ.gl_bnkr_cram_dws),
                              --bus_ntfn_typ_cd (4004)
                              tdt.mtg_tran_id,
                              --mtg_tran_id
                              tdt.LN_NEW_UPB_AMT,--tdt.ln_upb_amt + (nvl(tdt.ln_upb_cplzd_amt, 0.0) - nvl(tdt.ln_upb_frgv_amt, 0.0)),
                              --ln_upb_amt
                              NULL,
                              --ln_mth_since_fcl_cnt
                              tdt.ln_aft_update_amrt_typ_cd,
                              --ln_amrt_typ_cd
                              NULL,
                              --ln_bln_ind
                              tdt.ln_upb_cplzd_amt,
                              --ln_upb_cplzd_amt
                              NULL,
                              --pmt_rt_chg_rsn_desc
                              NULL,--tdt.ln_core_cvrn_dt,
                              --ln_core_cvrn_dt
                              tdt.ln_int_rt,
                              --ln_curr_note_rt
                              tdt.ln_pi_amt,
                              --ln_pi_amt
                              tdt.ln_lndr_ptr,
                              --ln_curr_lndr_ps_thru_int_rt
                              tdt.tran_eff_dt,
                              --lpc_tran_eff_dt
                              tdt.bnk_ln_id,
                              -- bnk_ln_id
                              NULL,
                              -- ln_fcl_eff_dt
                              NULL,
                              --bank_idx_rt
                              NULL,
                              --pmt_chg_last_eff_dt
                              NULL,
                              --cflw_lndr_ps_thru_int_rt
                              tdt.ln_sle_typ_cd,
                              --ln_sle_typ_cd
                              tdt.bank_ast_id,
                              --bank_ast_id
                              tdt.ln_extd_term,
                              --ln_amrt_term
                              tdt.lpc_tran_id, -- LPC_TRAN_ID
                              tdt.lpi_dt,
                              --ln_lpi_dt
                              tdt.ln_mtry_dt,
                              --ln_mtry_dt
                              NULL,
                              --cflw_prjd_lndr_ps_thru_int_rt
                              NULL,
                              --ln_adjd_int_rt
                              NULL,
                              --ln_prjd_pi_pmt_amt
                              NULL,
                              --pkt_bank_ast_id
                              NULL,
                              -- ln_pyof_at_mtry_amt
                              NULL,
                              --ln_pmt_rt_st_eff_dt
                              NULL,
                              --pkt_pool_id
                              NULL,
                              --pkt_typ_cd
                              NULL,
                              --ln_pr_rmtt_typ_cd
                              NULL,
                              --ln_pr_schd_upb_amt
                              NULL,
                              --ln_prdc_lbl_typ_cd
                              NULL,
                              --pmt_rt_chg_rsn_cd
                              NULL,
                              --bus_ntfn_rsn_txt
                              NULL,
                              --ln_rclsn_dt
                              NULL,
                              --ln_rmbrsd_int_amt
                              NULL,
                              --ln_rmbrsd_prin_amt
                              tdt.ln_rmtt_typ_cd,
                              --ln_rmtt_typ_cd
                              tdt.srvr_lndr_ln_id,
                              --srvr_lndr_ln_id
                              tdt.selr_srvr_no,
                              --selr_srvr_no
                              tdt.LN_SRVG_FEE_RT,
                              --cflw_grs_srvg_fee_rt
                              NULL,
                              --ln_data_src_cd
                              tdt.ln_int_cplzd_amt+tdt.ln_int_frgv_amt,
                              --ln_uscurd_int_amt
                              tdt.ln_new_upb_amt-tdt.ln_upb_amt,
                              --uscurd_prin_amt
                              tdt.ln_prin_frbrn_new_amt,
                              --ln_prin_frbrn_amt
                              tdt.ln_wrkot_new_typ_cd
                              --ln_wrkot_typ_cd
                              )
        FROM tmp_mtg_srv_tran tdt
        WHERE tdt.mtg_srv_tran_st_cd = pkg_zmtg_srv_tran_st.gl_cpld
        AND   tdt.ln_mtg_srv_update_cncln_ind = pkg_core.gl_false;

    v_notifications lpc_bus_ntfn_nt_typ;

  BEGIN
    -- Set Log step name
    pkg_log_util.p_set_tx_step_nme('Create MTG SRV Notification');
    pkg_performance_util.p_start('pkg_mtg_srv_tx.p_execute_mtg_srv_ben_notification');
    -- Log the start of the procedure
    pkg_log_util.p_fine(mtg_logger_ctxt, 'p_execute_mtg_srv_ben_notification - Has started');

    OPEN c_notif_cv;
    LOOP
      BEGIN
        FETCH c_notif_cv BULK COLLECT
          INTO v_notifications;

        IF v_notifications.COUNT > 0
        THEN
          IF pkg_log_util.f_is_fine_enabled(io_ctx => mtg_logger_ctxt)
          THEN
            pkg_log_util.p_fine(mtg_logger_ctxt, 'Issuing ' || v_notifications.COUNT || ' srv notifications');
          END IF;

          -- shove them off ...
          pkg_ben.p_create_ntfns(v_notifications);
        ELSE
          EXIT;
        END IF;
      EXCEPTION
        WHEN OTHERS THEN
          -- always close the cursor
          IF pkg_log_util.f_is_warn_enabled(io_ctx => mtg_logger_ctxt)
          THEN
            pkg_log_util.p_warn(mtg_logger_ctxt, 'encountered exception: ' || SQLERRM(SQLCODE));
          END IF;
          CLOSE c_notif_cv;
          RAISE;
      END;

    END LOOP;

    CLOSE c_notif_cv;

    -- Log end of the API call
    pkg_performance_util.p_stop('pkg_mtg_srv_tx.p_execute_mtg_srv_ben_notification');
    pkg_log_util.p_fine(mtg_logger_ctxt, 'p_execute_mtg_srv_ben_notification - Has Ended');

  END p_execute_mtg_srv_ben_notification;


   -------------------------------------------------------------------------------------------
    --Module      f_gen_mtg_srv_tran_actn_id_seq
    --Purpose      Generates sequence values for on temp table..
    -------------------------------------------------------------------------------------------

    FUNCTION f_gen_mtg_srv_tran_actn_id_seq RETURN NUMBER IS
      v_mtg_srv_tran_actn_seq_id NUMBER;
    BEGIN

      SELECT mtg_srv_tran_actn_id_seq.NEXTVAL
        INTO v_mtg_srv_tran_actn_seq_id
        FROM dual;
      RETURN(v_mtg_srv_tran_actn_seq_id);

  END f_gen_mtg_srv_tran_actn_id_seq;

END pkg_mtg_srv_tx;
/
