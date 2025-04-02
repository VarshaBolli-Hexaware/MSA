CREATE OR REPLACE PACKAGE BODY pkg_mtg_srv_load_wkset AS

 -- Module Name : pkg_mtg_srv_load_wkset
 -- Purpose : The primary purpose of this package is to load the mortgage  update working set tables
 -- Stream : Mortgage
 -- Substream : Servicing
 -- File Name : pkg_mtg_srv_load_wkset.pks
 
  SUBTYPE gt_obj_nme IS VARCHAR2(30);

  mtg_log_ctx pkg_log_util.gt_log_first := pkg_log_util.f_log_start_first(i_bus_fnctn_cd => pkg_fnctn_typ.gl_ln_attr_chg_mts_cramdw,
                                                                  i_logger_nme   => 'pkg_mtg_srv_load_wkset');

   -------------------------------------------------------------------------
   --Module       p_mtg_srv_populate_mtg_srv_tran
   --Purpose       temporary working set table which is created with all the loans
   --             which are not submitted for cancellation from the primary stage table, in addition
   --             it will have few more attributes for further processing.
   --Parameters: i_batch_id,i_run_part_id,i_tx_id,i_pcsg_dt
   -------------------------------------------------------------------------
    PROCEDURE p_mtg_srv_populate_mtg_srv_tran(i_batch_id    IN NUMBER,
                               i_run_part_id IN NUMBER,
                               i_tx_id       IN NUMBER,
                               i_pcsg_dt     IN DATE)
    IS
      l_proc_nme CONSTANT gt_obj_nme :=  'p_mtg_srv_populate_mtg_srv_tran';

      v_int_tot_rec_cnt  pkg_core.gt_cnt := 0;
      v_int_tot_vld_cnt  pkg_core.gt_cnt := 0;
      v_int_tot_cncl_cnt pkg_core.gt_cnt := 0;

      l_mthy_due_1st_pmt_cyl_id CONSTANT pkg_core.gt_id := 1;
      l_mthy_aa_sa_smry_pdcs_id CONSTANT pkg_core.gt_id := 26;

    BEGIN

      INSERT INTO tmp_mtg_srv_tran
        (bch_id,
         tran_id,
         run_ptit_id,
         bnk_ln_id,
         bank_ast_id,
         ln_mtg_srv_update_dt,
         ln_bef_update_amrt_typ_cd,
         ln_aft_update_amrt_typ_cd,
         ln_int_cplzd_amt,
         ln_upb_cplzd_amt,
         ln_mtry_dt,
         ln_upb_frgv_amt,
         ln_int_frgv_amt,
         ln_uapd_fnds_apld_to_int_amt,
         ln_upb_bef_update_amt,
         ln_srvg_fee_rt,
         ln_lndr_ptr,
         ln_excs_yld_rt,
         mtg_tran_id,
         tran_smss_src_cd,
         bnk_sys_sstr_typ_cd,
         mtg_srv_tran_st_cd,
         mtg_srv_tran_st_chg_usr_id,
         ln_multi_step_ind,
         ln_disaster_ind,
         ln_disaster_nme,
         ln_bef_lpi_dt,
         darts_id,
         ln_upb_cplzd_othr_amt,
         ln_srvg_fee_amt,
         selr_srvr_no,
         ln_new_upb_amt,
         tran_typ_cd,
         ln_bwkly_to_mthy_ind,
         ln_new_lpi_dt,
         ln_rimb_prin_amt,
         ln_rimb_lpt_amt,
         ln_mtg_srv_update_cncln_ind,
         ln_wrkot_new_typ_cd,
         ln_prin_frbrn_new_amt,
         ln_stat_cd,
         ln_core_cvrn_dt,
         ln_bank_acvy_archv_dt,
         prop_pur_prc_amt,
         ln_fst_inlm_due_dt,
         ln_bnk_acqrd_pct,
         ln_aqsn_actl_upb_amt,
         ln_int_only_end_dt,
         ln_rmtt_typ_cd,
         ln_update_alwd_ind,
         mtg_srv_chg_typ_dt,
         ln_int_accrl_frqy_mthd_cd,
         ln_orig_upb_amt,
         srvr_lndr_ln_id,
         ln_lpi_at_sir_setup_dt,
         ln_old_srvg_fee_rt,
         ln_wrkot_old_typ_cd,
         ln_prin_frbrn_old_amt,
         -- Loan Common Reference Attributes
         ltst_ln_apld_bank_st_id,
         ltst_ln_apld_pdc_acvy_st_id,
         srvr_prty_id,
         ltst_lpc_tran_seq_no,
         -- Loan feature attributes
         ln_sle_typ_cd,
         ln_fcl_ls_rsk_cd,
         ln_max_term,
         prcs_dt_clctn_id,
         pmt_cyl_id,
         orig_prcd_dt_clctn_id,
         ltst_ln_feat_id,
         ltst_ln_feat_expt_dt,
         ltst_ln_feat_eff_dt,
         ln_rptg_mthd_cd,
         ln_bnk_owns_pct,
         ln_prdc_lbl_typ_cd,
         --loan Applied Financial State Attributes
         ln_days_mtg_srv_cnt,
         ln_upb_amt,
         ln_lpi_dt,
         ln_apld_bank_st_seq_no,
         old_ln_rimb_prin_amt,
         old_ln_rimb_lpt_amt,
         ln_apld_schd_upb_amt,
         ln_rmng_term,
         ln_old_fctrd_upb_amt,
         ln_grs_upb_amt,
         --Process DT Collection
         rptg_frqy_cd,
         --Sequence Values
         mtg_srv_actg_evnt_id,
         ln_apld_bank_st_id,
         lpc_tran_id,
         orig_prcs_dt_clctn_id,
         ln_new_pmt_rt_st_id,
         ln_odd_due_cnvr_ind,
         pr_pmt_cyl_id,
         brwr_mdfd_mthy_exp_amt,
         brwr_mdfd_mthy_incm_amt,
         ln_tran_typ_cd,
         expd_lpi_dt,
         ln_lpt_int_advng_stop_typ_cd,
         ln_pdp_ind,
         ln_pmt_rt_prjtn_ind,
         ln_aqsn_dt,
         ln_old_cum_sfee_frbrn_amt,
         ln_old_cum_gfee_frbrn_amt
         )
        SELECT ldms.bch_id,
               ldms.tran_id,
               ldms.run_ptit_id,
               ldms.bnk_ln_id,
               ldms.bank_ast_id,
               ldms.ln_mtg_srv_update_dt,
               ldms.ln_bef_update_amrt_typ_cd,
               ldms.ln_aft_update_amrt_typ_cd,
               nvl(ldms.ln_int_cplzd_amt, 0),
               nvl(ldms.ln_upb_cplzd_amt, 0),
               ldms.ln_mtry_dt,
               nvl(ldms.ln_upb_frgv_amt, 0),
               nvl(ldms.ln_int_frgv_amt, 0),
               nvl(ldms.ln_uapd_fnds_apld_to_int_amt, 0),
               nvl(ldms.ln_upb_bef_update_amt, 0),
               nvl(ldms.ln_srvg_fee_rt, 0),
               nvl(ldms.ln_lndr_ptr, 0),
               nvl(ldms.ln_excs_yld_rt, 0),
               nvl(ldms.mtg_tran_id, mtg_tran_id_seq.NEXTVAL) mtg_tran_id,
               nvl(ldms.tran_smss_src_cd, pkg_ztran_smss_src.gl_srvr_bch) tran_smss_src_cd,
               pkg_zbnk_sys_sstr_typ.gl_acp bnk_sys_sstr_typ_cd,
               decode(nvl(ldms.mtg_srv_tran_st_cd, pkg_zmtg_srv_tran_st.gl_cpld),
                      pkg_zmtg_srv_tran_st.gl_smtd,
                      pkg_zmtg_srv_tran_st.gl_cpld,
                      nvl(ldms.mtg_srv_tran_st_cd, pkg_zmtg_srv_tran_st.gl_cpld)) mtg_srv_tran_st_cd, --If it null,Default to Completed
               ldms.mtg_srv_tran_st_chg_usr_id,
               upper(ldms.ln_mult_step_ind),
               upper(decode(ldms.tran_typ_cd,
                            pkg_ztran_typ.gl_mtg_srv,
                            nvl(ldms.ln_dsastr_ind, pkg_core.gl_false),
                            pkg_ztran_typ.gl_bnkr_cram_dw,
                            pkg_core.gl_true,
                            pkg_core.gl_true)) ln_disaster_ind,
               ldms.ln_dsastr_nme,
               nvl(ldms.ln_bef_cplzd_lpi_dt, lafs.ln_lpi_dt) ln_bef_lpi_dt,
               ldms.ln_darts_id,
               nvl(ldms.ln_upb_cplzd_amt, 0)- nvl(ldms.ln_int_cplzd_amt, 0) ln_upb_cplzd_othr_amt,
               nvl(ldms.ln_srvg_fee_amt, 0),
               lcr.srvr_prty_id,--selr_srvr_no (73996)
               nvl(ldms.ln_upb_amt, lafs.ln_upb_amt + nvl(l.ln_prin_frbrn_amt,0) + (nvl(ldms.ln_upb_cplzd_amt, 0.0) -
                   nvl(ldms.ln_upb_frgv_amt, 0.0)) - nvl(ldms.ln_prin_frbrn_amt,0)) ln_new_upb_amt,
               ldms.tran_typ_cd,
               (CASE
                 -- Bi_Weekly loans should remain as Bi-weekly as part of the Payment Deferral No Reclass Program.
                 WHEN ldms.ln_pdp_ind = pkg_core.gl_true THEN
                   pkg_core.gl_false
                 WHEN pdc.rptg_frqy_cd = pkg_zfrqy.gl_bwkly THEN
                   NVL(ldms.ln_bwkly_to_mthy_ind, pkg_core.gl_true)
                 ELSE
                   pkg_core.gl_false
                END
               ) ln_bwkly_to_mthy_ind,
               ldms.ln_lpi_dt,
               NVL(ldms.ln_rimb_prin_amt,0),
               NVL(ldms.ln_rimb_lpt_amt,0),
               NVL(ldms.ln_mtg_srv_update_cncln_ind, pkg_core.gl_false),
               ldms.ln_wrkot_typ_cd,
               ldms.ln_prin_frbrn_amt,
               ---loan table attributes
               l.ln_stat_cd,
               l.ln_core_cvrn_dt,
               l.ln_bank_acvy_archv_dt,
               l.prop_pur_prc_amt,
               l.ln_fst_inlm_due_dt,
               nvl(l.ln_bnk_acqrd_pct, 0.0),
               l.ln_aqsn_actl_upb_amt,
               l.ln_int_only_end_dt,
               l.ln_rmtt_typ_cd,
               l.ln_update_alwd_ind,
               l.mtg_srv_chg_typ_dt,
               l.ln_int_accrl_frqy_mthd_cd,
               l.ln_orig_upb_amt,
               l.srvr_lndr_ln_id,
               l.ln_lpi_at_sir_setup_dt,
               l.ln_min_srvg_fee_rt,
               l.ln_wrkot_typ_cd,
               l.ln_prin_frbrn_amt,
               -- Loan Common Reference Attributes
               lcr.ltst_ln_apld_bank_st_id,
               lcr.ltst_ln_apld_pdc_acvy_st_id,
               lcr.srvr_prty_id,
               lcr.ltst_lpc_tran_seq_no,
               -- Loan feature attributes
               lf.ln_sle_typ_cd,
               lf.ln_fcl_ls_rsk_cd,
               l.ln_max_term,
               CASE
                 WHEN pdc.rptg_frqy_cd = pkg_zfrqy.gl_bwkly
                  AND NVL(ldms.ln_bwkly_to_mthy_ind, pkg_core.gl_true) = pkg_core.gl_true
                 THEN l_mthy_aa_sa_smry_pdcs_id -- 26
                 ELSE l.prcs_dt_clctn_id
               END prcs_dt_clctn_id,
               CASE
                 WHEN pdc.rptg_frqy_cd = pkg_zfrqy.gl_bwkly
                  AND NVL(ldms.ln_bwkly_to_mthy_ind, pkg_core.gl_true) = pkg_core.gl_true
                 THEN l_mthy_due_1st_pmt_cyl_id
                 ELSE l.pmt_cyl_id
               END pmt_cyl_id,
               l.prcs_dt_clctn_id orig_prcd_dt_clctn_id,
               lf.ln_feat_id,
               lf.ln_feat_expt_dt,
               lf.ln_feat_eff_dt,
               l.ln_rptg_mthd_cd,
               lf.ln_bnk_owns_pct,
               lf.ln_prdc_lbl_typ_cd,
               --loan Applied Financial State Attributes
               lafs.ln_days_mtg_srv_cnt,
               lafs.ln_upb_amt,
               lafs.ln_lpi_dt,
               lafs.ln_apld_bank_st_seq_no,
               lafs.ln_tot_prin_advd_amt,
               lafs.ln_tot_lpt_int_advd_amt,
               lafs.ln_schd_upb_amt,
               lafs.ln_rmng_term,
               lafs.ln_fctrd_upb_amt,
               (CASE
                 WHEN l.ln_rmtt_typ_cd = pkg_zln_rmtt_typ.GL_S_S
                 THEN nvl(lafs.ln_fctrd_schd_upb_amt, 0) + (nvl(l.ln_prin_frbrn_amt, 0) * nvl(l.ln_bnk_acqrd_pct, 0))
                 ELSE nvl(lafs.ln_fctrd_upb_amt, 0) + (nvl(l.ln_prin_frbrn_amt, 0) * nvl(l.ln_bnk_acqrd_pct, 0))
               END) ln_grs_upb_amt,
               --Process DT Collection
               pdc.rptg_frqy_cd,
               --Sequence.NextVals (MTG_ACNTG,LAFS,LPC_TRAN_ID)
               mtg_srv_actg_evnt_id_seq.NEXTVAL   mtg_srv_actg_evnt_id,
               ln_apld_bank_st_id_seq.NEXTVAL new_ln_apld_bank_st_id,
               lpc_tran_id_seq.NEXTVAL        new_lpc_tran_id,
               l.prcs_dt_clctn_id,
               CASE
                 WHEN ldms.ln_pdp_ind = pkg_core.gl_true
                 THEN NULL
                 ELSE ln_pmt_rt_st_id_seq.NEXTVAL
               END new_pmt_rt_st_id,
               -- Convert Odd Due to Due On 1st
               CASE
                 WHEN ldms.ln_pdp_ind = pkg_core.gl_true
                 THEN pkg_core.gl_false
                 ELSE pkg_core.gl_false
               END ln_odd_due_cnvr_ind ,
               l.pmt_cyl_id pr_pmt_cyl_id,
               ldms.brwr_mdfd_mthy_exp_amt,
               ldms.brwr_mdfd_mthy_incm_amt,
               l.tran_typ_cd,
               NULL expd_lpi_dt,
               lafs.ln_lpt_int_advng_stop_typ_cd,
               -- Will be used throughout delmod for areas impacted by PDP.
               ldms.ln_pdp_ind,
               CASE
                 WHEN ldms.ln_pdp_ind = pkg_core.gl_true
                 THEN pkg_core.gl_false
                 ELSE pkg_core.gl_true
               END ln_pmt_rt_prjtn_ind,
               l.ln_aqsn_dt,
               nvl(l.ln_cum_sfee_frbrn_amt,0) ln_old_cum_sfee_frbrn_amt,
               nvl(l.ln_cum_gfee_frbrn_amt,0) ln_old_cum_gfee_frbrn_amt
          FROM ln_mtg_srv_update_stgg ldms
          JOIN ln_cmn_ref lcr ON (ldms.bank_ast_id = lcr.bank_ast_id)
          JOIN ln_feat lf ON (lcr.ltst_ln_feat_id = lf.ln_feat_id)
          JOIN loan l ON (l.bank_ast_id = ldms.bank_ast_id)
          JOIN ln_apld_bank_st lafs ON (lcr.ltst_ln_apld_bank_st_id =
                                       lafs.ln_apld_bank_st_id AND
                                       lcr.ltst_ln_apld_pdc_acvy_st_id =
                                       lafs.prcs_dt_clctn_st_acvy_id)
          JOIN prcs_dt_clctn pdc ON (pdc.prcs_dt_clctn_id = l.prcs_dt_clctn_id)
         WHERE ldms.run_ptit_id = i_run_part_id
           AND ldms.tran_id = i_tx_id
           AND ldms.bch_id = i_batch_id
           AND nvl(ldms.mtg_srv_tran_st_cd, pkg_zmtg_srv_tran_st.gl_cpld) <> pkg_zmtg_srv_tran_st.gl_cncld;

      v_int_tot_vld_cnt := SQL%ROWCOUNT;

      pkg_log_util.p_fine(mtg_log_ctx, l_proc_nme||' inserted '||v_int_tot_vld_cnt||' rows');

      SELECT COUNT(DISTINCT ldms.bank_ast_id)
        INTO v_int_tot_rec_cnt
        FROM ln_mtg_srv_update_stgg ldms
       WHERE nvl(ldms.mtg_srv_tran_st_cd, pkg_zmtg_srv_tran_st.gl_cpld) <> pkg_zmtg_srv_tran_st.gl_cncld
         AND ldms.run_ptit_id = i_run_part_id
         AND ldms.tran_id = i_tx_id
         AND ldms.bch_id = i_batch_id;

      SELECT COUNT(DISTINCT bank_ast_id)
        INTO v_int_tot_cncl_cnt
        FROM tmp_mtg_srv_cncl_tran;

      -- Check if inpt table count matches, Total Transaction  count + Duplicate Eliminated Records

      IF v_int_tot_rec_cnt <> v_int_tot_vld_cnt + v_int_tot_cncl_cnt THEN
        pkg_mtg_srv_util.p_raise_fallout(io_ctx        => mtg_log_ctx,
                                        i_fallout_tbl => 'tmp_mtg_srv_tran',
                                        i_fallout_col => 'bank_ast_id',
                                        i_base_tbl    => 'ln_mtg_srv_update_stgg',
                                        i_expd_cnt    => v_int_tot_rec_cnt,
                                        i_actl_cnt    => (v_int_tot_vld_cnt + v_int_tot_cncl_cnt));

      END IF;

   END p_mtg_srv_populate_mtg_srv_tran;

END pkg_mtg_srv_load_wkset;
/
