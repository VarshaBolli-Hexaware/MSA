CREATE OR REPLACE PACKAGE BODY pkg_mtg_srv_update_cash IS
  
  -- initialize logging context
  mtg_logger_ctxt pkg_log_util.gt_log_first := pkg_log_util.f_log_start_first(i_bus_fnctn_cd => pkg_fnctn_typ.gl_ln_attr_chg_mts_cramdw,
                                                              i_logger_nme   => 'pkg_mtg_srv_update_cash');


  -- Module Name    : pkg_mtg_srv_update_cash
  -- Purpose        : This Package is used to load the tmp_ln_cash_Tran_Stgg table and call the loan cash tran service.
  -- stream      : Mortgage
  -- substream      : Servicing 
  -- File Name       : pkg_mtg_srv_update_cash.pks
     -------------------------------------------------------------------------
  --Module      p_mtg_srv_perform_cash
  --Purpose     Populate staging table with cash flows and invoke cash processing
  --
  --Parameters: i_lpc_tran_pcsg_dt as transaction processing date
  -------------------------------------------------------------------------

  PROCEDURE p_mtg_srv_perform_cash(i_lpc_tran_pcsg_dt IN DATE) AS
  tran_stgg_cnt NUMBER;
  BEGIN
     pkg_log_util.p_fine ( mtg_logger_ctxt, 'p_mtg_srv_perform_cash' );

     pkg_performance_util.p_start('p_mtg_srv_perform_cash');


     SELECT COUNT(1) 
       INTO tran_stgg_cnt
       FROM tmp_ln_cash_tran_stgg;

     pkg_log_util.p_fine (mtg_logger_ctxt, 'No of records inserted in tmp_ln_cash_tran_stgg '||tran_stgg_cnt );
     IF tran_stgg_cnt IS NOT NULL
                      AND tran_stgg_cnt >0 
     THEN

        pkg_log_util.p_fine (mtg_logger_ctxt, 'Invoking Cash Processing API to process cash transactions, no of cash flows are '||tran_stgg_cnt );
        pkg_ln_cash_tran_srvc.p_prfm_ln_cash_tran(i_lpc_tran_pcsg_dt);

     END IF;

     pkg_performance_util.p_stop('p_mtg_srv_perform_cash');


  END p_mtg_srv_perform_cash;
END pkg_mtg_srv_update_cash;
/