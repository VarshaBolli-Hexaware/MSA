CREATE OR REPLACE PACKAGE BODY pkg_mtg_srv_update_rcycl_stgg
 AS
  ------------------------------------------------------------------------------

  -- Module Name: pkg_mtg_srv_update_rcycl_stgg
  -- Purpose:     This package serves as the entry-point for the Del Mod Recycle Batch.
  -- Stream : Mortgage
  -- Substream : Servicing
  -- File Name:    pkg_mtg_srv_update_rcycl_stgg.pkb
  -- initialize logging context
  mtg_log_ctx pkg_log_util.gt_log_first := pkg_log_util.f_log_start_first(i_bus_fnctn_cd => pkg_fnctn_typ.GL_LN_ATTR_CHG_MTS_CRAMDW,
                                                                  i_logger_nme   => 'pkg_mtg_srv_update_rcycl_stgg');

  ------------------------------------------------------------------------------
  -- Module:      p_load_stgg
  -- Purpose:     Public procedure that identifies all loans to be validated and recycled
  -- Parameters:  i_batch_name        The name of the batch
  --      i_batch_id         The unique identifier for this run of your application.
  --      i_run_part_id      The unique identifier of a run partition within the staging tables.
  --      i_pcsg_dt          The effective date for the batch.
  --      i_new_tx_threshold The % of the io_tx_rec_size beyond which the staging
  --           should create a new transaction.
  --      io_tx_rec_size     Set to OPTIMAL_SET_SIZE initially.
  --                         If the transaction size is different than OPTIMAL_SET_SIZE,
  --                         it has to be returned to the staging module.
  --      o_num_total_recs   Total number of records loaded to
  --                         the primary staging partition
  --      o_num_txs       Total number of transactions generated.
  --      o_last_tx_size  The size of the last transaction
  --      o_uniform_rec_size_ind The value of 0 indicates the transaction record sizes are not uniform

  -- Exceptions:  application error when input values are invalid
  -- Source:      multiple
  -- Destination:
  -- Visibility:  public
  -- Cardinality:
  -- Depends:     something
  -- NOTES:
  --
  --
  ------------------------------------------------------------------------------
  PROCEDURE p_load_stgg(i_batch_name           IN VARCHAR2,
                        i_run_part_id          IN NUMBER,
                        i_batch_id             IN NUMBER,
                        i_pcsg_dt              IN DATE,
                        i_new_tx_threshold     IN NUMBER,
                        io_tx_rec_size         IN OUT NUMBER,
                        o_num_total_recs       OUT NUMBER,
                        o_num_txs              OUT NUMBER,
                        o_last_tx_size         OUT NUMBER,
                        o_uniform_rec_size_ind OUT NUMBER) IS

  BEGIN
    pkg_log_util.p_set_tx_step_nme('START - Mortgage  Update  the Details Recycle Batch - STAGING PHASE');

    IF (i_batch_id IS NULL OR i_run_part_id IS NULL OR i_pcsg_dt is NULL OR
         io_tx_rec_size IS NULL) THEN
        raise_application_error(pkg_sir_exceptions.gl_ln_intg_rule_batch_ex,
                                'Mortage upd Recycle Validation Batch - p_load_stgg -' ||
                                'invalid data: i_batch_id = ' || i_batch_id ||
                                ', i_run_part_id = ' || i_run_part_id ||
                                ', i_pcsg_dt = ' || i_pcsg_dt ||
                                ', io_tx_rec_size = ' || io_tx_rec_size);

    END IF;

    pkg_log_util.p_fine(mtg_log_ctx,
                           'P_LOAD_STGG procedure begining' ||
                           'with the input parameters' || i_batch_name || ',' ||
                           i_run_part_id || ',' || i_pcsg_dt || ',' ||
                           i_new_tx_threshold || ',' || io_tx_rec_size || ',' ||
                           i_batch_id);

    pkg_log_util.p_fine(mtg_log_ctx,
                       'Completed P_LOAD_STGG procedure ' ||
                       'with the output parameters' || io_tx_rec_size || ',' ||
                       o_num_total_recs || ',' || o_num_txs || ',' ||
                       o_uniform_rec_size_ind);

    pkg_log_util.p_clear_tx_step_nme;

    COMMIT;

  END p_load_stgg;

END pkg_mtg_srv_update_rcycl_stgg;
/