CREATE OR REPLACE PACKAGE pkg_mtg_srv_tx IS
  -- Module Name    : pkg_mtg_srv_tx
  -- Purpose        : This Package is the main wrapper for MTG UPDATE PL/SQL Component.
  -- stream      : Mortgage
  -- substream      : Servicing
  -- File Name       : pkg_mtg_srv_tx.pks

  -- Module : p_batch_execution_tx
  -- Purpose : This package contains procedures and functions supporting transaction processing stage
  -- of MTG UPDATE PL/SQL Component, including p_batch_execution_tx() procedure
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
  -- Batch Controller unhandled.
  -- Unrecoverable exception may be thrown if data corruption is encountered.
  -------------------------------------------------------------------------------
  PROCEDURE p_batch_execution_tx(i_batch_id    IN NUMBER,
                               i_run_part_id IN NUMBER,
                               i_tx_id       IN NUMBER,
                               i_pcsg_dt     IN DATE);
  -------------------------------------------------------------------------
  --Module      p_mtg_srv_loans_locked
  --Purpose     This procedure will lock all the loans retrieved from the staging table identified
  --            by financial asset ids, involved in the given input transaction
  --            (which are not submitted for cancellation into an array).
  --Parameters  i_pcsg_dt - Processing Date
  -------------------------------------------------------------------------
  PROCEDURE p_mtg_srv_loans_locked(i_batch_id    IN NUMBER,
                            i_run_part_id IN NUMBER,
                            i_tx_id       IN NUMBER,
                            o_rec_cnt_ni  OUT PLS_INTEGER);

  -------------------------------------------------------------------------

  -------------------------------------------------------------------------
  --Module      p_mtg_srv_business_validation
  --Purpose            This procedure is for running MTG SRV UPD business rules against all the SRV UPD Transactions.
  --Parameters  i_pcsg_dt - Processing Date
  -------------------------------------------------------------------------
  PROCEDURE p_mtg_srv_business_validation(i_pcsg_dt IN DATE);

  -------------------------------------------------------------------------
  --Module      p_execute_mtg_srv_ben_notification
  --Purpose     This component is responsible for creating BEN Notification for all the loans processed by mortgage  updates.
  --Parameters  i_pcsg_dt - Processing Date
  -------------------------------------------------------------------------
  PROCEDURE p_execute_mtg_srv_ben_notification;

 -------------------------------------------------------------------------------------------
  --Module      f_gen_mtg_srv_tran_actn_id_seq
  --Purpose      Generates sequence values for bank_srvc_TRAN_TRKG_ID on tmp_mtg_srv_acntg_tran table..
  -------------------------------------------------------------------------------------------

  FUNCTION f_gen_mtg_srv_tran_actn_id_seq RETURN NUMBER;

END pkg_mtg_srv_tx;
/
