CREATE OR REPLACE PACKAGE pkg_mtg_srv_upload_update_tx IS

  -- Module Name    : pkg_mtg_srv_upload_update_tx
  -- Purpose        : This Package is the main wrapper for 
  --                  Mortgage  Loan Update Auto Cancellation
  --                  File upload PL/SQL Component.
  -- stream         : Mortgage
  -- substream      : Servicing
  -- File Name      : pkg_mtg_srv_upload_update_tx.pks
  --
  
  --

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
  -- Batch Controller unhandled.
  -- Unrecoverable exception may be thrown if data corruption is encountered.
  -------------------------------------------------------------------------------
  PROCEDURE p_batch_execution_tx(i_batch_id    IN NUMBER,
                               i_run_part_id IN NUMBER,
                               i_tx_id       IN NUMBER,
                               i_pcsg_dt     IN DATE);
  -------------------------------------------------------------------------------------------
  --Module      p_mtg_srv_populate_mtg_srv_tran
  --Purpose     Inserts data into Stagging table from tmp_mtg_srv_tran or updates the changes in
  --            case of resubmission (Only if the status of the existing transactions is not
  --            completed). Update status of the transactions to cancel for all the MTG
  --            transactions from TMP_CNCL_TRAN.
  --Parameter   i_batch_id, i_pcsg_dt
  -------------------------------------------------------------------------------------------

  PROCEDURE p_mtg_srv_populate_mtg_srv_tran(i_batch_id IN NUMBER, i_pcsg_dt IN DATE);

  ---------------------------------------------------------------------------
  --Module      p_mtg_srv_business_validation
  --Purpose     This procedure is for running MTG Del Mod business rules 
  --            against all the Del Mod Transactions.
  -------------------------------------------------------------------------
  PROCEDURE p_mtg_srv_business_validation(i_pcsg_dt IN DATE);

  
END pkg_mtg_srv_upload_update_tx;
/
